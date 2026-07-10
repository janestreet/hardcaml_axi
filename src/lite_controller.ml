open Core
open Hardcaml
open Signal

module Make (X : sig
    include Master_slave_bus_config.S

    val enable_address_hi : bool
  end) =
struct
  let lite_addr_bits = if X.enable_address_hi then X.addr_bits * 2 else X.addr_bits

  module Lite = Lite.Make (struct
      let data_bits = X.data_bits
      let addr_bits = lite_addr_bits
    end)

  module Ibus = Internal_bus.Make (X)

  let () =
    (* This simplifies the registers and byte offsets, since ADDRESS can be written with a
       single regsiter write. *)
    if X.addr_bits <> X.data_bits
    then
      Error.raise_s
        [%message
          "Address bits and data bits must be equal"
            (X.addr_bits : int)
            (X.data_bits : int)]
  ;;

  let bits_in_byte = 8

  module Registers = struct
    type t =
      | ADDRESS_LO
      | DATA
      | INCREMENTING
      | LOCK
      | ADDRESS_HI
    [@@deriving enumerate, sexp_of, variants]

    let offset = Variants.to_rank
    let byte_addr t = X.data_bits / bits_in_byte * offset t
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; m_ibus : 'a Ibus.Master_to_slave.t [@rtlprefix "m_ibus$"]
      ; s_axi : 'a Lite.Slave_to_master.t [@rtlprefix "s_axi$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { s_ibus : 'a Ibus.Slave_to_master.t [@rtlprefix "s_ibus$"]
      ; m_axi : 'a Lite.Master_to_slave.t [@rtlprefix "m_axi$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module State = struct
    type t =
      | Idle
      | Reading
      | Writing
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  let create scope { I.clock; clear; m_ibus; s_axi } : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in
    let int_master = Lite.Internal_bus.Master_to_slave.Of_always.wire zero in
    let int_slave = Lite.Internal_bus.Slave_to_master.Of_always.wire zero in
    let axi_sm =
      Lite.Master_statemachine.hierarchical
        scope
        { Lite.Master_statemachine.I.clock
        ; clear
        ; axi_slave = s_axi
        ; int_master = Lite.Internal_bus.Master_to_slave.Of_always.value int_master
        }
    in
    let sm = Always.State_machine.create (module State) spec in
    let address_lo = Always.Variable.reg ~width:X.addr_bits spec in
    let address_hi = Always.Variable.reg ~width:X.addr_bits spec in
    let incrementing = Always.Variable.reg ~width:1 spec in
    let first = Always.Variable.reg ~width:1 spec in
    let%hw_var lock = Always.Variable.reg ~width:1 spec in
    Always.(
      let incr_address =
        let next_address =
          (address_hi.value @: address_lo.value) +:. (X.data_bits / bits_in_byte)
        in
        proc
          ([ address_lo <-- sel_bottom ~width:X.addr_bits next_address ]
           @
           if X.enable_address_hi
           then [ address_hi <-- sel_top ~width:X.addr_bits next_address ]
           else [])
      in
      compile
        [ (int_master.address
           <--
           if X.enable_address_hi
           then address_hi.value @: address_lo.value
           else address_lo.value)
        ; int_master.write_byte_en <-- m_ibus.write_byte_en
        ; int_master.write_data <-- m_ibus.write_data
        ; sm.switch
            [ ( Idle
              , [ first <-- vdd
                ; when_
                    (m_ibus.write_valid &: m_ibus.write_first)
                    [ when_
                        (m_ibus.address ==:. Registers.byte_addr ADDRESS_LO)
                        [ int_slave.write_ready <-- vdd
                        ; address_lo <-- m_ibus.write_data
                        ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr ADDRESS_HI)
                        [ int_slave.write_ready <-- vdd
                        ; address_hi <-- m_ibus.write_data
                        ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr DATA)
                        [ sm.set_next Writing ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr INCREMENTING)
                        [ int_slave.write_ready <-- vdd
                        ; incrementing <-- lsb m_ibus.write_data
                        ]
                    ; when_
                        (m_ibus.address
                         ==:. Registers.byte_addr LOCK
                         &: ~:(lsb m_ibus.write_data))
                        [ int_slave.write_ready <-- vdd; lock <-- gnd ]
                    ]
                ; when_
                    (m_ibus.read_valid &: m_ibus.read_first)
                    [ when_
                        (m_ibus.address ==:. Registers.byte_addr ADDRESS_LO)
                        [ int_slave.read_ready <-- vdd
                        ; int_slave.read_data <-- address_lo.value
                        ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr ADDRESS_HI)
                        [ int_slave.read_ready <-- vdd
                        ; int_slave.read_data <-- address_hi.value
                        ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr DATA)
                        [ sm.set_next Reading ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr INCREMENTING)
                        [ int_slave.read_ready <-- vdd
                        ; int_slave.read_data
                          <-- uresize incrementing.value ~width:X.data_bits
                        ]
                    ; when_
                        (m_ibus.address ==:. Registers.byte_addr LOCK)
                        [ int_slave.read_ready <-- vdd
                        ; int_slave.read_data <-- uresize lock.value ~width:X.data_bits
                        ; lock <-- vdd
                        ]
                    ]
                ] )
            ; ( Reading
              , [ Lite.Internal_bus.Slave_to_master.Of_always.assign
                    int_slave
                    axi_sm.int_slave
                ; int_master.read_valid <-- vdd
                ; int_master.read_first <-- first.value
                ; first <-- gnd
                ; when_
                    axi_sm.int_slave.read_ready
                    [ sm.set_next Idle; when_ incrementing.value [ incr_address ] ]
                ] )
            ; ( Writing
              , [ Lite.Internal_bus.Slave_to_master.Of_always.assign
                    int_slave
                    axi_sm.int_slave
                ; int_master.write_valid <-- vdd
                ; int_master.write_first <-- first.value
                ; first <-- gnd
                ; when_
                    axi_sm.int_slave.write_ready
                    [ sm.set_next Idle; when_ incrementing.value [ incr_address ] ]
                ] )
            ]
        ]);
    { O.s_ibus =
        (* Coerce the AXI-Lite state machine ibus slave into the upstream ibus slave. *)
        { write_ready = int_slave.write_ready.value
        ; read_ready = int_slave.read_ready.value
        ; read_data = int_slave.read_data.value
        }
    ; m_axi = axi_sm.axi_master
    }
  ;;

  let hierarchical scope (i : _ I.t) : _ O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"lite_controller" create i
  ;;
end
