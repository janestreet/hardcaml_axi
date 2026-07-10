open Base
open Hardcaml
include Register_bank_intf
module Packed_array = Hardcaml_packed_array.Packed_array

module Make_read_enable (X : Interface.S) = struct
  module T = struct
    include X

    let port_names_and_widths = map port_names ~f:(fun n -> "ren$" ^ n, 1)
  end

  include T
  include Interface.Make (T)
end

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  open Signal
  module Master_to_slave = Master_to_slave
  module Slave_to_master = Slave_to_master

  module Without_interface = struct
    type result =
      { write_values : Signal.t With_valid.t list
      ; read_enables : Signal.t list
      }
    [@@deriving sexp_of]

    type t = (Signal.t Slave_to_master.t, result) Slave_with_data.t

    include Internal_bus_utils.Make (Master_to_slave) (Slave_to_master)

    let tree_mux ~cycles ~reg selector data =
      if cycles = 0
      then mux selector data
      else
        Hardcaml_circuits.Pipelined_tree_mux.pipelined_tree_mux
          ~cycles
          ~reg
          ~selector
          data
    ;;

    let create
      ?(pipelined_read_depth =
        { Pipelined_read_depth.external_cycles = 0; internal_mux_cycles = 0 })
      reg_spec
      ~clear_write_values
      ~(master : _ Master_to_slave.t)
      ~write_modes
      ~read_values
      =
      List.iter read_values ~f:(fun read_value ->
        assert (width read_value <= Master_to_slave.data_bits));
      let total_pipelined_read_depth =
        pipelined_read_depth.external_cycles + pipelined_read_depth.internal_mux_cycles
      in
      let num_read_values = List.length read_values in
      let num_write_values = List.length write_modes in
      let read_addr = word_address ~master ~size:num_read_values in
      let write_addr = word_address ~master ~size:num_write_values in
      let write_values =
        let wa1h = binary_to_onehot write_addr.value in
        List.mapi write_modes ~f:(fun i (mode : Register_mode.t) ->
          let e = master.write_first &: wa1h.:(i) &: write_addr.valid in
          let d = master.write_data in
          let e, d =
            match Register_mode.mode mode with
            | Toggle_low -> vdd, mux2 e d (width d |> zero)
            | Toggle_high -> vdd, mux2 e d (width d |> ones)
            | Hold -> e, d
          in
          (* internal clear, if required *)
          let clear =
            if Register_mode.internal_clear mode
            then Some (Reg_spec.clear_exn reg_spec |: clear_write_values)
            else None
          in
          (* default value after clear *)
          let clear_to =
            if Register_mode.clear_to mode <> 0
            then Some (of_int_trunc ~width:(width d) (Register_mode.clear_to mode))
            else None
          in
          { With_valid.valid = reg reg_spec e
          ; value =
              reg reg_spec ?clear ?clear_to ~enable:e d
              |> Fn.flip add_attribute (Rtl_attribute.Vivado.extract_enable false)
          })
      in
      let slave =
        let read_mux =
          match read_values with
          | [] -> zero 32
          | [ x ] -> x
          | _ ->
            let cycles = pipelined_read_depth.internal_mux_cycles in
            mux2
              read_addr.valid
              (tree_mux ~cycles ~reg:(reg reg_spec) read_addr.value read_values)
              (pipeline reg_spec ~n:cycles (ones 32))
        in
        create_slave
          ~read_latency:(total_pipelined_read_depth + 1)
          ~write_latency:1
          ~reg_spec
          ~master
          ~read_data:
            (Signal.reg
               reg_spec
               ~enable:(pipeline reg_spec ~n:total_pipelined_read_depth master.read_first)
               read_mux)
      in
      let read_enables =
        if num_read_values = 0
        then []
        else (
          let read_enables =
            (binary_to_onehot read_addr.value).:[num_read_values - 1, 0]
          in
          read_enables
          &: repeat (master.read_first &: read_addr.valid) ~count:(width read_enables)
          |> bits_lsb)
      in
      { Slave_with_data.slave; data = { write_values; read_enables } }
    ;;
  end

  module With_interface_and_addrs (M : sig
      module Read : Interface.S
      module Write : Interface.S

      val write_addresses : int Write.t
      val read_addresses : int Read.t
    end) =
  struct
    open M
    module Write_with_valid = With_valid.Fields.Make (Write)
    module Read_enable = Make_read_enable (Read)

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; clear_write_values : 'a
        ; master : 'a Master_to_slave.t
        ; read_values : 'a Read.t
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    module O = struct
      type 'a t =
        { slave : 'a Slave_to_master.t
        ; write_values : 'a Write_with_valid.t
        ; read_enable : 'a Read_enable.t
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    let write_addresses = write_addresses
    let read_addresses = read_addresses

    let validate_addresses () =
      let validate addrs =
        let addr_to_name = Hashtbl.create (module Int) in
        List.iter addrs ~f:(fun (name, addr) ->
          if addr < 0 || addr % 4 <> 0
          then
            raise_s
              [%message
                "Register address must be a multiple of 4 and non-negative"
                  (name : string)
                  (addr : int)];
          match Hashtbl.add addr_to_name ~key:addr ~data:name with
          | `Ok -> ()
          | `Duplicate ->
            let existing = Hashtbl.find_exn addr_to_name addr in
            raise_s
              [%message
                "Duplicate register addresses"
                  (addr : int)
                  ~names:([ existing; name ] : string list)])
      in
      Read.zip Read.port_names read_addresses |> Read.to_list |> validate;
      Write.zip Write.port_names write_addresses |> Write.to_list |> validate
    ;;

    (* Sort [items] by their [addresses], and pad with [pad]. The resulting list has one
       entry per word in the address space (covering [0] up to the maximum address).

       Returns the padded list, and a mapping of each register to its index in the padded
       list, so we can remap the output from the register bank back to the original
       interface. *)
    let sort_and_pad ~pad ~addresses ~items =
      let sorted =
        List.zip_exn addresses items
        |> List.mapi ~f:(fun idx (addr, item) -> idx, addr, item)
        |> List.sort ~compare:(fun (_, addr0, _) (_, addr1, _) ->
          Int.ascending addr0 addr1)
      in
      let padded =
        let len =
          match List.last sorted with
          | None -> 0
          | Some (_, max_addr, _) -> (max_addr / 4) + 1
        in
        Array.create ~len pad
      in
      let index_in_padded_list = Array.create ~len:(List.length sorted) 0 in
      List.iter sorted ~f:(fun (original_index, addr, item) ->
        let idx_in_padded_list = addr / 4 in
        padded.(idx_in_padded_list) <- item;
        index_in_padded_list.(original_index) <- idx_in_padded_list);
      Array.to_list padded, index_in_padded_list
    ;;

    let create ?pipelined_read_depth _scope ~write_modes (i : _ I.t) =
      validate_addresses ();
      let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let write_modes = Write.to_list write_modes in
      let read_values =
        Read.zip Read.port_names_and_widths i.read_values
        |> Read.to_list
        |> List.map ~f:(fun ((name, intf_width), s) ->
          if Signal.width s > 32
          then
            raise_s
              [%message
                "register width > 32 bit"
                  (name : string)
                  (Signal.width s : int)
                  (intf_width : int)]
          else Signal.uresize s ~width:32)
      in
      let read_values_padded, read_index_in_padded_list =
        sort_and_pad
          ~pad:(Signal.ones 32)
          ~addresses:(Read.to_list read_addresses)
          ~items:read_values
      in
      let write_modes_padded, write_index_in_padded_list =
        sort_and_pad
          ~pad:Register_mode.hold
          ~addresses:(Write.to_list write_addresses)
          ~items:write_modes
      in
      let { Slave_with_data.slave
          ; data = { Without_interface.write_values; read_enables }
          }
        =
        Without_interface.create
          ?pipelined_read_depth
          reg_spec
          ~master:i.master
          ~write_modes:write_modes_padded
          ~read_values:read_values_padded
          ~clear_write_values:i.clear_write_values
      in
      let write_values =
        let write_values_array = Array.of_list write_values in
        let pick =
          let t =
            Write.to_list Write.port_names
            |> List.mapi ~f:(fun i name ->
              name, write_values_array.(write_index_in_padded_list.(i)))
          in
          fun name -> List.Assoc.find_exn t name ~equal:String.equal
        in
        Write.map Write.port_names_and_widths ~f:(fun (n, width) ->
          if width > 32
          then raise_s [%message "write register width >32b" (n : string) (width : int)];
          let { With_valid.valid; value } = pick n in
          { With_valid.valid; value = value.Signal.:[width - 1, 0] })
      in
      let read_enable =
        let read_enables_array = Array.of_list read_enables in
        let t =
          Read.to_list Read.port_names
          |> List.mapi ~f:(fun i name ->
            name, read_enables_array.(read_index_in_padded_list.(i)))
        in
        Read.map Read.port_names ~f:(fun name ->
          List.Assoc.find_exn t name ~equal:String.equal)
      in
      { O.slave; write_values; read_enable }
    ;;

    let hierarchical ?instance ?pipelined_read_depth scope ~write_modes =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ?instance
        ~scope
        ~name:"register_bank"
        (create ?pipelined_read_depth ~write_modes)
    ;;
  end

  module With_interface (Read : Interface.S) (Write : Interface.S) =
  With_interface_and_addrs (struct
      module Read = Read
      module Write = Write

      let write_addresses =
        Write.(scan port_names ~init:0 ~f:(fun addr _ -> addr + 1, addr * 4))
      ;;

      let read_addresses =
        Read.(scan port_names ~init:0 ~f:(fun addr _ -> addr + 1, addr * 4))
      ;;
    end)

  include Without_interface
end
