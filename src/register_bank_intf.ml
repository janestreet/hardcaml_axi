open Base
open Hardcaml

(** A bank of read/write registers connected to a Master interface.

    The register bank connects a set of IO ports in a user design to an [Internal_bus]
    master (which is, generally, a CPU). The master device writes values over the bus and
    these appear on the [write_values] list for presentation to the user design. The
    [read_values] list is provided by the user design and is read by the master. *)
module type S = sig
  module Master_to_slave : Internal_bus_ports.Master_to_slave
  module Slave_to_master : Internal_bus_ports.Slave_to_master

  type result =
    { write_values : Signal.t With_valid.t list
    ; read_enables : Signal.t list
    }
  [@@deriving sexp_of]

  type t = (Signal.t Slave_to_master.t, result) Slave_with_data.t

  type pipelined_read_depth =
    { external_cycles : int
    ; internal_mux_cycles : int
    }

  (** Creates a register bank.

      {[
        let { slave; write_values } = create ~reg_spec ~master ~write_modes ~read_values
      ]}

      [write_modes] specifies what behavior the corresponding [write_value] should have:
      hold the value written, or toggle back to some known value after 1 cycle.

      [write_values] can be connected to [read_values] to create a read/write register.

      Writes ignore [master.write_byte_en], therefore only aligned 32 bit transfers are
      fully supported.

      [clear_write_values] clears write registers whose mode is configured with mode
      [internal_clear = true]. *)

  val create
    :  ?pipelined_read_depth:
         pipelined_read_depth
         (* Default is zero internal and external cycles of latency *)
    -> Signal.Reg_spec.t
    -> clear_write_values:Signal.t
    -> master:Signal.t Master_to_slave.t
    -> write_modes:Register_mode.t list
    -> read_values:Signal.t list
    -> t

  module With_interface (Read : Interface.S) (Write : Interface.S) : sig
    module Write_with_valid : Interface.S with type 'a t = 'a With_valid.t Write.t
    module Read_enable : Interface.S with type 'a t = 'a Read.t

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; clear_write_values : 'a
        ; master : 'a Master_to_slave.t
        ; read_values : 'a Read.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { slave : 'a Slave_to_master.t
        ; write_values : 'a Write_with_valid.t
        ; read_enable : 'a Read_enable.t
        (** [read_enable] is a single bit for each field in the register interface that
            toggles high for one cycle when that register is accessed. For multi-part
            registers, this allows the designer to implement any desired synchronisation
            between the individual fields. *)
        }
      [@@deriving hardcaml]
    end

    val write_addresses : int Write.t
    val read_addresses : int Read.t

    val create
      :  ?pipelined_read_depth:pipelined_read_depth
      -> Scope.t
      -> write_modes:Register_mode.t Write.t
      -> Interface.Create_fn(I)(O).t

    val hierarchical
      :  ?instance:string
      -> ?pipelined_read_depth:pipelined_read_depth
      -> Scope.t
      -> write_modes:Register_mode.t Write.t
      -> Interface.Create_fn(I)(O).t
  end
end

(** Packed arrays are a flattened version of [X.t] represented as an array of 32 bit
    vectors.

    They may be used as fields within a register interface to encode larger or grouped
    values. *)
module Packed_array = struct
  module M (X : Interface.S) = struct
    module type S = sig
      include Interface.S with type 'a t = 'a array

      val to_packed_array : (module Comb.S with type t = 'a) -> 'a X.t -> 'a t

      val to_packed_array_latch_on_read
        :  read_latency:int
        -> Signal.Reg_spec.t
        -> Signal.t X.t
        -> Signal.t t
        -> Signal.t t

      val of_packed_array : (module Comb.S with type t = 'a) -> 'a t -> 'a X.t

      val of_packed_array_with_valid
        :  (module Comb.S with type t = 'a)
        -> 'a With_valid.t t
        -> 'a With_valid.t X.t

      (* Extract fields *)
      val extract_field_as_int : (int t -> int) X.t
      val extract_field_as_int64 : (int t -> int64) X.t
      val extract_field_as_bytes : (int t -> Bytes.t -> unit) X.t
      val extract_field_as_string : (int t -> String.t) X.t

      (* Set fields *)
      val set_field_as_int : (int t -> int -> unit) X.t
      val set_field_as_int64 : (int t -> int64 -> unit) X.t
      val set_field_as_bytes : (int t -> Bytes.t -> unit) X.t
      val set_field_as_string : (int t -> String.t -> unit) X.t
      val hold : Register_mode.t t

      (* Specialized conversions for ints *)
      val of_packed_int_array : int t -> int X.t
      val to_packed_int_array : int X.t -> int t
    end
  end
end

module type Register_bank = sig
  module type S = S

  module Packed_array : sig
    module M = Packed_array.M

    module Make (X : sig
        include Interface.S

        val name : string
      end) : M(X).S
  end

  module Make
      (Master_to_slave : Internal_bus_ports.Master_to_slave)
      (Slave_to_master : Internal_bus_ports.Slave_to_master) :
    S
    with module Master_to_slave := Master_to_slave
     and module Slave_to_master := Slave_to_master
end
