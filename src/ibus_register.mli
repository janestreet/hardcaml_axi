open Hardcaml

(** Creates a simple state machine on an Ibus that can be used to register the datapath to
    help with timing.
*)
module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; slave_dn : 'a Slave_to_master.t
      ; master_up : 'a Master_to_slave.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { slave_up : 'a Slave_to_master.t
      ; master_dn : 'a Master_to_slave.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  (** If the Ibus supports write backpressure via [slave_dn.write_ready], the register
      will wait for the slave to assert it. Otherwise just register and pass the write
      from [master_up] through. Exablaze is an example of vendor infrastructure that does
      not support write backpressure. *)
  val hierarchical
    :  ?instance:string
    -> supports_wready:bool
    -> Scope.t
    -> Interface.Create_fn(I)(O).t
end
