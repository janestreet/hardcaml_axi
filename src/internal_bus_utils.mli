open Base
open Hardcaml

module Make
  (Master_to_slave : Internal_bus_ports.Master_to_slave)
  (Slave_to_master : Internal_bus_ports.Slave_to_master) : sig
  val bytes_per_word : int
  val word_address : master:Signal.t Master_to_slave.t -> size:int -> Signal.t

  val create_slave
    :  read_latency:int
    -> write_latency:int
    -> reg_spec:Reg_spec.t
    -> master:Signal.t Master_to_slave.t
    -> read_data:Signal.t
    -> Signal.t Slave_to_master.t
end
