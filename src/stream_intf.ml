open Base
open Hardcaml
module Handshake = Hardcaml_handshake

module type Config = sig
  val data_bits : int
  val user_bits : int
end

module type Source = sig
  type 'a t =
    { tvalid : 'a (** High when data is available. *)
    ; tdata : 'a (** Data of width [data_bits]. *)
    ; tkeep : 'a (** Packet remainder signalling - valid bytes in last word. *)
    ; tstrb : 'a (** Valid data bytes per word (not generally used). *)
    ; tlast : 'a (** High on last word in packet. *)
    ; tuser : 'a (** User specified signalling. *)
    }
  [@@deriving hardcaml, compare]

  val get_valid : Signal.t t -> Signal.t
  val set_valid : Signal.t t -> valid:Signal.t -> Signal.t t
end

module type Dest = sig
  type 'a t = { tready : 'a (** High when destination is ready to receive data. *) }
  [@@deriving hardcaml]
end

(** An AXI-Stream instantiation. *)
module type S = sig
  (** AXI4-stream data source ports *)
  module Source : sig
    include Source (** @inline *)
  end

  (** AXI4-stream data acknowledgement ports *)
  module Dest : sig
    include Dest (** @inline *)
  end

  val add_properties
    :  ?clear:Signal.t
    -> ?prefix:string
    -> source:Signal.t Source.t
    -> dest:Signal.t Dest.t
    -> Scope.t
    -> unit

  (** When placed between two components which produce/consume an AXI stream, this
      module ensures that every output signal is registerd. It fully supports the
      [tvalid]/[tready] handshake protocol.*)
  module Datapath_register : sig
    module IO : sig
      type 'a t =
        { source : 'a Source.t
        ; dest : 'a Dest.t
        }
      [@@deriving hardcaml]
    end

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; i : 'a IO.t
        }
      [@@deriving hardcaml]
    end

    val create_io : Reg_spec.t -> Signal.t IO.t -> Signal.t IO.t
    val create : Scope.t -> Signal.t I.t -> Signal.t IO.t
    val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t IO.t

    module Pipeline_stage_descr : sig
      type t =
        { instance_name : string option
        ; clear : Signal.t
        }
    end

    (** Instantiates a chain of [n] [Datapath_register] components and wire up the
        [source] and [dest] signals appropriately.

        In most cases, you probably want to use [pipeline_simple].
    *)
    val pipeline_expert
      :  pipeline_stages:Pipeline_stage_descr.t list
      -> scope:Scope.t
      -> clock:Signal.t
      -> io:Signal.t IO.t
      -> Signal.t IO.t

    (** Construacts a datapath register pipeline with [n] stages, where all the pipeline
        stages have the same clear and same instance name.
    *)
    val pipeline_simple
      :  ?instance_name:string
      -> n:int
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t IO.t

    val handshake_simple
      :  ?instance_name:string
      -> n:int
      -> clock:Signal.t
      -> clear:Signal.t
      -> Scope.t
      -> (Signal.t Source.t, Signal.t Source.t) Handshake.t
  end
end

(** AXI4-stream source and destination interface. *)
module type Stream = sig
  (** Config interface for AXI-Stream instantiations.  *)
  module type Config = Config

  (** Data acknowledgement interface for AXI-Stream instantiations.  *)
  module type Dest = Dest

  (** Data Source interface for AXI-Stream instantiations.  *)
  module type Source = Source

  (** AXI Stream interfaces for instantiations.  *)
  module type S = S

  (** Instantiates an AXI Stream interface {!S} from the given config. *)
  module Make (X : Config) : S
end
