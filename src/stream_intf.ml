open Base
open Hardcaml
module Handshake = Hardcaml_handshake
module Pipeline_stage_descr = Build_register_pipeline.Pipeline_stage_descr

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
  val of_untyped : Signal.t Stream_untyped.Source.t -> Signal.t t
  val to_untyped : 'a t -> 'a Stream_untyped.Source.t
end

module type Dest = sig
  type 'a t = { tready : 'a (** High when destination is ready to receive data. *) }
  [@@deriving hardcaml]

  val of_untyped : Signal.t Stream_untyped.Dest.t -> Signal.t t
  val to_untyped : 'a t -> 'a Stream_untyped.Dest.t
end

(** An AXI-Stream instantiation. *)
module type S = sig
  (** AXI4-stream data source ports *)
  module Source : Source

  (** AXI4-stream data acknowledgement ports *)
  module Dest : Dest

  val add_properties
    :  ?clear:Signal.t
    -> ?prefix:string
    -> source:Signal.t Source.t
    -> dest:Signal.t Dest.t
    -> Scope.t
    -> unit

  (** When placed between two components which produce/consume an AXI stream, this module
      ensures that every output signal is registerd. It fully supports the
      [tvalid]/[tready] handshake protocol. *)
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

    val create_io : Signal.Reg_spec.t -> Signal.t IO.t -> Signal.t IO.t
    val create : Scope.t -> Signal.t I.t -> Signal.t IO.t
    val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t IO.t

    module Pipeline_stage_descr = Pipeline_stage_descr

    (** Instantiates a chain of [n] [Datapath_register] components and wire up the
        [source] and [dest] signals appropriately.

        In most cases, you probably want to use [pipeline_simple]. *)
    val pipeline_expert
      :  pipeline_stages:Pipeline_stage_descr.t list
      -> scope:Scope.t
      -> clock:Signal.t
      -> io:Signal.t IO.t
      -> Signal.t IO.t

    (** Constructs a datapath register pipeline with [n] stages, where all the pipeline
        stages have the same clear and same instance name. *)
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
  module Pipeline_stage_descr = Pipeline_stage_descr

  (** Config interface for AXI-Stream instantiations. *)
  module type Config = Config

  (** Data acknowledgement interface for AXI-Stream instantiations. *)
  module type Dest = Dest

  (** Data Source interface for AXI-Stream instantiations. *)
  module type Source = Source

  (** AXI Stream interfaces for instantiations. *)
  module type S = S

  (** Instantiates an AXI Stream interface {!S} from the given config. *)
  module Make (X : Config) : S

  module Source_untyped = Stream_untyped.Source
  module Dest_untyped = Stream_untyped.Dest

  module type S_untyped =
    S with type 'a Source.t = 'a Source_untyped.t and type 'a Dest.t = 'a Dest_untyped.t

  (** Similar to [Make], but use [Source_untyped.t] and [Dest_untyped.t] for its
      [Source.t] and [Dest.t] respectively. This sacrifices type-safety by not creating a
      fresh-type for every invocation of [Make], but can easier to work with in some
      cases, especially use cases with heavily nested functors.

      Users should by default prefer using [Make] rather than [Make_untyped]. *)
  module Make_untyped (X : Config) : S_untyped
end
