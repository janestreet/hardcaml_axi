open Base
open Hardcaml
include Stream_intf

module Make (X : Config) = struct
  module Dest = struct
    type 'a t = { tready : 'a } [@@deriving hardcaml]
  end

  module Source = struct
    type 'a t =
      { tvalid : 'a
      ; tdata : 'a [@bits X.data_bits]
      ; tkeep : 'a [@bits X.data_bits / 8]
      ; tstrb : 'a [@bits X.data_bits / 8]
      ; tlast : 'a
      ; tuser : 'a [@bits X.user_bits]
      }
    [@@deriving hardcaml, compare]

    let get_valid (t : Signal.t t) = t.tvalid
    let set_valid t ~valid:tvalid = { t with tvalid }
  end

  let add_properties
    ?(clear = Signal.gnd)
    ?(prefix = "")
    ~(source : _ Source.t)
    ~(dest : _ Dest.t)
    scope
    =
    if Scope.trace_properties scope
    then (
      let open Signal in
      let open Property.LTL in
      let make_ap name signal = Scope.make_ltl_ap scope (prefix ^ name) signal in
      let ap_tvalid = make_ap "tvalid" source.tvalid in
      let ap_tready = make_ap "tready" dest.tready in
      let ap_clear = make_ap "tclear" clear in
      let ap_tlast = ap_tvalid &: make_ap "tlast" source.tlast in
      let make_bits_aps name signal =
        List.range 0 (width signal)
        |> List.map ~f:(fun n -> make_ap (name ^ Int.to_string n) (bit signal n))
      in
      let aps_tdata = make_bits_aps "tdata" source.tdata in
      let aps_tkeep = make_bits_aps "tkeep" source.tkeep in
      let aps_tstrb = make_bits_aps "tstrb" source.tstrb in
      Scope.add_ltl_property
        scope
        "tvalid until tready"
        (g (ap_tvalid ==>: r ap_tready ap_tvalid));
      Scope.add_ltl_property
        scope
        "tlast remains until ready"
        (g (ap_tlast ==>: r ap_tready ap_tlast));
      (* Generates one LTL property per bit to ensure that bit doesn't change when tvalid is
         high until after we see a tready. Can't be just a single LTL property as that would
         require 2^number_of_bits states *)
      List.concat [ [ ap_tlast ]; aps_tdata; aps_tkeep; aps_tstrb ]
      |> List.iter ~f:(fun ap ->
           let prop =
             g (ap_tvalid &: ap ==>: (r ap_tready ap |: u ap ap_clear))
             &: g (ap_tvalid &: ~:ap ==>: (r ap_tready ~:ap |: u ~:ap ap_clear))
           in
           let signal_name signal = List.hd_exn (Signal.names signal) in
           let ap_name = Property.LTL.to_string ~name:signal_name ap in
           Scope.add_ltl_property
             scope
             [%string "tvalid waits for tready without %{ap_name} changing"]
             prop))
  ;;

  module Datapath_register = struct
    module IO = struct
      type 'a t =
        { source : 'a Source.t
        ; dest : 'a Dest.t
        }
      [@@deriving hardcaml]
    end

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; i : 'a IO.t [@rtlprefix "i_"]
        }
      [@@deriving hardcaml]
    end

    let create_io spec (i : _ IO.t) =
      let open Signal in
      let reg = reg spec in
      let wire0 () = Always.Variable.wire ~default:gnd in
      let output_axis_tready = i.dest.tready in
      let temp_axis_tvalid_reg = wire 1 in
      let output_axis_tvalid_reg = wire 1 in
      let output_axis_tvalid_next = wire0 () in
      let temp_axis_tvalid_next = wire0 () in
      let store_axis_input_to_output = wire0 () in
      let store_axis_input_to_temp = wire0 () in
      let store_axis_temp_to_output = wire0 () in
      let input_axis_tready_early =
        output_axis_tready
        |: (~:temp_axis_tvalid_reg &: (~:output_axis_tvalid_reg |: ~:(i.source.tvalid)))
      in
      let input_axis_tready_reg = reg ~enable:vdd input_axis_tready_early in
      output_axis_tvalid_reg <== reg ~enable:vdd output_axis_tvalid_next.value;
      temp_axis_tvalid_reg <== reg ~enable:vdd temp_axis_tvalid_next.value;
      Always.(
        compile
          [ (* transfer sink ready state to source *)
            output_axis_tvalid_next <-- output_axis_tvalid_reg
          ; temp_axis_tvalid_next <-- temp_axis_tvalid_reg
          ; store_axis_input_to_output <--. 0
          ; store_axis_input_to_temp <--. 0
          ; store_axis_temp_to_output <--. 0
          ; if_
              input_axis_tready_reg
              [ (* input is ready *)
                if_
                  (output_axis_tready |: ~:output_axis_tvalid_reg)
                  [ (* output is ready or currently not valid, transfer data to output *)
                    output_axis_tvalid_next <-- i.source.tvalid
                  ; store_axis_input_to_output <--. 1
                  ]
                  [ (* output is not ready, store input in temp *)
                    temp_axis_tvalid_next <-- i.source.tvalid
                  ; store_axis_input_to_temp <--. 1
                  ]
              ]
            @@ elif
                 output_axis_tready
                 [ (* input is not ready, but output is ready *)
                   output_axis_tvalid_next <-- temp_axis_tvalid_reg
                 ; temp_axis_tvalid_next <--. 0
                 ; store_axis_temp_to_output <--. 1
                 ]
                 []
          ]);
      let temp = Source.map i.source ~f:(reg ~enable:store_axis_input_to_temp.value) in
      let output =
        Source.map
          (Source.Of_signal.mux2 store_axis_input_to_output.value i.source temp)
          ~f:
            (reg
               ~enable:
                 (store_axis_input_to_output.value |: store_axis_temp_to_output.value))
      in
      { IO.source = { output with tvalid = output_axis_tvalid_reg }
      ; dest = { tready = input_axis_tready_reg }
      }
    ;;

    let create _scope (i : _ I.t) =
      let spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
      create_io spec i.i
    ;;

    let hierarchical ?instance scope i =
      let module Scoped = Hierarchy.In_scope (I) (IO) in
      Scoped.hierarchical
        ~scope
        ~name:("axi_datapath_reg_" ^ Int.to_string X.data_bits)
        ?instance
        create
        i
    ;;

    module Pipeline_stage = struct
      type t =
        { src_dn : Signal.t Source.t
        ; dst_up : Signal.t Dest.t
        ; dst_dn : Signal.t Dest.t
        }
    end

    module Pipeline_stage_descr = struct
      type t =
        { instance_name : string option
        ; clear : Signal.t
        }
    end

    let build_datapath_reg_pipeline ~scope ~clock =
      let rec loop ~source ~pipeline_stages =
        match (pipeline_stages : Pipeline_stage_descr.t list) with
        | [] -> []
        | hd :: tl ->
          let this_stage =
            let tready = Signal.wire 1 in
            let component =
              hierarchical
                ?instance:hd.instance_name
                scope
                { clock; clear = hd.clear; i = { source; dest = { tready } } }
            in
            { Pipeline_stage.src_dn = component.source
            ; dst_up = component.dest
            ; dst_dn = { tready }
            }
          in
          let remaining_stages = loop ~source:this_stage.src_dn ~pipeline_stages:tl in
          this_stage :: remaining_stages
      in
      fun ~source ~pipeline_stages -> loop ~source ~pipeline_stages
    ;;

    let pipeline_expert
      ~(pipeline_stages : Pipeline_stage_descr.t list)
      ~scope
      ~clock
      ~(io : _ IO.t)
      =
      let pipeline =
        build_datapath_reg_pipeline ~scope ~pipeline_stages ~clock ~source:io.source
      in
      ignore
        (List.fold_right pipeline ~init:io.dest ~f:(fun pipeline_stage axi_dest ->
           Signal.( <== ) pipeline_stage.dst_dn.tready axi_dest.Dest.tready;
           pipeline_stage.dst_up)
          : Signal.t Dest.t);
      match pipeline_stages with
      | [] -> io
      | _ ->
        { IO.source = (List.last_exn pipeline).src_dn
        ; dest = (List.hd_exn pipeline).dst_up
        }
    ;;

    let pipeline_simple ?instance_name ~n scope (i : _ I.t) =
      let pipeline_stages =
        List.init n ~f:(Fn.const { Pipeline_stage_descr.instance_name; clear = i.clear })
      in
      pipeline_expert ~pipeline_stages ~scope ~clock:i.clock ~io:i.i
    ;;

    let handshake_simple ?instance_name ~n ~clock ~clear scope =
      Handshake.component (fun (io : _ Hardcaml_handshake.IO.t) ->
        let i =
          { I.clock; clear; i = { source = io.data; dest = { tready = io.ack } } }
        in
        let o = pipeline_simple ?instance_name ~n scope i in
        { Hardcaml_handshake.IO.data = o.source; ack = o.dest.tready })
    ;;
  end
end
