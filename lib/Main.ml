open ListMonad
open InfixOperators
open DataFlowEdges
open SimilarityHandler
open GraphSplitter
open SpawnPython
open GraphRepr
open Loop
open NodeWiseFeatures
open InferenceResult
module F = Format

let build_graph (graph_fragment : G.t) : G.t =
  Out_channel.print_endline
  @@ F.asprintf "building nodewise maps for %s..." graph_fragment.comp_unit ;
  let healed = Repair.reconnect_disconnected_edges graph_fragment in
  let open NodeWiseFeatures.NodeWiseFeatureMap in
  let this_fragment_unmarked_apis_featuremap, this_fragment_unmarked_udfs_featuremap =
    init_for_graph healed
  in
  print_endline "Serializing Nodewise Maps..." ;
  CSVSerializer.serialize this_fragment_unmarked_apis_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" healed.comp_unit) ;
  CSVSerializer.serialize this_fragment_unmarked_udfs_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" healed.comp_unit) ;
  (* Trunk.Serializer.serialize_graph_trunks_to_json healed ; *)
  healed
  |> fun i ->
  print_endline "Making Similarity Edges..." ;
  i |> SimilarityHandler.make_nodewise_sim_edge
  |> fun i ->
  print_endline "Applying Axioms..." ;
  i |> Axioms.apply_axioms


let one_pass (graph_fragment : G.t) : G.t =
  let finished_graph = build_graph graph_fragment in
  let loop_finished = loop finished_graph NodeWiseFeatureMap.empty ~auto_test:true in
  InferenceResult.Serializer.output_result loop_finished ;
  loop_finished


let manual_test_mappings_annot (graph_fragment : G.t) : unit =
  if String.( = ) graph_fragment.comp_unit "sagan-renderer" then
    let renderer_finished = build_graph graph_fragment
    and mappings =
      [ "ResourceSupport IndexController.index()"
      ; "Resources GuidesController.listGuides()"
      ; "ResponseEntity GuidesController.renderGuide(String,String)"
      ; "ResponseEntity GuidesController.showGuide(String,String)"
      ; "ResponseEntity MarkupController.renderMarkup(MediaType,String)" ]
    in
    ManualTest.manual_test_queue_initial renderer_finished mappings


let main () =
  Out_channel.print_endline @@ F.asprintf "Current directory is %s" @@ Sys.getcwd () ;
  let json = Deserializer.deserialize_json () in
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" ~finished:false with
    | None ->
        let out =
          G.empty |> batch_add_vertex json |> batch_add_edge json
          |> ReturnStmtLocation.repair_vertices_with_incorrect_return_loc
        in
        G.serialize_to_bin ~suffix:"df_edges" out ;
        out
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  Out_channel.print_endline "df_graph initialized." ;
  RedefineHandler.make_and_output_redefine_dict json ;
  let splitted = split_graph_by_comp_unit df_edges_added in
  List.fold
    ~f:(fun acc subgraph ->
      let transferred_subgraph = Transfer.transfer_graph acc subgraph in
      let finished = one_pass transferred_subgraph in
      G.merge acc finished )
    splitted ~init:G.empty


(** TEMP OVERRIDE *)
(* let main () = *)
(*   Out_channel.print_endline @@ F.asprintf "Current directory is %s" @@ Sys.getcwd () ; *)
(*   let json = Deserializer.deserialize_json () in *)
(*   let df_edges_added = *)
(*     match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" ~finished:false with *)
(*     | None -> *)
(*       let out = *)
(*         G.empty |> batch_add_vertex json |> batch_add_edge json *)
(*         |> ReturnStmtLocation.repair_vertices_with_incorrect_return_loc *)
(*       in *)
(*       G.serialize_to_bin ~suffix:"df_edges" out ; *)
(*       out *)
(*     | Some filename -> *)
(*       Deserializer.deserialize_graph filename *)
(*   in *)
(*   Out_channel.print_endline "df_graph initialized." ; *)
(*   RedefineHandler.make_and_output_redefine_dict json ; *)
(*   let splitted = split_graph_by_comp_unit df_edges_added in *)
(*   List.iter ~f:manual_test_mappings_annot splitted *)

(** TEMP OVERRIDE *)
(* let main () = *)
(*   let open Visualizer in *)
(*   let open TrunkView in *)
(*   let open Trunk in *)
(*   let json = Deserializer.deserialize_json () in *)
(*   let df_edges_added = *)
(*     match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" ~finished:false with *)
(*     | None -> *)
(*         let result = G.empty |> batch_add_vertex json |> batch_add_edge json in *)
(*         G.serialize_to_bin result ~suffix:"df_edges" ; *)
(*         result *)
(*     | Some filename -> *)
(*         Deserializer.deserialize_graph filename *)
(*   in *)
(*   let splitted = split_graph_by_comp_unit df_edges_added in *)
(*   let renderer_graph = List.nth_exn splitted 0 in *)
(*   let renderer_trunkview = renderer_graph |> identify_longest_trunks |> make_trunkview in *)
(*   let out_chan = Out_channel.create "renderer_graph_trunks.dot" in *)
(*   Visualizer.TrunkViewDot.output_graph out_chan renderer_trunkview ; *)
(*   Out_channel.close out_chan *)
