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
  CSVSerializer.serialize this_fragment_unmarked_apis_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" healed.comp_unit) ;
  CSVSerializer.serialize this_fragment_unmarked_udfs_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" healed.comp_unit) ;
  Trunk.Serializer.serialize_graph_trunks_to_json healed ;
  healed |> SimilarityHandler.make_nodewise_sim_edge |> Axioms.apply_axioms


let one_pass (graph_fragment : G.t) : G.t =
  let finished_graph = build_graph graph_fragment in
  let loop_finished = loop finished_graph NodeWiseFeatureMap.empty ~auto_test:true in
  InferenceResult.Serializer.output_result loop_finished ;
  loop_finished


let main () =
  Out_channel.print_endline @@ F.asprintf "Current directory is %s" @@ Sys.getcwd () ;
  let json = Deserializer.deserialize_json () in
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" ~finished:false with
    | None ->
        G.empty |> batch_add_vertex json |> batch_add_edge json
        |> ReturnStmtLocation.repair_vertices_with_incorrect_return_loc
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
