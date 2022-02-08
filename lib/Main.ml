open ListMonad
open InfixOperators
open GraphMaker
open SimilarityHandler
open GraphSplitter
open SpawnPython
open GraphRepr
open Loop
module F = Format

let build_graph (graph_fragment : G.t) : G.t =
  Out_channel.print_endline
  @@ F.asprintf "building nodewise maps for %s..." graph_fragment.comp_unit ;
  let unmarked_vertices = G.get_unmarked_vertices graph_fragment in
  let this_fragment_unmarked_apis = G.get_unmarked_apis graph_fragment
  and this_fragment_unmarked_udfs = G.get_unmarked_udfs graph_fragment in
  let open NodeWiseFeatures.NodeWiseFeatureMap in
  let this_fragment_unmarked_apis_featuremap, this_fragment_unmarked_udfs_featuremap =
    init_for_graph graph_fragment
  in
  (* ===== prepare data for python processes ===== *)
  CSVSerializer.serialize this_fragment_unmarked_apis_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" graph_fragment.comp_unit) ;
  CSVSerializer.serialize this_fragment_unmarked_udfs_featuremap
    ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" graph_fragment.comp_unit) ;
  Trunk.Serializer.serialize_graph_trunks_to_json graph_fragment ;
  (* ======================================== *)
  let finished_graph =
    graph_fragment |> SimilarityHandler.make_nodewise_sim_edge
    |> SimilarityHandler.make_contextual_sim_edge
  in
  finished_graph


let one_pass (graph_fragment : G.t) : unit =
  let finished_graph = build_graph graph_fragment in
  Visualizer.visualize_snapshot finished_graph ~autoopen:false ~micro:false ;
  ignore @@ loop graph_fragment [] NodeWiseFeatures.NodeWiseFeatureMap.empty


let main () =
  Out_channel.print_endline @@ F.asprintf "Current directory is %s" @@ Sys.getcwd () ;
  let json = Deserializer.deserialize_json () in
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  Out_channel.print_endline "df_graph initialized." ;
  RedefineHandler.make_and_output_redefine_dict json ;
  let axiom_applied = Axioms.apply_axioms df_edges_added in
  let splitted = split_graph_by_comp_unit axiom_applied in
  List.iter ~f:one_pass splitted
