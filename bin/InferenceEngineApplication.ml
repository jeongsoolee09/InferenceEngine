open GraphMaker
open ListMonad
open InfixOperators

exception TODO

let main () =
  (* let graph = Deserializer.deserialize_json () |> GraphMaker.init_graph ~debug:true in *)
  let json = Deserializer.deserialize_json () in
  let df_edges_added =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  print_endline "df_graph initialized." ;
  (* let renderer_graph = GraphMaker.get_subgraph_for_comp_unit df_graph "renderer" in *)
  (* let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph in *)
  (* let interaction_completed = Loop.loop graph [] nodewise_featuremap 1 in *)
  let splitted = GraphSplitter.split_graph_by_comp_unit df_edges_added in
  let renderer_graph = List.nth_exn splitted 0 in
  let renderer_vertices = G.all_vertices_of_graph renderer_graph in
  let site_graph = List.nth_exn splitted 1 in
  let site_vertices = G.all_vertices_of_graph site_graph in
  let renderer_methods = G.all_methods_of_graph renderer_graph in
  let map_array =
    SimilarityHandler.NodeWiseSimilarityMap.make_empty renderer_methods
    |> fun map ->
    SimilarityHandler.NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
    |> List.filter ~f:(fun (m1, m2) ->
           (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
    |> Array.of_list
  in
  Array.map
    ~f:SimilarityHandler.SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity
    map_array


let _ = main ()
