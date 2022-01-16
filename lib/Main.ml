open ListMonad
open InfixOperators
open GraphMaker
open SimilarityHandler

exception TODO

let operate_on_single_splitted_graph splitted_graph =
  let ns_edges_added =
    match graph_already_serialized "ns_edges" with
    | None ->
        let result = EstablishSimEdges.make_nodewise_sim_edge splitted_graph in
        G.serialize_to_bin result ~suffix:"ns_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  print_endline "ns_graph initialized." ;
  let cs_edges_added =
    match graph_already_serialized "cs_edges" with
    | None ->
        let result = EstablishSimEdges.make_contextual_sim_edge splitted_graph in
        G.serialize_to_bin result ~suffix:"cs_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  (* Loop.loop *)
  raise TODO


let main () =
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
  let all_splitted_df_graphs = GraphSplitter.split_graph_by_comp_unit df_edges_added in
  List.iter ~f:operate_on_single_splitted_graph all_splitted_df_graphs
