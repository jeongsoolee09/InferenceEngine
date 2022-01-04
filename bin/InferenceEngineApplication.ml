open GraphMaker

exception TODO

let main () =
  (* let graph = Deserializer.deserialize_json () |> GraphMaker.init_graph ~debug:true in *)
  let json = Deserializer.deserialize_json () in
  let df_graph =
    match graph_already_serialized "df_edges" with
    | None ->
      let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
      G.serialize_to_bin result ~suffix:"df_edges" ;
      result
    | Some filename ->
      Deserializer.deserialize_graph filename in
  print_endline "df_graph initialized.";
  let renderer_graph = GraphMaker.get_subgraph_for_comp_unit df_graph "renderer" in
  (* let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph in *)
  (* let interaction_completed = Loop.loop graph [] nodewise_featuremap 1 in *)
  print_endline "hihi"


let _ = main ()
