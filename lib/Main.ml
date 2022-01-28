open ListMonad
open InfixOperators
open GraphMaker
open SimilarityHandler
open GraphSplitter
open SpawnPython
open GraphRepr
module F = Format

exception TODO

let main () =
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
  let axiom_applied = Axioms.apply_axioms df_edges_added in
  (* split, mark and serialize *)
  let splitted = split_graph_by_comp_unit axiom_applied in
  List.iter
    ~f:(fun graph_fragment ->
      Out_channel.print_endline
      @@ F.asprintf "building nodewise maps for %s..." graph_fragment.comp_unit ;
      let unmarked_vertices = G.get_unmarked_vertices graph_fragment in
      let this_fragment_unmarked_apis = G.get_unmarked_apis graph_fragment
      and this_fragment_unmarked_udfs = G.get_unmarked_udfs graph_fragment in
      let open NodeWiseFeatures.NodeWiseFeatureMap in
      let this_fragment_unmarked_apis_featuremap, this_fragment_unmarked_udfs_featuremap =
        init_for_graph graph_fragment
      in
      CSVSerializer.serialize this_fragment_unmarked_apis_featuremap
        ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" graph_fragment.comp_unit) ;
      CSVSerializer.serialize this_fragment_unmarked_udfs_featuremap
        ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" graph_fragment.comp_unit) ;
      (* ======================================== *)
      Out_channel.print_endline "spawning python process compute_nodewise_similarity.py..." ;
      SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py" ~args:[] ;
      Out_channel.print_string "done" ;
      Out_channel.flush stdout ;
      (* ======================================== *)
      Out_channel.print_endline "spawning python process compute_contextual_similarity.py..." ;
      SpawnPython.spawn_python ~pyfile:"./lib/python/compute_contextual_similarity.py" ~args:[] ;
      Out_channel.print_string "done" ;
      Out_channel.flush stdout )
    splitted ;
  (* TODO: Loop.loop *)
  ()
