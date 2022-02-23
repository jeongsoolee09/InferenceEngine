open ListMonad
open InfixOperators
open DataFlowEdges
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
    graph_fragment |> SimilarityHandler.make_contextual_sim_edge
    |> SimilarityHandler.make_nodewise_sim_edge
  in
  Visualizer.visualize_snapshot finished_graph ~autoopen:false ~micro:false ;
  finished_graph


let one_pass (graph_fragment : G.t) : unit =
  let healed = Repair.reconnect_disconnected_edges graph_fragment in
  let finished_graph = build_graph healed in
  Visualizer.visualize_snapshot finished_graph ~autoopen:false ~micro:false ;
  ignore @@ loop healed NodeWiseFeatures.NodeWiseFeatureMap.empty


let main () =
  Out_channel.print_endline @@ F.asprintf "Current directory is %s" @@ Sys.getcwd () ;
  let json = Deserializer.deserialize_json () in
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result =
          G.empty |> batch_add_vertex json |> batch_add_edge json
          |> fun i ->
          G.serialize_to_bin i ~suffix:"df_edges" ;
          i |> ReturnStmtLocation.repair_vertices_with_incorrect_return_loc
        in
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  Out_channel.print_endline "df_graph initialized." ;
  RedefineHandler.make_and_output_redefine_dict json ;
  let axiom_applied = Axioms.apply_axioms df_edges_added in
  let splitted = split_graph_by_comp_unit axiom_applied in
  List.iter ~f:one_pass splitted

  (* let vertex1 = ("Repository GithubClient.fetchOrgRepository(String,String)", "{ line 112 }", ProbQuadruple.initial) in *)

  (* let vertex2 = ("Object RestTemplate.getForObject(String,Class,Object[])", "{ line 113 }", ProbQuadruple.initial) in *)

  (* let vertex3 = ("Repository GithubClient.fetchOrgRepository(String,String)", "{ line 113 }", ProbQuadruple.initial) in *)

  (* let vertex4 = ("ResponseEntity GuidesController.showGuide(String,String)", "{ line 73 }", ProbQuadruple.initial) in *)

  (* let vertex5 = ("GuideResource GuideResourceAssembler.toResource(Repository)", "{ line 75 }", ProbQuadruple.initial) in *)

  (* let edges = [(vertex1, vertex2); (vertex2, vertex3); (vertex1, vertex4); (vertex4, vertex5)] in *)

  (* let open ReturnStmtLocation in *)


  (* let test = fun vertex -> *)
  (*   let methname, locset, dist = *)
  (*     (Vertex.get_method vertex, Vertex.get_loc vertex, Vertex.get_dist vertex) *)
  (*   in *)
  (*   try *)
  (*     if *)
  (*       String.equal (Method.get_return_type methname) "void" *)
  (*       || (not @@ is_return_stmt_location_vertex methname locset) *)
  (*       || (not @@ Method.is_udf methname) *)
  (*     then vertex *)
  (*     else *)
  (*       let locset_repaired = *)
  (*         LocationSet.to_int_list locset *)
  (*         |> List.map ~f:(rectify_return_stmt_location (Method.get_declaration_file methname)) *)
  (*         |> LocationSet.of_int_list *)
  (*       in *)
  (*       Vertex.make methname locset_repaired dist *)
  (*   with Not_found_s _ -> vertex in *)

  (* let repaired_edges = List.map ~f:(fun (v1, v2) -> *)
  (*     let v1_repaired = ReturnStmtLocation.rectify_return_stmt_location_vertex v1 *)
  (*     and v2_repaired = ReturnStmtLocation.rectify_return_stmt_location_vertex v2 in *)
  (*     (v1_repaired, v2_repaired) *)
  (*   ) edges in *)

  (* let out = List.fold ~f:(fun acc (v1, v2) -> *)
  (*     G.add_edge_e acc (v1, EdgeLabel.DataFlow, v2) *)
  (*   ) ~init:G.empty repaired_edges in *)

  (* Visualizer.visualize_snapshot out ~autoopen:true ~micro:false *)
