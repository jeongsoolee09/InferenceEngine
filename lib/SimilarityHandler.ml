open ListMonad
open InfixOperators
open GraphRepr
open Chain
open NodeWiseFeatures
open ContextualFeatures
open SpanningTree
open EdgeLabel
module Json = Yojson.Basic

let make_contextual_sim_edge (graph : G.t) : G.t =
  let csv_filename = F.asprintf "%s_all_longest_trunks.json_filtered.csv" graph.comp_unit in
  if not @@ Sys.file_exists_exn csv_filename then (
    Out_channel.print_string "spawning python process compute_contextual_similarity.py..." ;
    SpawnPython.spawn_python ~pyfile:"./lib/python/compute_contextual_similarity.py"
      ~args:[graph.comp_unit] ;
    Out_channel.print_endline "done" ) ;
  let in_chan = In_channel.create csv_filename in
  let csv_array = Csv.to_array @@ Csv.load_in in_chan in
  In_channel.close in_chan ;
  let acc = ref G.empty and history = ref [] in
  Out_channel.print_string "Now adding CS edges..." ;
  Out_channel.flush stdout ;
  Array.iter csv_array ~f:(fun array ->
      let vertex1 = array.(0) and vertex2 = array.(1) in
      let vertex1 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex1) graph.graph
      and vertex2 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex2) graph.graph in
      let method1 = Vertex.get_method vertex1 and method2 = Vertex.get_method vertex2 in
      if
        not
        @@ List.mem !history (method1, method2) ~equal:(fun (m11, m12) (m21, m22) ->
               Method.equal m11 m21 && Method.equal m12 m22 )
      then acc := G.add_edge_e !acc (vertex1, ContextualSimilarity, vertex2) ;
      acc := G.add_edge_e !acc (vertex2, ContextualSimilarity, vertex1) ;
      history := (method1, method2) :: !history ;
      history := (method2, method1) :: !history ) ;
  let dieted = prune_to_mst !acc in
  let out =
    G.fold_edges (fun v1 v2 acc -> G.add_edge_e acc (v1, ContextualSimilarity, v2)) dieted graph
  in
  Out_channel.print_endline "done adding contextual edges." ;
  out


let make_nodewise_sim_edge (graph : G.t) : G.t =
  let api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit in
  let udf_csv_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.csv" graph.comp_unit in
  if not @@ Sys.file_exists_exn api_csv_filename then
    SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity_apis.py"
      ~args:[graph.comp_unit] ;
  if not @@ Sys.file_exists_exn udf_csv_filename then
    SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity_udfs.py"
      ~args:[graph.comp_unit] ;
  let api_in_chan = In_channel.create api_csv_filename
  and udf_in_chan = In_channel.create udf_csv_filename in
  let api_array = Csv.to_array @@ Csv.load_in api_in_chan
  and udf_array = Csv.to_array @@ Csv.load_in udf_in_chan in
  In_channel.close api_in_chan ;
  In_channel.close udf_in_chan ;
  Out_channel.print_string "Now adding NS edges..." ;
  Out_channel.flush stdout ;
  let acc = ref [] in
  Array.iter api_array ~f:(fun array_ ->
      let method1 = array_.(1) and method2 = array_.(2) in
      let m1_vertices = G.this_method_vertices graph method1
      and m2_vertices = G.this_method_vertices graph method2 in
      List.iter
        ~f:(fun m1_vertex ->
          List.iter
            ~f:(fun m2_vertex ->
              let m1_vertex_method = Vertex.get_method m1_vertex
              and m2_vertex_method = Vertex.get_method m2_vertex in
              let there_is_no_cs =
                not
                @@ List.exists
                     ~f:(fun (v1, _, v2) ->
                       let v1_method = Vertex.get_method v1 and v2_method = Vertex.get_method v2 in
                       Method.equal m1_vertex_method v1_method
                       && Method.equal m2_vertex_method v2_method )
                     (G.get_edges graph ~label:ContextualSimilarity)
              in
              if there_is_no_cs then
                let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
                and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
                acc := edge1 :: edge2 :: !acc )
            m2_vertices )
        m1_vertices ) ;
  Array.iter udf_array ~f:(fun array_ ->
      let method1 = array_.(1) and method2 = array_.(2) in
      let m1_vertices = G.this_method_vertices graph method1
      and m2_vertices = G.this_method_vertices graph method2 in
      List.iter
        ~f:(fun m1_vertex ->
          List.iter
            ~f:(fun m2_vertex ->
              let m1_vertex_method = Vertex.get_method m1_vertex
              and m2_vertex_method = Vertex.get_method m2_vertex in
              let there_is_no_cs =
                not
                @@ List.exists
                     ~f:(fun (v1, _, v2) ->
                       let v1_method = Vertex.get_method v1 and v2_method = Vertex.get_method v2 in
                       Method.equal m1_vertex_method v1_method
                       && Method.equal m2_vertex_method v2_method )
                     (G.get_edges graph ~label:ContextualSimilarity)
              in
              if there_is_no_cs then
                let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
                and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
                acc := edge1 :: edge2 :: !acc )
            m2_vertices )
        m1_vertices ) ;
  let dieted = diet_edge_list !acc in
  let out =
    List.fold
      ~f:(fun acc (v1, _, v2) -> G.add_edge_e acc (v1, NodeWiseSimilarity, v2))
      dieted ~init:graph
  in
  Out_channel.print_endline "done adding nodewise edges." ;
  out


let parse_mst_json (filename : string) =
  let udf_json =
    let in_chan = In_channel.create filename in
    let json = Json.from_channel in_chan in
    In_channel.close in_chan ;
    json
  in
  Json.Util.to_list udf_json
  >>| fun lst -> Json.Util.to_list lst >>| fun l -> Json.Util.to_list l |> Json.Util.filter_string


let all_ns_clusters : G.t -> G.t array =
  G.leave_only_ns_edges >> WeaklyConnectedComponents.find_distinct_subgraphs_with_edges


let all_ns_clusters_api_only : G.t -> G.t array =
  G.leave_only_ns_edges >> WeaklyConnectedComponents.find_distinct_subgraphs_with_edges
  >> Array.filter ~f:(fun subgraph ->
         List.for_all (G.all_methods_of_graph subgraph) ~f:Method.is_api )
