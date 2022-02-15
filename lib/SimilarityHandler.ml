open ListMonad
open InfixOperators
open GraphRepr
open Chain
open NodeWiseFeatures
open ContextualFeatures
open SpanningTree
module Json = Yojson.Basic

let make_contextual_sim_edge (graph : G.t) : G.t =
  Out_channel.print_string "spawning python process compute_contextual_similarity.py..." ;
  SpawnPython.spawn_python ~pyfile:"./lib/python/compute_contextual_similarity.py"
    ~args:[graph.comp_unit] ;
  Out_channel.print_endline "done" ;
  let csv_filename = F.asprintf "%s_all_longest_trunks.json_filtered.csv" graph.comp_unit in
  let in_chan = In_channel.create csv_filename in
  let csv_array = Csv.to_array @@ Csv.load_in in_chan in
  In_channel.close in_chan ;
  let acc = ref G.empty in
  Out_channel.print_string "Now adding CS edges..." ;
  Out_channel.flush stdout ;
  for i = 1 to Array.length csv_array - 1 do
    let vertex1 = csv_array.(i).(0) and vertex2 = csv_array.(i).(1) in
    let vertex1 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex1) graph.graph
    and vertex2 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex2) graph.graph in
    acc := G.add_edge_e !acc (vertex1, EdgeLabel.ContextualSimilarity, vertex2) ;
    acc := G.add_edge_e !acc (vertex2, EdgeLabel.ContextualSimilarity, vertex1)
  done ;
  let dieted = prune_to_mst !acc in
  let out =
    G.fold_edges
      (fun v1 v2 acc -> G.add_edge_e acc (v1, EdgeLabel.ContextualSimilarity, v2))
      dieted graph
  in
  Out_channel.print_endline "done" ;
  out


let make_nodewise_sim_edge (graph : G.t) : G.t =
  Out_channel.print_string "spawning python process compute_nodewise_similarity.py..." ;
  SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py"
    ~args:[graph.comp_unit] ;
  Out_channel.print_endline "done" ;
  let api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit in
  let api_in_chan = In_channel.create api_csv_filename in
  let api_array = Csv.to_array @@ Csv.load_in api_in_chan in
  In_channel.close api_in_chan ;
  Out_channel.print_string "Now adding NS edges..." ;
  Out_channel.flush stdout ;
  (* make a temporary empty graph *)
  let acc = ref G.empty in
  for i = 0 to Array.length api_array - 1 do
    let method1 = api_array.(i).(0) and method2 = api_array.(i).(1) in
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
                   (G.get_edges graph ~label:EdgeLabel.ContextualSimilarity)
            in
            if there_is_no_cs then (
              let edge1 = (m1_vertex, EdgeLabel.NodeWiseSimilarity, m2_vertex)
              and edge2 = (m2_vertex, EdgeLabel.NodeWiseSimilarity, m1_vertex) in
              acc := G.add_edge_e !acc edge1 ;
              acc := G.add_edge_e !acc edge2 ) )
          m2_vertices )
      m1_vertices
  done ;
  let dieted = prune_to_mst !acc in
  let out =
    G.fold_edges
      (fun v1 v2 acc -> G.add_edge_e acc (v1, EdgeLabel.NodeWiseSimilarity, v2))
      dieted graph
  in
  Out_channel.print_endline "done" ;
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


(** cheap version of GraphRepr.all_ns_clusters *)
let all_ns_clusters (graph : G.t) : G.V.t list list =
  let udf_json_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.json" graph.comp_unit
  and api_json_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.json" graph.comp_unit in
  let udf_parsed = parse_mst_json udf_json_filename
  and api_parsed = parse_mst_json api_json_filename in
  let udf_mst_vertices = udf_parsed >>| fun tuplist -> tuplist >>= ident |> List.stable_dedup
  and api_mst_vertices = api_parsed >>| fun tuplist -> tuplist >>= ident |> List.stable_dedup in
  udf_mst_vertices @ api_mst_vertices >>| fun cluster -> cluster >>= G.this_method_vertices graph
