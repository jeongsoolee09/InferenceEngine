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
      ~args:[graph.comp_unit] ) ;
  Out_channel.print_endline "done" ;
  let in_chan = In_channel.create csv_filename in
  let csv_array = Csv.to_array @@ Csv.load_in in_chan in
  In_channel.close in_chan ;
  let acc = ref G.empty and history = ref [] in
  Out_channel.print_string "Now adding CS edges..." ;
  Out_channel.flush stdout ;
  for i = 1 to Array.length csv_array - 1 do
    let vertex1 = csv_array.(i).(0) and vertex2 = csv_array.(i).(1) in
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
    history := (method2, method1) :: !history
  done ;
  let dieted = prune_to_mst !acc in
  let out =
    G.fold_edges (fun v1 v2 acc -> G.add_edge_e acc (v1, ContextualSimilarity, v2)) dieted graph
  in
  Out_channel.print_endline "done" ;
  out


let make_nodewise_sim_edge (graph : G.t) : G.t =
  let api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit in
  let udf_csv_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.csv" graph.comp_unit in
  if not @@ Sys.file_exists_exn api_csv_filename then (
    Out_channel.print_string "spawning python process compute_nodewise_similarity.py..." ;
    SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py"
      ~args:[graph.comp_unit] ) ;
  Out_channel.print_endline "done" ;
  let api_in_chan = In_channel.create api_csv_filename in
  let api_array = Csv.to_array @@ Csv.load_in api_in_chan in
  let udf_in_chan = In_channel.create udf_csv_filename in
  let udf_array = Csv.to_array @@ Csv.load_in udf_in_chan in
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
                   (G.get_edges graph ~label:ContextualSimilarity)
            in
            if there_is_no_cs then (
              let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
              and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
              acc := G.add_edge_e !acc edge1 ;
              acc := G.add_edge_e !acc edge2 ) )
          m2_vertices )
      m1_vertices
  done ;
  for i = 0 to Array.length udf_array - 1 do
    let method1 = udf_array.(i).(0) and method2 = udf_array.(i).(1) in
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
            if there_is_no_cs then (
              let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
              and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
              acc := G.add_edge_e !acc edge1 ;
              acc := G.add_edge_e !acc edge2 ) )
          m2_vertices )
      m1_vertices
  done ;
  let dieted = prune_to_mst !acc in
  let out =
    G.fold_edges (fun v1 v2 acc -> G.add_edge_e acc (v1, NodeWiseSimilarity, v2)) dieted graph
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


(** NOTE This function is wrong. Deprecated. *)
let temp_make_nodewise_sim_edge_Mapping (graph : G.t) : G.t =
  let mapping_annotated_vertices =
    Array.of_list
    @@ List.filter
         ~f:(fun vertex ->
           let method_ = Vertex.get_method vertex in
           Annotations.has_annot method_
           &&
           let annots = Annotations.get_annots method_ in
           List.exists annots ~f:(fun annot ->
               let annot_name = annot.name in
               String.is_substring annot_name ~substring:"Mapping" ) )
         (G.all_vertices_of_graph graph)
  in
  let raw_edges =
    Array.cartesian_product mapping_annotated_vertices mapping_annotated_vertices
    |> Array.filter ~f:(fun (v1, v2) ->
           not @@ Method.equal (Vertex.get_method v1) (Vertex.get_method v2) )
    |> Array.map ~f:(fun (v1, v2) -> (v1, NodeWiseSimilarity, v2))
  in
  let mst_edges = Array.of_list @@ diet_edge_list (Array.to_list raw_edges) in
  let acc = ref graph in
  let history : (Method.t * Method.t) list ref = ref [] in
  Array.iter
    ~f:(fun (v1, label, v2) ->
      let method1 = Vertex.get_method v1 and method2 = Vertex.get_method v2 in
      if
        not
        @@ List.mem !history (method1, method2) ~equal:(fun (m11, m12) (m21, m22) ->
               Method.equal m11 m21 && Method.equal m12 m22 )
      then acc := G.add_edge_e !acc (v1, label, v2) ;
      history := (method1, method2) :: !history )
    mst_edges ;
  !acc


let temp_make_nodewise_sim_edge_Printer (graph : G.t) : G.t =
  let printer_methods =
    Array.of_list
    @@ List.filter (G.all_vertices_of_graph graph) ~f:(fun vertex ->
           String.equal (vertex |> Vertex.get_method |> Method.get_class_name) "Printer" )
  in
  let raw_edges =
    Array.cartesian_product printer_methods printer_methods
    |> Array.filter ~f:(fun (v1, v2) -> not @@ Vertex.equal v1 v2)
    |> Array.map ~f:(fun (v1, v2) -> (v1, NodeWiseSimilarity, v2))
  in
  let mst_edges = Array.of_list @@ diet_edge_list (Array.to_list raw_edges) in
  let acc = ref graph in
  let history : (Method.t * Method.t) list ref = ref [] in
  Array.iter
    ~f:(fun (v1, label, v2) ->
      let method1 = Vertex.get_method v1 and method2 = Vertex.get_method v2 in
      if
        not
        @@ List.mem !history (method1, method2) ~equal:(fun (m11, m12) (m21, m22) ->
               Method.equal m11 m21 && Method.equal m12 m22 )
      then acc := G.add_edge_e !acc (v1, label, v2) ;
      history := (method1, method2) :: !history )
    mst_edges ;
  !acc
