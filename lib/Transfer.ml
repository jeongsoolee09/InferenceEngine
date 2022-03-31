open InfixOperators
open ListMonad
open Utils
open GraphRepr
open SpawnPython
open TaintLabel
open InferenceResult

let resolve_multiple_labels (method_ : Method.t) (labels : TaintLabel.t list) (next_graph : G.t) =
  let this_method_vertices = G.this_method_vertices next_graph method_ in
  List.fold this_method_vertices
    ~f:(fun acc vertex ->
      if
        Trunk.vertex_is_close_to_root acc (G.LiteralVertex.of_vertex vertex)
        && List.exists labels ~f:TaintLabel.is_source
      then
        (* it it's close to root, then get the Src if it exists *)
        G.strong_update_dist vertex (DistManipulator.overwrite ~src:5. ~sin:2. ~san:2. ~non:2.) acc
      else if
        Trunk.vertex_is_close_to_leaf acc (G.LiteralVertex.of_vertex vertex)
        && List.exists labels ~f:TaintLabel.is_sink
      then
        (* it it's close to leaf, then get the Sin if it exists *)
        G.strong_update_dist vertex (DistManipulator.overwrite ~src:2. ~sin:5. ~san:2. ~non:2.) acc
      else if List.exists labels ~f:TaintLabel.is_sanitizer then
        G.strong_update_dist vertex (DistManipulator.overwrite ~src:2. ~sin:2. ~san:5. ~non:2.) acc
      else
        G.strong_update_dist vertex (DistManipulator.overwrite ~src:2. ~sin:2. ~san:2. ~non:5.) acc
      )
    ~init:next_graph


let get_api_udf_method_pairs (prev_graph_comp_unit : string) (next_graph_comp_unit : string) :
    string array array * string array array =
  let csv_filename_api =
    F.asprintf "%s->%s_api_transferred.csv" prev_graph_comp_unit next_graph_comp_unit
  and csv_filename_udf =
    F.asprintf "%s->%s_udf_transferred.csv" prev_graph_comp_unit next_graph_comp_unit
  in
  if not @@ Sys.file_exists_exn csv_filename_api then
    spawn_python ~pyfile:"./lib/python/compute_apis_to_transfer.py"
      ~args:["--source"; prev_graph_comp_unit; "--target"; next_graph_comp_unit] ;
  if not @@ Sys.file_exists_exn csv_filename_udf then
    spawn_python ~pyfile:"./lib/python/compute_udfs_to_transfer.py"
      ~args:["--source"; prev_graph_comp_unit; "--target"; next_graph_comp_unit] ;
  let api_in_chan = In_channel.create csv_filename_api
  and udf_in_chan = In_channel.create csv_filename_udf in
  (* remove the headers *)
  let api_method_pairs = Array.slice (Csv.to_array @@ Csv.load_in api_in_chan) 1 0
  and udf_method_pairs = Array.slice (Csv.to_array @@ Csv.load_in udf_in_chan) 1 0 in
  In_channel.close api_in_chan ;
  In_channel.close udf_in_chan ;
  (api_method_pairs, udf_method_pairs)


let transfer_single_method (method_ : Method.t) (label : TaintLabel.t) (next_graph : G.t) : G.t =
  let this_method_vertices_in_next_graph = G.this_method_vertices next_graph method_ in
  List.fold this_method_vertices_in_next_graph
    ~f:(fun acc vertex ->
      let transferred_dist =
        match label with
        | Source ->
            DistManipulator.overwrite ~src:8. ~sin:2. ~san:2. ~non:2.
        | Sink ->
            DistManipulator.overwrite ~src:2. ~sin:8. ~san:2. ~non:2.
        | Sanitizer ->
            DistManipulator.overwrite ~src:2. ~sin:2. ~san:8. ~non:2.
        | None ->
            DistManipulator.overwrite ~src:2. ~sin:2. ~san:2. ~non:8.
        | Indeterminate ->
            ProbQuadruple.initial
      in
      G.strong_update_dist vertex transferred_dist acc )
    ~init:next_graph


let transfer_graph (prev_graph : G.t) (next_graph : G.t) : G.t =
  if G.is_empty prev_graph then next_graph
  else
    let api_method_pairs, udf_method_pairs =
      get_api_udf_method_pairs prev_graph.comp_unit next_graph.comp_unit
    in
    let all_pairs = Array.append api_method_pairs udf_method_pairs
    and prev_label_result_map = dist_map_to_label_map @@ make_dist_result_map prev_graph in
    Array.fold all_pairs
      ~f:(fun acc method_pair ->
        let prev_graph_method = method_pair.(1) and next_graph_method = method_pair.(2) in
        let prev_graph_method_labels =
          LabelResultMap.find prev_graph_method prev_label_result_map
        in
        match prev_graph_method_labels with
        | [label] ->
            transfer_single_method next_graph_method label acc
        | [_; _] as labels ->
            resolve_multiple_labels next_graph_method labels acc
        | otherwise ->
            (* skip instead of raise *)
            print_endline
            @@ F.asprintf "transfer_graph encountered an edge case: %s."
                 (TaintLabel.string_of_list otherwise) ;
            acc )
      ~init:next_graph


let transfer_from_json ~(filename : string) ~(prev_comp_unit : string) (next_graph : G.t) : G.t =
  let api_method_pairs, udf_method_pairs =
    get_api_udf_method_pairs prev_comp_unit next_graph.comp_unit
  in
  let all_pairs = Array.append api_method_pairs udf_method_pairs
  and prev_label_result_map = deserialize_label_result_map filename prev_comp_unit in
  Array.fold all_pairs
    ~f:(fun acc method_pair ->
      let prev_graph_method = method_pair.(1) and next_graph_method = method_pair.(2) in
      let prev_graph_method_labels = LabelResultMap.find prev_graph_method prev_label_result_map in
      match prev_graph_method_labels with
      | [label] ->
          print_endline "single";
          transfer_single_method next_graph_method label acc
      | [_; _] as labels ->
        print_endline "multiple";
          (* resolve_multiple_labels next_graph_method labels acc *)
        transfer_single_method next_graph_method (List.hd_exn labels) acc
      | otherwise ->
          (* skip instead of raise *)
          print_endline
          @@ F.asprintf "transfer_graph encountered an edge case: %s."
               (TaintLabel.string_of_list otherwise) ;
          acc )
    ~init:next_graph


let initiate_cluster (supergraph : G.t) (cluster : G.t) : G.t =
  let there_is_no_indeterminate =
    List.for_all (G.all_vertices_of_graph cluster)
      ~f:(Vertex.get_dist >> ProbQuadruple.is_determined)
  and all_vertices_are_indeterminate =
    List.for_all (G.all_vertices_of_graph cluster)
      ~f:(Vertex.get_dist >> ProbQuadruple.is_indeterminate)
  in
  if there_is_no_indeterminate || all_vertices_are_indeterminate then supergraph
  else
    let random_elem =
      Utils.random_elem
        (List.filter (G.all_vertices_of_graph cluster)
           ~f:(Vertex.get_dist >> ProbQuadruple.is_determined) )
    in
    let this_elem_response =
      let method_ = Vertex.get_method random_elem and dist = Vertex.get_dist random_elem in
      Response.ForLabel (method_, ProbQuadruple.determine_label dist)
    in
    let ns_propagated, _ =
      Propagator.propagator this_elem_response cluster
        [| { rule= PropagationRules.nodewise_similarity_propagation_rule
           ; label= "nodewise_similarity_propagation_rule" } |]
        [] [||]
        [| { rule= PropagationRules.nodewise_similarity_propagation_rule
           ; label= "nodewise_similarity_propagation_rule" } |]
    in
    G.fold_vertex
      (fun vertex current_supergraph ->
        G.strong_update_dist vertex (Vertex.get_dist vertex) current_supergraph )
      ns_propagated supergraph


let initiate_NS_propagation (transferred_graph : G.t) : G.t =
  let ns_clusters = SimilarityHandler.all_ns_clusters transferred_graph in
  Array.fold ns_clusters ~f:initiate_cluster ~init:transferred_graph
