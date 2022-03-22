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
    let csv_filename_api =
      F.asprintf "%s->%s_api_filtered.csv" prev_graph.comp_unit next_graph.comp_unit
    and csv_filename_udf =
      F.asprintf "%s->%s_udf_filtered.csv" prev_graph.comp_unit next_graph.comp_unit
    in
    if not @@ Sys.file_exists_exn csv_filename_api then
      spawn_python ~pyfile:"./lib/python/compute_api_to_transfer.py"
        ~args:["--source"; prev_graph.comp_unit; "--target"; next_graph.comp_unit] ;
    if not @@ Sys.file_exists_exn csv_filename_udf then
      spawn_python ~pyfile:"./lib/python/compute_udf_to_transfer.py"
        ~args:["--source"; prev_graph.comp_unit; "--target"; next_graph.comp_unit] ;
    let api_in_chan = In_channel.create csv_filename_api
    and udf_in_chan = In_channel.create csv_filename_udf in
    let api_method_pairs = Csv.to_array @@ Csv.load_in api_in_chan
    and udf_method_pairs = Csv.to_array @@ Csv.load_in udf_in_chan in
    In_channel.close api_in_chan ;
    In_channel.close udf_in_chan ;
    let all_pairs = Array.append api_method_pairs udf_method_pairs
    and prev_label_result_map = dist_map_to_label_map @@ make_dist_result_map prev_graph in
    Array.fold all_pairs
      ~f:(fun acc method_pair ->
        let prev_graph_method = method_pair.(0) and next_graph_method = method_pair.(1) in
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


let transfer_from_json (filename : string) (prev_comp_unit : string) (next_graph : G.t) : G.t =
  let csv_filename_api = F.asprintf "%s->%s_api_filtered.csv" prev_comp_unit next_graph.comp_unit
  and csv_filename_udf = F.asprintf "%s->%s_udf_filtered.csv" prev_comp_unit next_graph.comp_unit in
  if not @@ Sys.file_exists_exn csv_filename_api then
    spawn_python ~pyfile:"./lib/python/compute_api_to_transfer.py"
      ~args:["--source"; prev_comp_unit; "--target"; next_graph.comp_unit] ;
  if not @@ Sys.file_exists_exn csv_filename_udf then
    spawn_python ~pyfile:"./lib/python/compute_udf_to_transfer.py"
      ~args:["--source"; prev_comp_unit; "--target"; next_graph.comp_unit] ;
  let api_in_chan = In_channel.create csv_filename_api
  and udf_in_chan = In_channel.create csv_filename_udf in
  let api_method_pairs = Csv.to_array @@ Csv.load_in api_in_chan
  and udf_method_pairs = Csv.to_array @@ Csv.load_in udf_in_chan in
  In_channel.close api_in_chan ;
  In_channel.close udf_in_chan ;
  let all_pairs = Array.append api_method_pairs udf_method_pairs
  and prev_label_result_map = deserialize_label_result_map filename prev_comp_unit in
  Array.fold all_pairs
    ~f:(fun acc method_pair ->
      let prev_graph_method = method_pair.(0) and next_graph_method = method_pair.(1) in
      let prev_graph_method_labels = LabelResultMap.find prev_graph_method prev_label_result_map in
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


let transfer_from_labelmap (labelmap : LabelResultMap.t) (prev_comp_unit : string) (next_graph : G.t)
    : G.t =
  let csv_filename_api = F.asprintf "%s->%s_api_filtered.csv" prev_comp_unit next_graph.comp_unit
  and csv_filename_udf = F.asprintf "%s->%s_udf_filtered.csv" prev_comp_unit next_graph.comp_unit in
  if not @@ Sys.file_exists_exn csv_filename_api then
    spawn_python ~pyfile:"./lib/python/compute_api_to_transfer.py"
      ~args:["--source"; prev_comp_unit; "--target"; next_graph.comp_unit] ;
  if not @@ Sys.file_exists_exn csv_filename_udf then
    spawn_python ~pyfile:"./lib/python/compute_udf_to_transfer.py"
      ~args:["--source"; prev_comp_unit; "--target"; next_graph.comp_unit] ;
  let api_in_chan = In_channel.create csv_filename_api
  and udf_in_chan = In_channel.create csv_filename_udf in
  let api_method_pairs = Csv.to_array @@ Csv.load_in api_in_chan
  and udf_method_pairs = Csv.to_array @@ Csv.load_in udf_in_chan in
  In_channel.close api_in_chan ;
  In_channel.close udf_in_chan ;
  let all_pairs = Array.append api_method_pairs udf_method_pairs
  and prev_label_result_map = labelmap in
  Array.fold all_pairs
    ~f:(fun acc method_pair ->
      let prev_graph_method = method_pair.(0) and next_graph_method = method_pair.(1) in
      let prev_graph_method_labels = LabelResultMap.find prev_graph_method prev_label_result_map in
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
