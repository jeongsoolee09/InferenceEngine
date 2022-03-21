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
  let prev_label_result_map = dist_map_to_label_map @@ make_dist_result_map prev_graph in
  LabelResultMap.fold
    (fun method_ labels acc ->
      match labels with
      | [label] ->
          transfer_single_method method_ label acc
      | [_; _] as labels ->
          resolve_multiple_labels method_ labels acc
      | otherwise ->
          (* skip instead of raising *)
          print_endline
          @@ F.asprintf "transfer_graph encountered an edge case: %s."
               (TaintLabel.string_of_list otherwise) ;
          acc )
    prev_label_result_map next_graph


let transfer_from_json (json : JSON.t) (next_graph : G.t) : G.t =
  (* let label_result_map = JS *)
  (* LabelResultMap.fold *)
  (*   (fun method_ labels acc -> *)
  (*     match labels with *)
  (*     | [label] -> *)
  (*         transfer_single_method method_ label acc *)
  (*     | [_; _] as labels -> *)
  (*         resolve_multiple_labels method_ labels acc *)
  (*     | otherwise -> *)
  (*         (\* skip instead of raising *\) *)
  (*         print_endline *)
  (*         @@ F.asprintf "transfer_graph encountered an edge case: %s." *)
  (*              (TaintLabel.string_of_list otherwise) ; *)
  (*         acc ) *)
  (*   prev_label_result_map next_graph *)
  raise TODO
