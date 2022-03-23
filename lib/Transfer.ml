open InfixOperators
open ListMonad
open Utils
open GraphRepr
open SpawnPython
open TaintLabel
open InferenceResult
open PropagationRules
open Propagator

let resolve_multiple_labels (method_ : Method.t) (labels : TaintLabel.t list) (next_graph : G.t) :
    (Response.t * ProbQuadruple.t) list =
  let this_method_vertices = G.this_method_vertices next_graph method_ in
  List.fold this_method_vertices
    ~f:(fun acc vertex ->
      if
        Trunk.vertex_is_close_to_root next_graph (G.LiteralVertex.of_vertex vertex)
        && List.exists labels ~f:TaintLabel.is_source
      then
        let response = Response.ForLabel (method_, Source)
        and next_vertex_dist = DistManipulator.overwrite ~src:5. ~sin:2. ~san:2. ~non:2. in
        (response, next_vertex_dist) :: acc
      else if
        Trunk.vertex_is_close_to_leaf next_graph (G.LiteralVertex.of_vertex vertex)
        && List.exists labels ~f:TaintLabel.is_sink
      then
        let response = Response.ForLabel (method_, Sink)
        and next_vertex_dist = DistManipulator.overwrite ~src:2. ~sin:5. ~san:2. ~non:2. in
        (response, next_vertex_dist) :: acc
      else if List.exists labels ~f:TaintLabel.is_sanitizer then
        let response = Response.ForLabel (method_, Sanitizer)
        and next_vertex_dist = DistManipulator.overwrite ~src:2. ~sin:2. ~san:5. ~non:2. in
        (response, next_vertex_dist) :: acc
      else acc )
    ~init:[]


let transfer_single_method (next_graph_method : Method.t) (label : TaintLabel.t) :
    Response.t * ProbQuadruple.t =
  let response = Response.ForLabel (next_graph_method, label)
  and dist =
    match label with
    | Source ->
        DistManipulator.overwrite ~src:5. ~sin:2. ~san:2. ~non:2.
    | Sink ->
        DistManipulator.overwrite ~src:2. ~sin:5. ~san:2. ~non:2.
    | Sanitizer ->
        DistManipulator.overwrite ~src:2. ~sin:2. ~san:5. ~non:2.
    | None ->
        DistManipulator.overwrite ~src:2. ~sin:2. ~san:2. ~non:5.
    | Indeterminate ->
        ProbQuadruple.initial
  in
  (response, dist)


let get_api_and_udf_pairs_to_transfer (prev_graph_comp_unit : string) (next_graph_comp_unit : string)
    : string array array * string array array =
  let csv_filename_api =
    F.asprintf "%s->%s_api_transferred.csv" prev_graph_comp_unit next_graph_comp_unit
  and csv_filename_udf =
    F.asprintf "%s->%s_udf_transferred.csv" prev_graph_comp_unit next_graph_comp_unit
  in
  if not @@ Sys.file_exists_exn csv_filename_api then
    spawn_python ~pyfile:"./lib/python/compute_api_to_transfer.py"
      ~args:["--source"; prev_graph_comp_unit; "--target"; next_graph_comp_unit] ;
  if not @@ Sys.file_exists_exn csv_filename_udf then
    spawn_python ~pyfile:"./lib/python/compute_udf_to_transfer.py"
      ~args:["--source"; prev_graph_comp_unit; "--target"; next_graph_comp_unit] ;
  let api_in_chan = In_channel.create csv_filename_api
  and udf_in_chan = In_channel.create csv_filename_udf in
  (* remove the headers *)
  let api_method_pairs = Array.slice (Csv.to_array @@ Csv.load_in api_in_chan) 1 0
  and udf_method_pairs = Array.slice (Csv.to_array @@ Csv.load_in udf_in_chan) 1 0 in
  In_channel.close api_in_chan ;
  In_channel.close udf_in_chan ;
  (api_method_pairs, udf_method_pairs)


let transfer_graph (prev_graph : G.t) (next_graph : G.t) : G.t =
  if G.is_empty prev_graph then next_graph
  else
    let api_method_pairs, udf_method_pairs =
      get_api_and_udf_pairs_to_transfer prev_graph.comp_unit next_graph.comp_unit
    in
    let all_pairs = Array.append api_method_pairs udf_method_pairs
    and prev_label_result_map = dist_map_to_label_map @@ make_dist_result_map prev_graph in
    fst
    @@ Array.fold all_pairs
         ~f:(fun (graph_acc, transferred_responses) method_pair ->
           let prev_graph_method = method_pair.(1) and next_graph_method = method_pair.(2) in
           let prev_graph_method_labels =
             LabelResultMap.find prev_graph_method prev_label_result_map
           in
           match prev_graph_method_labels with
           | [label] ->
               let response, dist = transfer_single_method next_graph_method label in
               let this_method_propagated =
                 List.fold
                   (G.this_method_vertices graph_acc next_graph_method)
                   ~f:(fun acc vertex -> G.strong_update_dist vertex dist acc)
                   ~init:next_graph
               in
               let propagation_rules_to_apply =
                 MetaRules.ForPropagation.sort_propagation_rules_by_priority this_method_propagated
                   response
               in
               let propagated =
                 fst
                 @@ propagator response this_method_propagated propagation_rules_to_apply
                      transferred_responses [||] PropagationRules.all_rules
               in
               (Axioms.apply_axioms propagated, response :: transferred_responses)
           | [_; _] as labels ->
               let responses = resolve_multiple_labels next_graph_method labels graph_acc in
               List.fold responses
                 ~f:(fun (big_graph_acc, big_response_acc) (response, dist) ->
                   let this_method_propagated =
                     List.fold
                       (G.this_method_vertices big_graph_acc next_graph_method)
                       ~f:(fun acc vertex -> G.strong_update_dist vertex dist acc)
                       ~init:big_graph_acc
                   in
                   let propagation_rules_to_apply =
                     MetaRules.ForPropagation.sort_propagation_rules_by_priority
                       this_method_propagated response
                   in
                   let propagated =
                     fst
                     @@ propagator response this_method_propagated propagation_rules_to_apply
                          big_response_acc [||] PropagationRules.all_rules
                   in
                   (Axioms.apply_axioms propagated, response :: big_response_acc) )
                 ~init:(graph_acc, [])
           | otherwise ->
               (* skip instead of raise *)
               print_endline
               @@ F.asprintf "transfer_graph encountered an edge case: %s."
                    (TaintLabel.string_of_list otherwise) ;
               (graph_acc, transferred_responses) )
         ~init:(next_graph, [])


let transfer_from_json ~(filename : string) ~(prev_comp_unit : string) (next_graph : G.t) : G.t =
  let api_method_pairs, udf_method_pairs =
    get_api_and_udf_pairs_to_transfer prev_comp_unit next_graph.comp_unit
  in
  let all_pairs = Array.append api_method_pairs udf_method_pairs
  and prev_label_result_map = deserialize_label_result_map filename prev_comp_unit in
  fst
  @@ Array.fold all_pairs
       ~f:(fun (big_graph_acc, big_transferred_responses) method_pair ->
         print_endline @@ F.asprintf "%s -> %s" method_pair.(1) method_pair.(2) ;
         let prev_graph_method = method_pair.(1) and next_graph_method = method_pair.(2) in
         let prev_graph_method_labels =
           LabelResultMap.find prev_graph_method prev_label_result_map
         in
         match prev_graph_method_labels with
         | [label] ->
             let response, dist = transfer_single_method next_graph_method label in
             let this_method_propagated =
               List.fold
                 (G.this_method_vertices big_graph_acc next_graph_method)
                 ~f:(fun smol_acc vertex -> G.strong_update_dist vertex dist smol_acc)
                 ~init:next_graph
             in
             let propagation_rules_to_apply =
               MetaRules.ForPropagation.sort_propagation_rules_by_priority this_method_propagated
                 response
             in
             let propagated =
               fst
               @@ propagator response this_method_propagated propagation_rules_to_apply
                    big_transferred_responses [||] PropagationRules.all_rules
             in
             (Axioms.apply_axioms propagated, response :: big_transferred_responses)
         | [_; _] as labels ->
             let responses = resolve_multiple_labels next_graph_method labels big_graph_acc in
             List.fold responses
               ~f:(fun (big_graph_acc, big_response_acc) (response, dist) ->
                 let this_method_propagated =
                   List.fold
                     (G.this_method_vertices big_graph_acc next_graph_method)
                     ~f:(fun acc vertex -> G.strong_update_dist vertex dist acc)
                     ~init:next_graph
                 in
                 let propagation_rules_to_apply =
                   MetaRules.ForPropagation.sort_propagation_rules_by_priority
                     this_method_propagated response
                 in
                 let propagated =
                   fst
                   @@ propagator response this_method_propagated propagation_rules_to_apply
                        big_response_acc [||] PropagationRules.all_rules
                 in
                 (Axioms.apply_axioms propagated, response :: big_response_acc) )
               ~init:(next_graph, [])
         | otherwise ->
             (* skip instead of raise *)
             print_endline
             @@ F.asprintf "transfer_graph encountered an edge case: %s."
                  (TaintLabel.string_of_list otherwise) ;
             (big_graph_acc, big_transferred_responses) )
       ~init:(next_graph, [])
