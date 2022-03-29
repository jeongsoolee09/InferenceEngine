open InfixOperators
open ListMonad
open GraphRepr
open TaintLabel
open Propagator
open Utils
module Hashtbl = Caml.Hashtbl

let prepare_solution (raw_solution : (string * string list) array) :
    (string, TaintLabel.t list) Hashtbl.t =
  let acc = Hashtbl.create 777 in
  Array.iter
    ~f:(fun (method_, label_strs) -> Hashtbl.add acc method_ (label_strs >>| TaintLabel.of_string))
    raw_solution ;
  acc


let solution_table =
  prepare_solution
  @@ Array.concat
       [ Sagan_solution.sagan_udf_solution
       ; Sagan_solution.sagan_api_solution
       ; Sagan_solution.sagan_interface_solution ]


let get_solution (method_ : Method.t) : TaintLabel.t list =
  if Method.is_initializer method_ then [None] else Hashtbl.find solution_table method_


module Scoring = struct
  let vertex_inference_result_is_correct (vertex : Vertex.t) : bool =
    let ground = get_solution (Vertex.get_method vertex)
    and inferred = ProbQuadruple.determine_label (Vertex.get_dist vertex) in
    List.mem ground inferred ~equal:TaintLabel.equal


  (** get the percentage of correct vertices in this snapshot. *)
  let get_vertexwise_precision_of_snapshot (snapshot : G.t) : Float.t =
    let correct_vertices_count =
      G.fold_vertex
        (fun vertex acc -> if vertex_inference_result_is_correct vertex then acc + 1 else acc)
        snapshot 0
    in
    let num_of_all_vertices = List.length @@ G.all_vertices_of_graph snapshot in
    Float.of_int correct_vertices_count /. Float.of_int num_of_all_vertices *. 100.


  let is_srm (method_ : Method.t) : bool =
    if Method.is_initializer method_ then false
    else
      let label_strs = Hashtbl.find solution_table method_ in
      List.mem label_strs Source ~equal:TaintLabel.equal
      || List.mem label_strs Sink ~equal:TaintLabel.equal
      || List.mem label_strs Sanitizer ~equal:TaintLabel.equal


  let srm_report_of_snapshot (snapshot : G.t) : int * int * float =
    let all_srm_vertices_count =
      G.fold_vertex
        (fun vertex acc -> if is_srm (Vertex.get_method vertex) then acc + 1 else acc)
        snapshot 0
    and correct_srm_vertex_count =
      G.fold_vertex
        (fun vertex acc ->
          if is_srm (Vertex.get_method vertex) then
            let estimation_is_correct = vertex_inference_result_is_correct vertex in
            if estimation_is_correct then acc + 1 else acc
          else acc )
        snapshot 0
    in
    ( correct_srm_vertex_count
    , all_srm_vertices_count
    , Float.of_int correct_srm_vertex_count /. Float.of_int all_srm_vertices_count *. 100. )
end

let srm_map_of_snapshot (snapshot : G.t) : string =
  let label_result_map = InferenceResult.make_label_result_map snapshot in
  let only_srm_map =
    InferenceResult.LabelResultMap.filter (fun meth _ -> Scoring.is_srm meth) label_result_map
  in
  let json_repr = InferenceResult.Serializer.to_json_repr snapshot only_srm_map in
  JSON.pretty_to_string json_repr


let watch (snapshot : G.t) (methods : Method.t list) : string =
  let label_result_map = InferenceResult.make_label_result_map snapshot in
  let only_these_methods_map =
    InferenceResult.LabelResultMap.filter
      (fun meth _ -> List.mem methods meth ~equal:Method.equal)
      label_result_map
  in
  let json_repr = InferenceResult.Serializer.to_json_repr snapshot only_these_methods_map in
  JSON.pretty_to_string json_repr


let watch_for_class (snapshot : G.t) (classes : string list) : string =
  let label_result_map = InferenceResult.make_label_result_map snapshot in
  let only_these_methods_map =
    InferenceResult.LabelResultMap.filter
      (fun meth _ -> List.mem classes (Method.get_class_name meth) ~equal:Method.equal)
      label_result_map
  in
  let json_repr = InferenceResult.Serializer.to_json_repr snapshot only_these_methods_map in
  JSON.pretty_to_string json_repr


let responder =
  let memory : (Method.t, TaintLabel.t list) Hashtbl.t = Hashtbl.create 777 in
  fun (question : Question.t) : Response.t ->
    match question with
    | AskingForLabel meth ->
        (* give any label not in memory *)
        let solution_labels = get_solution meth in
        let not_responded_labels =
          match Hashtbl.find_opt memory meth with
          | None ->
              solution_labels
          | Some responded_labels ->
              List.filter solution_labels ~f:(fun label ->
                  not @@ List.mem responded_labels label ~equal:TaintLabel.equal )
        in
        let not_responded_label = Utils.random_elem not_responded_labels in
        Response.ForLabel (meth, not_responded_label)
    | AskingForConfirmation (meth, asked_label) ->
        (* see if the asked_label is in the solution_labels. *)
        let is_correct = List.mem (get_solution meth) asked_label ~equal:TaintLabel.equal in
        Response.ForYesOrNo (meth, asked_label, is_correct)


let auto_test_spechunter_for_snapshot_once (current_snapshot : G.t)
    (received_responses : Response.t list) =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
    (current_snapshot, received_responses)
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
    in
    let question = question_maker.rule current_snapshot received_responses ~dry_run:false in
    print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
    let response = responder question in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
    in
    let propagated =
      fst
      @@ propagator response current_snapshot propagation_rules_to_apply received_responses [||]
           PropagationRules.all_rules
    in
    let propagated' = Axioms.apply_axioms propagated in
    let stats =
      let correct_vertices_count =
        G.fold_vertex
          (fun vertex acc ->
            if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
          propagated' 0
      in
      F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
        correct_vertices_count (G.nb_vertex propagated')
        (Scoring.get_vertexwise_precision_of_snapshot propagated')
        ( List.length
        @@ List.filter
             (G.all_vertices_of_graph propagated')
             ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
        (G.nb_vertex propagated')
    in
    print_endline stats ;
    let srm_stats =
      let correct_srms_count, all_srms_count, accuracy =
        Scoring.srm_report_of_snapshot propagated'
      in
      F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
    in
    print_endline srm_stats ;
    print_endline @@ srm_map_of_snapshot propagated' ;
    (propagated', response :: received_responses)


let rec auto_test_spechunter_for_snapshot_inner (current_snapshot : G.t)
    (received_responses : Response.t list)
    (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) (count : int)
    (log_data_acc : string list) : G.t * string =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
    (current_snapshot, List.rev log_data_acc |> List.map ~f:(fun str -> str ^ "\n") |> String.concat)
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
    in
    let question = question_maker.rule current_snapshot received_responses ~dry_run:false in
    print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
    let response = responder question in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
    in
    let propagated =
      fst
      @@ propagator response current_snapshot propagation_rules_to_apply received_responses [||]
           PropagationRules.all_rules
    in
    let propagated' = Axioms.apply_axioms propagated in
    let stats =
      let correct_vertices_count =
        G.fold_vertex
          (fun vertex acc ->
            if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
          propagated' 0
      in
      F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
        correct_vertices_count (G.nb_vertex propagated')
        (Scoring.get_vertexwise_precision_of_snapshot propagated')
        ( List.length
        @@ List.filter
             (G.all_vertices_of_graph propagated')
             ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
        (G.nb_vertex propagated')
    in
    print_endline stats ;
    let srm_stats =
      let correct_srms_count, all_srms_count, accuracy =
        Scoring.srm_report_of_snapshot propagated'
      in
      F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
    in
    print_endline srm_stats ;
    (* print_endline @@ srm_map_of_snapshot propagated' ; *)
    print_endline @@ watch_for_class propagated' ["Optional"; "String"; "StringBuilder"] ;
    G.serialize_to_bin ~suffix:"ahahaha" propagated' ;
    (* TEMP *)
    auto_test_spechunter_for_snapshot_inner propagated' (response :: received_responses)
      nodewise_featuremap (count + 1) (stats :: log_data_acc)


let auto_test (initial_snapshot : G.t) : unit =
  let _, log_data =
    auto_test_spechunter_for_snapshot_inner initial_snapshot []
      NodeWiseFeatures.NodeWiseFeatureMap.empty 0 []
  in
  let out_chan =
    Out_channel.create @@ F.asprintf "%s_%s.txt" (make_now_string 9) initial_snapshot.comp_unit
  in
  Out_channel.output_string out_chan log_data ;
  Out_channel.close out_chan
