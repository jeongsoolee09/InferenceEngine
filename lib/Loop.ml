open GraphRepr
open ListMonad
open RulesOfInference
(* open FeatureMaps *)

exception TODO

module In_channel = Core_kernel.In_channel
module Out_channel = Core_kernel.Out_channel

module Visualizer = struct
  (** (1) output a dot file of this snapshot, (2) render a svg off the dot file, and (3) show the
      svg file. *)
  let visualize_snapshot (snapshot : G.t) ~(micro : bool) ~(autoopen : bool) : unit =
    let open DataFlowEdges in
    let now_timestring = make_now_string 9 in
    let filename_without_extension =
      if micro then F.asprintf "%s_micro" now_timestring else now_timestring
    in
    graph_to_dot snapshot ~filename:(filename_without_extension ^ ".dot") ;
    ignore
    @@ Unix.system
         (F.asprintf "dot -Tsvg -o %s.svg %s.dot" filename_without_extension
            filename_without_extension ) ;
    if autoopen then ignore @@ Unix.system (F.asprintf "open %s.svg" filename_without_extension)
end

(** (1) receive a rule to propagate, (2) use that propagation rule, and (3) spawn itself to the
    propagation targets. *)
let rec propagator (new_fact : Response.t) (current_snapshot : G.t) (previous_snapshot : G.t option)
    (rules_to_propagate : PropagationRules.t list) (prev_facts : Response.t list)
    (history : Vertex.t list) (prop_rule_pool : PropagationRules.t list) : G.t * Vertex.t list =
  if List.is_empty rules_to_propagate then
    (* if we can't propagate any further, terminate *)
    (current_snapshot, [])
  else if
    (* if we have been here before, terminate *)
    List.mem (history >>| Vertex.get_method) (Response.get_method new_fact) ~equal:Method.equal
  then (current_snapshot, history)
  else
    (* first, mark the current method's vertices as absolute *)
    let new_fact_vertices =
      G.this_method_vertices current_snapshot (Response.get_method new_fact)
    in
    let oracle_marked =
      List.fold new_fact_vertices
        ~f:(fun snapshot_acc vertex ->
          G.strong_update_dist vertex
            (DistManipulator.oracle_overwrite (Response.get_label new_fact))
            snapshot_acc )
        ~init:current_snapshot
    in
    (* do the propagation on current targets *)
    let propagated_snapshot, current_propagation_targets =
      List.fold
        ~f:(fun (snapshot_acc, affected_vertices) (rule : PropagationRules.t) ->
          let propagated_snapshot, this_affected =
            rule.rule snapshot_acc new_fact prev_facts ~dry_run:false
          in
          (propagated_snapshot, affected_vertices @ this_affected) )
        ~init:(oracle_marked, []) rules_to_propagate
    in
    List.fold current_propagation_targets
      ~f:(fun (big_acc, big_history) target ->
        if
          List.mem big_history target ~equal:Vertex.equal
          || List.mem new_fact_vertices target ~equal:Vertex.equal
        then (big_acc, big_history)
        else
          let target_meth, target_loc, target_dist = target in
          (* summarize this node's distribution into a Response.t! *)
          let target_rule_summary =
            Response.response_of_dist target_meth
              (G.lookup_dist_for_meth_and_loc target_meth target_loc propagated_snapshot)
          in
          let applicable_rules =
            MetaRules.ForPropagation.take_subset_of_applicable_propagation_rules current_snapshot
              target_rule_summary prev_facts prop_rule_pool
          in
          let propagated, updated_history =
            List.fold applicable_rules
              ~f:(fun (smol_acc, smol_history) prop_rule ->
                propagator target_rule_summary smol_acc (Some current_snapshot) applicable_rules
                  (target_rule_summary :: new_fact :: prev_facts)
                  smol_history prop_rule_pool )
              ~init:(big_acc, big_history)
          in
          (propagated, updated_history) )
      ~init:(propagated_snapshot, new_fact_vertices @ history)


let rec loop_inner (current_snapshot : G.t) (received_responses : Response.t list)
    (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) (count : int) : G.t =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then current_snapshot
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
        nodewise_featuremap
    in
    let question = question_maker.rule current_snapshot received_responses nodewise_featuremap in
    let prompt = Question.make_prompt question in
    Out_channel.output_string Out_channel.stdout prompt ;
    Out_channel.flush Out_channel.stdout ;
    let input = In_channel.input_line In_channel.stdin in
    if Option.is_none input || String.equal (Option.value_exn input) "stop" then current_snapshot
    else
      let response =
        match input with
        | Some response_str -> (
          match question with
          | AskingForLabel meth ->
              Response.response_of_string_forlabel meth response_str
          | AskingForConfirmation (meth, label) ->
              Response.response_of_string_foryesorno meth label response_str )
        | None ->
            failwith "no response ahahahah"
      in
      (* sort applicable Propagation Rules by adequacy. *)
      let propagation_rules_to_apply =
        MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
          received_responses
      in
      let propagated =
        fst
        @@ propagator response current_snapshot None propagation_rules_to_apply received_responses
             [] PropagationRules.all_rules
      in
      let propagated' = Axioms.apply_axioms propagated in
      Visualizer.visualize_snapshot propagated' ~micro:false ~autoopen:true ;
      (* G.serialize_to_bin propagated' ; *)
      (* G.snapshot_to_json propagated' ; *)
      loop_inner propagated' (response :: received_responses) nodewise_featuremap (count + 1)


let loop (current_snapshot : G.t) (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) =
  print_endline "Starting question-&-answer loop." ;
  G.snapshot_to_json current_snapshot ;
  loop_inner current_snapshot [] nodewise_featuremap 0
