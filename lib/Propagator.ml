open GraphRepr
open ListMonad
open RulesOfInference

(** (1) receive a rule to propagate, (2) use that propagation rule, and (3) spawn itself to the
    propagation targets. *)
let rec propagator (new_fact : Response.t) (current_snapshot : G.t)
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
          let propagated_snapshot, this_affected = rule.rule snapshot_acc new_fact ~dry_run:false in
          (propagated_snapshot, affected_vertices @ this_affected) )
        ~init:(oracle_marked, []) rules_to_propagate
    in
    List.fold current_propagation_targets
      ~f:(fun (acc, history) target ->
        if
          List.mem history target ~equal:Vertex.equal
          || List.mem new_fact_vertices target ~equal:Vertex.equal
        then (acc, history)
        else
          let target_meth = Vertex.get_method target and target_loc = Vertex.get_loc target in
          (* summarize this node's distribution into a Response.t! *)
          let target_rule_summary =
            Response.response_of_dist target_meth
              (G.lookup_dist_for_meth_and_loc target_meth target_loc propagated_snapshot)
          in
          let applicable_rules =
            MetaRules.ForPropagation.take_subset_of_applicable_propagation_rules current_snapshot
              target_rule_summary prop_rule_pool
          in
          let propagated, updated_history =
            propagator target_rule_summary acc applicable_rules
              (target_rule_summary :: new_fact :: prev_facts)
              history prop_rule_pool
          in
          (propagated, updated_history) )
      ~init:(propagated_snapshot, new_fact_vertices @ history)
