open GraphRepr

(** (1) use the received propagation rule, and (2) spawn itself to the propagation targets. *)
let rec propagator (new_fact : Response.t) (current_snapshot : G.t)
    (rules_to_propagate : PropagationRules.t array) (prev_facts : Response.t list)
    (history : Vertex.t array) (prop_rule_pool : PropagationRules.t array) : G.t * Vertex.t array =
  (* First, mark the current method's vertices as absolute *)
  let new_fact_vertices =
    G.this_method_vertices_array current_snapshot (Response.get_method new_fact)
  in
  let oracle_marked =
    Array.fold new_fact_vertices
      ~f:(fun snapshot_acc vertex ->
        G.strong_update_dist vertex
          (DistManipulator.oracle_overwrite (Response.get_label new_fact))
          snapshot_acc )
      ~init:current_snapshot
  in
  (* Next, do the propagation on current targets *)
  let propagated_snapshot, current_propagation_targets =
    Array.fold
      ~f:(fun (snapshot_acc, affected_vertices) rule ->
        let propagated_snapshot, this_affected = rule.rule snapshot_acc new_fact ~dry_run:false in
        (propagated_snapshot, affected_vertices @ this_affected) )
      ~init:(oracle_marked, []) rules_to_propagate
  in
  Array.fold
    (Array.of_list current_propagation_targets)
    ~f:(fun (acc, history) target ->
      if
        Array.mem history target ~equal:Vertex.equal
        || Array.mem new_fact_vertices target ~equal:Vertex.equal
      then (acc, history)
      else
        let target_meth = Vertex.get_method target and target_loc = Vertex.get_loc target in
        (* summarize this node's distribution into a Response.t *)
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
    ~init:(propagated_snapshot, Array.append new_fact_vertices history)
