open InfixOperators
open ListMonad
open GraphRepr

(** the priority of propagation rules represent their application order. the smallest number
    represents the highest priority. *)
module ForPropagation = struct
  let take_subset_of_applicable_propagation_rules (graph : G.t) (new_fact : Response.t)
      (prop_rules : PropagationRules.t list) : PropagationRules.t list =
    (* rule R is applicable to vertex V <=> (def) V has successor with labeled edge required by rule R
                                        <=> (def) the embedded assertion succeeds *)
    List.rev
    @@ List.fold
         ~f:(fun acc prop_rule ->
           try
             ignore @@ prop_rule.rule graph new_fact ~dry_run:true ;
             prop_rule :: acc
           with Assert_failure _ -> acc )
         ~init:[] prop_rules


  let assign_priority_on_propagation_rules prop_rules =
    (* TODO static for now, but could be dynamic *)
    List.map ~f:(fun rule -> (rule, 1)) prop_rules


  let sort_propagation_rules_by_priority graph new_fact : PropagationRules.t list =
    let priority_assigned =
      assign_priority_on_propagation_rules
        (take_subset_of_applicable_propagation_rules graph new_fact PropagationRules.all_rules)
    in
    List.map ~f:fst
    @@ List.sort
         ~compare:(fun (_, priority1) (_, priority2) -> -Int.compare priority1 priority2)
         priority_assigned
end

(** the priority of asking rules represent their current adequacy of application. *)
module ForAsking = struct
  let take_subset_of_applicable_asking_rules (snapshot : G.t) (responses : Response.t list)
      (asking_rules : AskingRules.t list) =
    (* rule R is applicable to vertex V <=> (def) V has successor with labeled edge required by rule R
                                        <=> (def) the embedded assertion succeeds *)
    List.rev
    @@ List.fold
         ~f:(fun acc asking_rule ->
           try
             ignore @@ asking_rule.rule snapshot responses ~dry_run:true ;
             asking_rule :: acc
           with Assert_failure _ -> acc )
         ~init:[] asking_rules


  let assign_priority_on_asking_rules (asking_rules : AskingRules.t list) :
      (AskingRules.t * int) list =
    let open AskingRules in
    asking_rules
    >>| fun asking_rule ->
    match asking_rule.label with
    | "ask_if_leaf_is_sink" ->
        (asking_rule, 2)
    | "ask_if_root_is_source" ->
        (asking_rule, 3)
    | "ask_foreign_package_label" ->
        (asking_rule, 4)
    | "ask_indeterminate" ->
        (asking_rule, 5)
    | "ask_from_ns_cluster_if_it_contains_internal_src_or_sink" ->
        (asking_rule, 2)
    | "ask_annotated_method" ->
        (asking_rule, 6)
    | otherwise ->
        failwith (otherwise ^ " is not covered!")


  (** choose the most applicable asking rule. *)
  let asking_rules_selector (graph : G.t) (responses : Response.t list) : AskingRules.t =
    let priority_assigned =
      assign_priority_on_asking_rules
        (take_subset_of_applicable_asking_rules graph responses AskingRules.all_rules)
    in
    (* sort the asking rules by the priority, and get the first one. *)
    let out =
      fst @@ List.hd_exn
      @@ List.sort
           ~compare:(fun (_, priority1) (_, priority2) -> -Int.compare priority1 priority2)
           priority_assigned
    in
    print_endline @@ F.asprintf "%s chosen." out.label ;
    out
end
