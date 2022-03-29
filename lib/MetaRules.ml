open InfixOperators
open ListMonad
open GraphRepr

(** the priority of propagation rules represent their application order. the smallest number
    represents the highest priority. *)
module ForPropagation = struct
  let take_subset_of_applicable_propagation_rules (graph : G.t) (new_fact : Response.t)
      (prop_rules : PropagationRules.t array) : PropagationRules.t array =
    (* rule R is applicable to vertex V <=> (def) V has successor with labeled edge required by rule R
                                        <=> (def) the embedded assertion succeeds *)
    let out =
      Array.of_list
      @@ Array.fold
           ~f:(fun acc prop_rule ->
             try
               ignore @@ prop_rule.rule graph new_fact ~dry_run:true ;
               prop_rule :: acc
             with Assert_failure _ -> acc )
           ~init:[] prop_rules
    in
    Array.rev_inplace out ;
    (* Array.iter out ~f:(fun rule -> print_endline @@ F.asprintf "Prop rule %s is chosen." rule.label) ; *)
    out


  let assign_priority_on_propagation_rules (prop_rules : PropagationRules.t array) =
    (* TODO static for now, but could be dynamic *)
    let open PropagationRules in
    Array.map
      ~f:(fun prop_rule ->
        match prop_rule.label with
        | "contextual_similarity_rule" ->
            (prop_rule, 0)
        | "nodewise_similarity_propagation_rule" ->
            (prop_rule, 2)
        | "annotation_rule" ->
            (prop_rule, 4)
        | "mark_api_based_on_relative_position_in_its_trunk" ->
            (prop_rule, 3)
        | otherwise ->
            failwithf "%s is not covered!!" otherwise () )
      prop_rules


  let sort_propagation_rules_by_priority graph new_fact : PropagationRules.t array =
    let priority_assigned =
      assign_priority_on_propagation_rules
        (take_subset_of_applicable_propagation_rules graph new_fact PropagationRules.all_rules)
    in
    Array.sort
      ~compare:(fun (_, priority1) (_, priority2) -> -Int.compare priority1 priority2)
      priority_assigned ;
    Array.map ~f:fst priority_assigned
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
        (asking_rule, 3)
    | "ask_indeterminate" ->
        (asking_rule, 2)
    | "ask_from_ns_api_cluster" ->
        (asking_rule, 5)
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
