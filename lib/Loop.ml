open GraphRepr
open ListMonad
open RulesOfInference
open FeatureMaps

exception TODO

module In_channel = Core_kernel.In_channel
module Out_channel = Core_kernel.Out_channel

module Visualizer = struct
  (** (1) output a dot file of this snapshot, (2) render a svg off the dot file, and (3) show the
      svg file. *)
  let visualize_snapshot (snapshot : G.t) ~(micro : bool) ~(autoopen : bool) : unit =
    let open GraphMaker in
    let now_timestring = make_now_string 9 in
    let filename_without_extension =
      if micro then F.asprintf "%s_micro" now_timestring else now_timestring
    in
    graph_to_dot snapshot ~filename:(filename_without_extension ^ ".dot") ;
    let dot_in_chan, dot_out_chan =
      Unix.open_process
        (F.asprintf "dot -Tsvg -o %s.svg %s.dot" filename_without_extension
           filename_without_extension )
    in
    In_channel.close dot_in_chan ;
    Out_channel.close dot_out_chan ;
    if autoopen then (
      Unix.sleep 3 ;
      let open_in_chan, open_out_chan =
        Unix.open_process (F.asprintf "open %s.svg" filename_without_extension)
      in
      In_channel.close open_in_chan ;
      Out_channel.close open_out_chan )
end

(** (1) receive a rule to propagate, (2) use that propagation rule, and (3) spawn itself to the
    propagation targets. *)
let rec propagator (new_fact : Response.t) (current_snapshot : G.t) (previous_snapshot : G.t option)
    (rules_to_propagate : PropagationRules.t list) (prev_facts : Response.t list)
    (history : Vertex.t list) (prop_rule_pool : PropagationRules.t list) : G.t * Vertex.t list =
  if List.is_empty rules_to_propagate then
    (* if we can't propagate any further, terminate *)
    (current_snapshot, [])
  else if List.mem (history >>| fst3) (Response.get_method new_fact) ~equal:Method.equal then
    (current_snapshot, history)
  else (
    Out_channel.print_endline "==============================" ;
    Out_channel.print_endline
      (F.asprintf "propagator is propagating on %s" (Response.to_string new_fact)) ;
    let current_visiting_vertices =
      G.this_method_vertices current_snapshot (Response.get_method new_fact)
    in
    (* Out_channel.print_endline *)
    (* @@ F.asprintf "current_visiting_vertices: %s" *)
    (*      (Vertex.vertex_list_to_string current_visiting_vertices) ; *)
    (* do the propagation *)
    let propagated_snapshot, current_propagation_targets =
      List.fold
        ~f:(fun (snapshot_acc, affected_vertices) (rule : PropagationRules.t) ->
          let propagated, this_affected =
            let out = rule.rule snapshot_acc new_fact prev_facts ~dry_run:false in
            Visualizer.visualize_snapshot (fst out) ~micro:true ~autoopen:false ;
            out
          in
          (propagated, affected_vertices @ this_affected) )
        ~init:(current_snapshot, []) rules_to_propagate
    in
    let out =
      List.fold
        ~f:(fun (big_acc, big_history) target ->
          if
            List.mem big_history target ~equal:Vertex.equal
            || List.mem ~equal:Vertex.equal current_visiting_vertices target
          then (big_acc, big_history)
          else (
            Out_channel.print_endline
            @@ F.asprintf "\npropagator is iterating on %s" (Vertex.to_string target) ;
            if
              List.mem big_history target ~equal:Vertex.equal
              || List.mem ~equal:Vertex.equal current_visiting_vertices target
            then (big_acc, big_history)
            else
              let target_meth, target_loc, target_dist = target in
              (* summarize this node's distribution into a Response.t! *)
              let target_rule_summary =
                Response.response_of_dist (fst3 target)
                  (G.lookup_dist_for_meth_and_loc target_meth target_loc propagated_snapshot)
              in
              (* Out_channel.print_endline *)
              (* @@ F.asprintf "\ntarget_rule_summary of %s: %s, dist: %s\n" (fst3 target) *)
              (*      (Response.to_string target_rule_summary) *)
              (*      (ProbQuadruple.to_string *)
              (*         (G.lookup_dist_for_meth_and_loc target_meth target_loc propagated_snapshot) ) ; *)
              let applicable_rules =
                MetaRules.ForPropagation.take_subset_of_applicable_propagation_rules
                  current_snapshot target_rule_summary prev_facts prop_rule_pool
              in
              let propagated, updated_history =
                List.fold
                  ~f:(fun (smol_acc, smol_history) prop_rule ->
                    let out =
                      propagator target_rule_summary smol_acc (Some current_snapshot)
                        applicable_rules
                        (target_rule_summary :: new_fact :: prev_facts)
                        smol_history prop_rule_pool
                    in
                    out )
                  ~init:(big_acc, big_history) applicable_rules
              in
              if Option.is_some previous_snapshot then
                G.print_snapshot_diff (Option.value_exn previous_snapshot) propagated ;
              (propagated, updated_history) ) )
        ~init:(propagated_snapshot, current_visiting_vertices @ history)
        current_propagation_targets
    in
    out )


let rec loop (current_snapshot : G.t) (received_responses : Response.t list)
    (nodewise_featuremap : FeatureMaps.NodeWiseFeatureMap.t) (count : int) : G.t =
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
        List.fold
          ~f:(fun acc prop_rule ->
            fst
            @@ propagator response acc None propagation_rules_to_apply received_responses []
                 PropagationRules.all_rules )
          ~init:current_snapshot propagation_rules_to_apply
      in
      let propagated' = SelfHeal.HealMisPropagation.heal_all propagated in
      Visualizer.visualize_snapshot propagated' ~micro:false ~autoopen:true ;
      G.serialize_to_bin propagated' ;
      loop propagated' (response :: received_responses) nodewise_featuremap (count + 1)
