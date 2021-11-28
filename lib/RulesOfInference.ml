open InfixOperators
open ListMonad
open GraphRepr

exception TODO

exception NotImplemented

module Random = Core_kernel.Random

module Question = struct
  type t = AskingForLabel of string | AskingForConfirmation of (string * TaintLabel.t)

  (** make a prompt message out of a question term. *)
  let make_prompt (question : t) : string =
    match question with
    | AskingForLabel meth ->
        F.asprintf "What label does %s bear? [src|sin|san|non]: " meth
    | AskingForConfirmation (meth, label) ->
        F.asprintf "Method %s is a %s, right? [yes|no]: " meth (TaintLabel.to_string label)


  let get_method (question : t) : string =
    match question with AskingForLabel meth -> meth | AskingForConfirmation (meth, _) -> meth


  let get_label (question : t) : TaintLabel.t =
    match question with
    | AskingForConfirmation (_, label) ->
        label
    | _ ->
        raise @@ Invalid_argument "hahaha"
end

module Response = struct
  type t = ForLabel of (string * TaintLabel.t) | ForYesOrNo of (string * TaintLabel.t * bool)

  let to_string (response : t) : string =
    match response with
    | ForLabel (meth_, label) ->
        F.asprintf "ForLabel (%s, %s)" meth_ (TaintLabel.to_string label)
    | ForYesOrNo (meth_, label, bool) ->
        F.asprintf "ForYesOrNo (%s, %s, %s)" meth_ (TaintLabel.to_string label)
          (Bool.to_string bool)


  let get_method (res : t) : string =
    match res with ForLabel (meth, _) -> meth | ForYesOrNo (meth, _, _) -> meth


  let get_label (res : t) : TaintLabel.t =
    match res with
    | ForLabel (_, label) ->
        label
    | _ ->
        raise @@ Invalid_argument "this is not a response of a yes/no question"


  let get_yesorno (res : t) : bool =
    match res with
    | ForYesOrNo (_, _, bool) ->
        bool
    | _ ->
        raise @@ Invalid_argument "this is not a response of a question asking for label"


  let response_of_dist (method_ : string) (dist : ProbQuadruple.t) : t =
    let label = ProbQuadruple.determine_label dist in
    ForLabel (method_, label)


  let response_of_string_forlabel (method_ : string) (response_str : string) : t =
    match response_str with
    | "src" | "source" ->
        ForLabel (method_, TaintLabel.Source)
    | "sin" | "sink" ->
        ForLabel (method_, TaintLabel.Sink)
    | "san" | "sanitizer" ->
        ForLabel (method_, TaintLabel.Sanitizer)
    | "non" | "none" ->
        ForLabel (method_, TaintLabel.None)
    | otherwise ->
        raise @@ Invalid_argument otherwise


  let response_of_string_foryesorno (method_ : string) (label : TaintLabel.t) (response_str : string)
      : t =
    match response_str with
    | "yes" | "y" ->
        ForYesOrNo (method_, label, true)
    | "no" | "n" ->
        ForYesOrNo (method_, label, false)
    | otherwise ->
        raise @@ Invalid_argument otherwise
end

module Utils = struct
  let random_select_elem (list : 'a list) : 'a =
    let random_index = Random.int_incl 0 (List.length list - 1) in
    List.nth_exn list random_index
end

(** Rules for propagating facts *)
module PropagationRules = struct
  type t = G.t -> Response.t -> Response.t list -> G.t * Vertex.t list

  let is_internal_udf_vertex vertex graph =
    let data_flows_in = Int.( >= ) (List.length @@ G.df_preds vertex graph) 1
    and data_flows_out = Int.( >= ) (List.length @@ G.df_succs vertex graph) 1
    and is_udf =
      let meth = fst3 vertex in
      let all_udfs = Deserializer.deserialize_method_txt () in
      List.mem ~equal:String.equal all_udfs meth
    in
    data_flows_in && data_flows_out && is_udf


  let internal_udf_vertex_is_none : t =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ->
    Out_channel.output_string Out_channel.stdout "internal_udf_vertex_is_none chosen" ;
    Out_channel.newline Out_channel.stdout ;
    let this_method = Response.get_method new_fact in
    let this_method_vertices = G.this_method_vertices graph this_method in
    let this_method_trunks = this_method_vertices >>= find_trunks_containing_vertex graph in
    List.fold
      ~f:(fun big_acc trunk ->
        List.fold
          ~f:(fun (graph_acc, affected_vertices) trunk_vertex ->
            let succ_dist = trd3 trunk_vertex in
            let new_dist =
              { ProbQuadruple.src= succ_dist.src -. 0.1
              ; ProbQuadruple.sin= succ_dist.sin -. 0.1
              ; ProbQuadruple.san= succ_dist.san -. 0.1
              ; ProbQuadruple.non= succ_dist.non +. 0.3 }
            in
            (G.strong_update_dist trunk_vertex new_dist graph_acc, trunk_vertex :: affected_vertices)
            )
          ~init:big_acc trunk )
      ~init:(graph, []) this_method_trunks


  (** propagating to contextually similar vertices: requires that the new_fact's method have
      successors with contextual similarity edge *)
  let contextual_similarity_rule : t =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ->
    Out_channel.output_string Out_channel.stdout "contextual_similarity_rule chosen" ;
    Out_channel.newline Out_channel.stdout ;
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _, _) -> String.equal meth new_fact_method)
    in
    let contextual_succs = new_fact_method_vertices >>= fun vertex -> G.cs_succs vertex graph in
    assert (Int.( >= ) (List.length contextual_succs) 1) ;
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = trd3 succ in
          let new_dist =
            match new_fact_label with
            | Source ->
                { ProbQuadruple.src= succ_dist.src +. 0.3
                ; sin= succ_dist.sin -. 0.1
                ; san= succ_dist.san -. 0.1
                ; non= succ_dist.non -. 0.1 }
            | Sink ->
                { ProbQuadruple.src= succ_dist.src -. 0.1
                ; sin= succ_dist.sin +. 0.3
                ; san= succ_dist.san -. 0.1
                ; non= succ_dist.non -. 0.1 }
            | Sanitizer ->
                { ProbQuadruple.src= succ_dist.src -. 0.1
                ; sin= succ_dist.sin -. 0.1
                ; san= succ_dist.san +. 0.3
                ; non= succ_dist.non -. 0.1 }
            | None ->
                raise NotImplemented
            | Indeterminate ->
                failwith "Impossible"
          in
          G.strong_update_dist succ new_dist acc )
        contextual_succs ~init:graph
    in
    (propagated, contextual_succs)


  (** Propagate the same info to nodes that are similar nodewise: requires that the new_fact's
      method have successors with nodewise simlarity edge *)
  let nodewise_similarity_propagation_rule : t =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ->
    Out_channel.output_string Out_channel.stdout "nodewise_similarity_propagation_rule chosen" ;
    Out_channel.newline Out_channel.stdout ;
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _, _) -> String.equal meth new_fact_method)
    in
    let similarity_succs = new_fact_method_vertices >>= fun vertex -> G.ns_succs vertex graph in
    assert (Int.( >= ) (List.length similarity_succs) 1) ;
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = trd3 succ in
          let new_dist =
            match new_fact_label with
            | Source ->
                if G.is_root succ graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_leaf succ graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non +. 0.3 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
            | Sink ->
                if G.is_root succ graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_leaf succ graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non +. 0.3 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
            | Sanitizer ->
                if G.is_root succ graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_leaf succ graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non +. 0.3 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san +. 0.3
                  ; non= succ_dist.non -. 0.1 }
            | None ->
                if G.is_root succ graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_leaf succ graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non +. 0.3 }
                else
                  (* bump the likelihood of the successor being a source *)
                  (* { ProbQuadruple.src= succ_dist.src -. 0.1 *)
                  (* ; sin= succ_dist.sin -. 0.1 *)
                  (* ; san= succ_dist.san -. 0.1 *)
                  (* ; non= succ_dist.non +. 0.3 } *)
                  raise NotImplemented
            | Indeterminate ->
                failwith "Impossible"
          in
          G.strong_update_dist succ new_dist acc )
        similarity_succs ~init:graph
    in
    (propagated, similarity_succs)


  (** Propagate the same info to nodes with the same @annotations: requires that the new_fact's
      method have successors with nodewise simlarity edge bearing the same @annotation *)

  (* let annotation_rule : t = *)
  (*  fun (distmap : ProbMap.t) (new_fact : Response.t) (prev_facts : Response.t list) (graph : G.t) -> *)
  (*   (\* assert that there is at least one successor with the same annotation. *\) *)
  (*   raise TODO *)

  let all_rules =
    [contextual_similarity_rule; nodewise_similarity_propagation_rule (* ; annotation_rule *)]
end

(* Use Random.int_incl for making a random integer. *)

module AskingRules = struct
  (* NOTE we might use a curried type to make this def simpler. *)
  type t = G.t -> Response.t list -> FeatureMaps.NodeWiseFeatureMap.t -> Question.t

  let ask_if_leaf_is_sink (graph : G.t) (received_responses : Response.t list)
      (nfeaturemap : FeatureMaps.NodeWiseFeatureMap.t) : Question.t =
    (* TODO: consider featuremaps *)
    let all_leaves = G.collect_leaves graph in
    let random_leaf =
      (* Utils.random_select_elem all_leaves *)
      (* TEMP Hardcoded *)
      ("void PrintStream.println(String)", "{ line 43 }", ProbQuadruple.initial)
    in
    (* Question.AskingForConfirmation (fst3 random_leaf, TaintLabel.Sink) *)
    Question.AskingForLabel (fst3 random_leaf)


  let ask_if_root_is_source (graph : G.t) (received_responses : Response.t list)
      (nfeaturemap : FeatureMaps.NodeWiseFeatureMap.t) : Question.t =
    (* TODO consider featuremaps *)
    let all_roots = G.collect_roots graph in
    let random_root =
      let random_index = Random.int_incl 0 (List.length all_roots - 1) in
      List.nth_exn all_roots random_index
    in
    (* Question.AskingForConfirmation (fst3 random_root, TaintLabel.Source) *)
    Question.AskingForLabel (fst3 random_root)


  (** ask a method from a foreign package of its label. *)
  let ask_foreign_package_label (graph : G.t) (received_responses : Response.t list)
      (nfeaturemap : FeatureMaps.NodeWiseFeatureMap.t) : Question.t =
    let all_foreign_package_vertices =
      G.fold_vertex
        (fun vertex acc ->
          let open NodeWiseFeatures in
          if SingleFeature.bool_of_feature @@ SingleFeature.is_framework_code (fst3 vertex) then
            vertex :: acc
          else acc )
        graph []
    in
    let random_foreign_vertex = Utils.random_select_elem all_foreign_package_vertices in
    Question.AskingForLabel (fst3 random_foreign_vertex)


  let all_rules = [ask_if_leaf_is_sink; ask_if_root_is_source; ask_foreign_package_label]
end

module MetaRules = struct
  (** the priority of propagation rules represent their application order. the smallest number
      represents the highest priority. *)
  module ForPropagation = struct
    let take_subset_of_applicable_propagation_rules (graph : G.t) (new_fact : Response.t)
        (prev_facts : Response.t list) (prop_rules : PropagationRules.t list) :
        PropagationRules.t list =
      (* rule R is applicable to vertex V iff (def) V has successor with labeled edge required by rule R
                                          iff (def) the embedded assertion succeeds *)
      List.rev
      @@ List.fold
           ~f:(fun acc prop_rule ->
             try
               (* try applying a prop_rule *)
               let _ = prop_rule graph new_fact prev_facts in
               prop_rule :: acc
             with Assert_failure _ -> acc )
           ~init:[] prop_rules


    (** main logic of this submodule. *)
    let assign_priority_on_propagation_rules prop_rules (graph : G.t) =
      List.map ~f:(fun rule -> (rule, 1)) prop_rules


    let sort_propagation_rules_by_priority graph new_fact responses : PropagationRules.t list =
      let priority_assigned =
        assign_priority_on_propagation_rules
          (take_subset_of_applicable_propagation_rules graph new_fact responses
             PropagationRules.all_rules )
          graph
      in
      List.map ~f:fst
      @@ List.sort
           ~compare:(fun (_, priority1) (_, priority2) -> Int.compare priority1 priority2)
           priority_assigned
  end

  (** the priority of asking rules represent their current adequacy of application. *)
  module ForAsking = struct
    let take_subset_of_applicable_asking_rules graph responses nfeaturemap asking_rules =
      (* rule R is applicable to vertex V iff (def) V has successor with labeled edge required by rule R
                                          iff (def) the embedded assertion succeeds *)
      List.rev
      @@ List.fold
           ~f:(fun acc asking_rule ->
             try
               let _ = asking_rule graph responses nfeaturemap in
               asking_rule :: acc
             with Assert_failure _ -> acc )
           ~init:[] asking_rules


    (** main logic of this submodule. *)
    let assign_priority_on_asking_rules asking_rules (graph : G.t) =
      (* TEMP *)
      List.map ~f:(fun rule -> (rule, 1)) asking_rules


    (** choose the most applicable asking rule. *)
    let asking_rules_selector (graph : G.t) responses nfeaturemap : AskingRules.t =
      let priority_assigned =
        assign_priority_on_asking_rules
          (take_subset_of_applicable_asking_rules graph responses nfeaturemap AskingRules.all_rules)
          graph
      in
      (* sort the asking rules by the priority, and get the first one. *)
      fst @@ List.hd_exn
      @@ List.sort
           ~compare:(fun (_, priority1) (_, priority2) -> Int.compare priority1 priority2)
           priority_assigned
  end
end

(** (1) receive a rule to propagate, (2) use that propagation rule, and (3) spawn itself to the
    propagation targets. *)
let rec propagator (new_fact : Response.t) (graph : G.t)
    (rules_to_propagate : PropagationRules.t list) (prev_facts : Response.t list)
    (prop_rule_pool : PropagationRules.t list) (history : G.V.t list) : G.t =
  Out_channel.output_string Out_channel.stdout
    (F.asprintf "propagator is called on %s" (Response.to_string new_fact)) ;
  Out_channel.newline Out_channel.stdout ;
  if List.is_empty rules_to_propagate then graph
  else
    (* if we can't propagate any further, terminate *)
    let current_propagated_distmap, current_propagation_targets =
      List.fold
        ~f:(fun (distmap_acc, affected_vertices) (rule : PropagationRules.t) ->
          let propagated_distmap, this_affected = rule graph new_fact prev_facts in
          (propagated_distmap, affected_vertices @ this_affected) )
        ~init:(graph, []) rules_to_propagate
    in
    let current_propagation_targets_no_again =
      List.filter
        ~f:(fun target -> not @@ List.mem ~equal:Vertex.equal history target)
        current_propagation_targets
    in
    List.fold
      ~f:(fun big_acc target ->
        let target_dist = trd3 target in
        (* summarize this node's distribution into a Response.t! *)
        let target_rule_summary = Response.response_of_dist (fst3 target) target_dist in
        let applicable_rules =
          MetaRules.ForPropagation.take_subset_of_applicable_propagation_rules graph
            target_rule_summary prev_facts prop_rule_pool
        in
        List.fold
          ~f:(fun smol_acc prop_rule ->
            propagator target_rule_summary smol_acc applicable_rules (new_fact :: prev_facts)
              prop_rule_pool (target :: history) )
          ~init:big_acc applicable_rules )
      ~init:graph current_propagation_targets_no_again
