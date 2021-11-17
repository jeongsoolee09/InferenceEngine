open ListMonad
open GraphRepr
open Probability

exception TODO

exception NotImplemented

module Random = Core_kernel.Random

module Response = struct
  type t = string * TaintLabel.t

  let get_method = fst

  let get_label = snd
end

module Question = struct
  type t = AskingForLabel of string | AskingForConfirmation of (string * TaintLabel.t)

  (** make a prompt message out of a question term. *)
  let make_prompt (question : t) : string =
    match question with
    | AskingForLabel meth ->
        F.asprintf "What label does %s bear? [src|sin|san|non]: " meth
    | AskingForConfirmation (meth, label) ->
        F.asprintf "Method %s is a(n) %s, right? [yes|no]: " meth (TaintLabel.to_string label)
end

module Utils = struct
  let random_select_elem (list : 'a list) : 'a =
    let random_index = Random.int_incl 0 (List.length list - 1) in
    List.nth_exn list random_index
end

(** Rules for propagating facts *)
module PropagationRules = struct
  (* rules have signature of ProbMap.t -> Response.t -> ProbMap.t. *)
  let contextual_similarity_rule (distmap : ProbMap.t) (new_fact : Response.t)
      (prev_facts : Response.t list) (graph : G.t) : ProbMap.t =
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _) -> String.equal meth new_fact_method)
    in
    let contextual_succs =
      new_fact_method_vertices >>= fun vertex -> Probability.Utils.cs_succs vertex graph
    in
    let contextual_succ_dist = contextual_succs >>| fun succ -> ProbMap.find succ distmap in
    List.fold
      ~f:(fun acc succ ->
        let succ_dist = ProbMap.find succ distmap in
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
        ProbMap.strong_update succ succ_dist acc )
      contextual_succs ~init:distmap


  (** Propagate the same info to nodes that are similar nodewise. *)
  let nodewise_similarity_propagation_rule (distmap : ProbMap.t) (new_fact : Response.t)
      (prev_facts : Response.t list) (graph : G.t) : ProbMap.t =
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _) -> String.equal meth new_fact_method)
    in
    let similarity_succs =
      new_fact_method_vertices >>= fun vertex -> Probability.Utils.ns_succs vertex graph
    in
    let similarity_succ_dist = similarity_succs >>| fun succ -> ProbMap.find succ distmap in
    List.fold
      ~f:(fun acc succ ->
        let succ_dist = ProbMap.find succ distmap in
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
        ProbMap.strong_update succ succ_dist acc )
      similarity_succs ~init:distmap


  let annotation_rule (distmap : ProbMap.t) (new_fact : Response.t) (prev_facts : Response.t list)
      (graph : G.t) : ProbMap.t =
    raise TODO


  let all_rules = [contextual_similarity_rule; nodewise_similarity_propagation_rule; annotation_rule]
end

(* Use Random.int_incl for making a random integer. *)

module AskingRules = struct
  let ask_if_leaf_is_sink (graph : G.t) (asked : string) nfeaturemap cfeaturemap : Question.t =
    (* TODO: consider featuremaps *)
    let all_leaves = G.collect_leaves graph in
    let random_leaf =
      let random_index = Random.int_incl 0 (List.length all_leaves - 1) in
      List.nth_exn all_leaves random_index
    in
    Question.AskingForConfirmation (fst random_leaf, TaintLabel.Sink)


  let ask_if_root_is_source (graph : G.t) (asked : string) nfeaturemap cfeaturemap : Question.t =
    (* TODO consider featuremaps *)
    let all_roots = G.collect_roots graph in
    let random_root =
      let random_index = Random.int_incl 0 (List.length all_roots - 1) in
      List.nth_exn all_roots random_index
    in
    Question.AskingForConfirmation (fst random_root, TaintLabel.Source)


  (** ask a method from a foreign package of its label. *)
  let ask_foreign_package_label (graph : G.t) (asked : string) nfeaturemap cfeaturemap : Question.t
      =
    let all_foreign_package_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if NodeWiseFeatures.Predicates.is_framework_code (fst vertex) then vertex :: acc else acc
          )
        graph []
    in
    let random_foreign_vertex = Utils.random_select_elem all_foreign_package_vertices in
    Question.AskingForLabel (fst random_foreign_vertex)


  let all_rules = [ask_if_leaf_is_sink; ask_if_root_is_source; ask_foreign_package_label]
end

module MetaRules = struct
  let assign_priority_on_propagation_rules prop_rules (graph : G.t) =
    (* TEMP *)
    List.map ~f:(fun rule -> (rule, 1)) prop_rules


  let assign_priority_on_asking_rules asking_rules (graph : G.t) =
    (* TEMP *)
    List.map ~f:(fun rule -> (rule, 1)) asking_rules
end
