open ListMonad
open GraphRepr
open Probability

exception TODO

exception NotImplemented

module Response = struct
  type t = string * TaintLabel.t

  let get_method = fst

  let get_label = snd
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
    let contextual_succs = new_fact_method_vertices >>= fun vertex -> Utils.cs_succs vertex graph in
    let contextual_succ_dist = contextual_succs >>| fun succ -> ProbMap.find succ distmap in
    List.fold
      ~f:(fun acc succ ->
        let succ_dist = ProbMap.find succ distmap in
        let new_dist =
          match new_fact_label with
          | Source | Sink | Sanitizer ->
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
    let similarity_succs = new_fact_method_vertices >>= fun vertex -> Utils.ns_succs vertex graph in
    let similarity_succ_dist = similarity_succs >>| fun succ -> ProbMap.find succ distmap in
    List.fold
      ~f:(fun acc succ ->
        let succ_dist = ProbMap.find succ distmap in
        let new_dist =
          match new_fact_label with
          | Source ->
              (* bump the likelihood of the successor being a source *)
              { ProbQuadruple.src= succ_dist.src +. 0.3
              ; sin= succ_dist.sin -. 0.1
              ; san= succ_dist.san -. 0.1
              ; non= succ_dist.non -. 0.1 }
          | Sink ->
              (* bump the likelihood of the successor being a source *)
              { ProbQuadruple.src= succ_dist.src -. 0.1
              ; sin= succ_dist.sin +. 0.3
              ; san= succ_dist.san -. 0.1
              ; non= succ_dist.non -. 0.1 }
          | Sanitizer ->
              (* bump the likelihood of the successor being a source *)
              { ProbQuadruple.src= succ_dist.src -. 0.1
              ; sin= succ_dist.sin -. 0.1
              ; san= succ_dist.san +. 0.3
              ; non= succ_dist.non -. 0.1 }
          | None ->
              (* bump the likelihood of the successor being a source *)
              { ProbQuadruple.src= succ_dist.src -. 0.1
              ; sin= succ_dist.sin -. 0.1
              ; san= succ_dist.san -. 0.1
              ; non= succ_dist.non +. 0.3 }
          | Indeterminate ->
              failwith "Impossible"
        in
        ProbMap.strong_update succ succ_dist acc )
      similarity_succs ~init:distmap


  let annotation_rule (distmap : ProbMap.t) (new_fact : Response.t) (prev_facts : Response.t list)
      (graph : G.t) : ProbMap.t =
    raise TODO
end

module AskingRules = struct
  let ask_if_leaf_is_sink = raise TODO

  let ask_if_root_is_source = raise TODO

  (** ask a method from a foreign package of its label. *)
  let ask_foreign_package_label = raise TODO
end
