open ListMonad
open GraphRepr
open Probability

exception TODO

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
    raise TODO


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
    match new_fact_label with
    | Source ->
        raise TODO
    | Sink ->
        raise TODO
    | Sanitizer ->
        raise TODO
    | None ->
        raise TODO
    | Indeterminate ->
        failwith "Impossible"


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
