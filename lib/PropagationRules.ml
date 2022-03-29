(** Rules for propagating facts *)

open ListMonad
open InfixOperators
open GraphRepr
open Utils
module LV = G.LiteralVertex

type rule = G.t -> Response.t -> dry_run:bool -> G.t * Vertex.t list

type t = {rule: rule; label: string}

(** propagating to contextually similar vertices: requires that the new_fact's method have
    successors with contextual similarity edge *)
let contextual_similarity_rule : rule =
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  let new_fact_method = Response.get_method new_fact
  and new_fact_label = Response.get_label new_fact in
  let new_fact_method_vertices =
    List.filter (G.all_vertices_of_graph graph)
      ~f:(Vertex.get_method >> Method.equal new_fact_method)
  in
  let contextual_succs =
    new_fact_method_vertices >>= (LV.of_vertex >> G.get_succs graph ~label:ContextualSimilarity)
  in
  assert (Int.( >= ) (List.length contextual_succs) 1) ;
  if dry_run then (graph, [])
  else
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = Vertex.get_dist succ in
          let is_inside_ns_cluster_containing_df_internals =
            let containing_cluster_opt =
              Array.find
                (Array.map ~f:G.all_vertices_of_graph (SimilarityHandler.all_ns_clusters graph))
                ~f:(fun cluster -> List.mem cluster succ ~equal:Vertex.equal)
            in
            match containing_cluster_opt with
            | None ->
                false
            | Some containing_cluster ->
                List.exists containing_cluster ~f:(fun vertex ->
                    G.is_df_internal graph (LV.of_vertex vertex) )
          in
          let new_dist =
            match new_fact_label with
            | Source | Sink | Sanitizer | None ->
                if is_inside_ns_cluster_containing_df_internals then succ_dist
                else DistManipulator.bump succ_dist [new_fact_label] ~inc_delta:10. ~dec_delta:5.
            | Indeterminate ->
                succ_dist
          in
          G.strong_update_dist succ new_dist acc )
        contextual_succs ~init:graph
    in
    (propagated, contextual_succs)


(** Propagate the same info to nodes that are similar nodewise: requires that the new_fact's method
    have successors with nodewise simlarity edge *)
let nodewise_similarity_propagation_rule : rule =
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  let new_fact_method = Response.get_method new_fact
  and new_fact_label = Response.get_label new_fact in
  let new_fact_method_vertices =
    List.filter (G.all_vertices_of_graph graph)
      ~f:(Vertex.get_method >> Method.equal new_fact_method)
  in
  let similarity_succs =
    let raw_succs =
      new_fact_method_vertices >>= (LV.of_vertex >> G.get_succs graph ~label:NodeWiseSimilarity)
    in
    let module VertexSet = Caml.Set.Make (Vertex) in
    raw_succs |> VertexSet.of_list |> VertexSet.elements
  in
  assert (Int.( >= ) (List.length similarity_succs) 1) ;
  if dry_run then (graph, [])
  else
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = Vertex.get_dist succ in
          let new_dist =
            match new_fact_label with
            | Source ->
                if Trunk.vertex_is_close_to_root graph (LV.of_vertex succ) then
                  (* DistManipulator.bump succ_dist [Source] ~inc_delta:10. ~dec_delta:5. *)
                  DistManipulator.overwrite ~src:10. ~sin:5. ~san:5. ~non:5.
                else if Trunk.vertex_is_close_to_leaf graph (LV.of_vertex succ) then
                  DistManipulator.overwrite ~src:5. ~sin:10. ~san:5. ~non:5.
                else DistManipulator.overwrite ~src:10. ~sin:5. ~san:5. ~non:5.
            | Sink ->
                if Trunk.vertex_is_close_to_root graph (LV.of_vertex succ) then
                  DistManipulator.overwrite ~src:10. ~sin:5. ~san:5. ~non:5.
                else if Trunk.vertex_is_close_to_leaf graph (LV.of_vertex succ) then
                  DistManipulator.overwrite ~src:5. ~sin:10. ~san:5. ~non:5.
                else DistManipulator.overwrite ~src:5. ~sin:10. ~san:5. ~non:5.
            | Sanitizer ->
                if Trunk.vertex_is_close_to_root graph (LV.of_vertex succ) then
                  DistManipulator.overwrite ~src:10. ~sin:5. ~san:5. ~non:5.
                else if Trunk.vertex_is_close_to_leaf graph (LV.of_vertex succ) then
                  DistManipulator.overwrite ~src:5. ~sin:10. ~san:5. ~non:5.
                else DistManipulator.overwrite ~src:5. ~sin:5. ~san:10. ~non:5.
            | None ->
                DistManipulator.overwrite ~src:5. ~sin:5. ~san:5. ~non:10.
            | Indeterminate ->
                succ_dist
          in
          G.strong_update_dist succ new_dist acc )
        similarity_succs ~init:graph
    in
    (propagated, similarity_succs)


(** Propagate the same info to nodes with the same @annotations: requires that the new_fact's
      method have successors with nodewise simlarity edge bearing the same @annotation *)
let annotation_rule : rule =
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  if dry_run then (
    assert (Annotations.has_annot (Response.get_method new_fact)) ;
    (graph, []) )
  else
    match new_fact with
    | ForLabel (this_method, this_method_label) ->
        let this_method_vertices = G.this_method_vertices graph this_method
        and this_method_annotation = Annotations.get_annots this_method in
        let ns_succs_with_same_annot =
          let all_ns_succs =
            let* this_method_vertex = this_method_vertices in
            G.get_succs graph (LV.of_vertex this_method_vertex) ~label:NodeWiseSimilarity
          in
          List.filter
            ~f:(fun succ ->
              let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
              Annotations.equivalent this_method_annotation successor_annotation )
            all_ns_succs
        in
        let ns_propagated =
          List.fold
            ~f:(fun acc vertex ->
              let vertex_dist = Vertex.get_dist vertex in
              let new_dist =
                match this_method_label with
                | Source | Sink | Sanitizer ->
                    if Trunk.vertex_is_close_to_root graph (LV.of_vertex vertex) then
                      DistManipulator.bump vertex_dist [Source] ~inc_delta:10. ~dec_delta:5.
                    else if Trunk.vertex_is_close_to_leaf graph (LV.of_vertex vertex) then
                      DistManipulator.bump vertex_dist [Sink] ~inc_delta:10. ~dec_delta:5.
                    else vertex_dist
                | None | Indeterminate ->
                    vertex_dist
              in
              G.strong_update_dist vertex new_dist acc )
            ~init:graph ns_succs_with_same_annot
        in
        (ns_propagated, ns_succs_with_same_annot)
    | ForYesOrNo (this_method, this_method_label, affirmative) when affirmative ->
        let this_method_vertices = G.this_method_vertices graph this_method
        and this_method_annotation = Annotations.get_annots this_method in
        let ns_succs_with_same_annot =
          let all_ns_succs =
            let* this_method_vertex = this_method_vertices in
            G.get_succs graph (LV.of_vertex this_method_vertex) ~label:NodeWiseSimilarity
          in
          List.filter
            ~f:(fun succ ->
              let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
              Annotations.equivalent this_method_annotation successor_annotation )
            all_ns_succs
        in
        let ns_propagated =
          List.fold
            ~f:(fun acc vertex ->
              let new_dist = DistManipulator.oracle_overwrite this_method_label in
              G.strong_update_dist vertex new_dist acc )
            ~init:graph ns_succs_with_same_annot
        in
        (ns_propagated, ns_succs_with_same_annot)
    | _ ->
        (graph, [])


let mark_api_based_on_relative_position_in_its_trunk : rule =
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  let this_method_vertices = G.this_method_vertices graph (Response.get_method new_fact) in
  if dry_run then (
    let there_is_at_least_one_df_succ =
      List.exists this_method_vertices ~f:(fun this_method_vertex ->
          not @@ List.is_empty
          @@ G.get_succs graph (LV.of_vertex this_method_vertex) ~label:DataFlow )
    in
    assert there_is_at_least_one_df_succ ;
    (graph, []) )
  else
    let df_succs = this_method_vertices >>| LV.of_vertex >>= G.get_succs graph ~label:DataFlow in
    (* propagate along DF edges *)
    let df_propagated =
      List.fold
        ~f:(fun graph_acc df_succ ->
          let vertex_dist = Vertex.get_dist df_succ in
          let new_dist =
            match Method.is_api (Vertex.get_method df_succ) with
            | true -> (
                if ProbQuadruple.is_determined @@ Vertex.get_dist df_succ then vertex_dist
                else
                  match
                    Trunk.VertexPosition.find_position_of_vertex graph_acc (LV.of_vertex df_succ)
                  with
                  | Close_to_Root ->
                      DistManipulator.bump vertex_dist [Source] ~inc_delta:3. ~dec_delta:1.
                  | Close_to_Leaf ->
                      DistManipulator.bump vertex_dist [Sink] ~inc_delta:3. ~dec_delta:1.
                  | Right_at_Middle ->
                      vertex_dist )
            | false ->
                if not @@ Annotations.has_annot (Vertex.get_method df_succ) then
                  DistManipulator.bump vertex_dist [None] ~inc_delta:3. ~dec_delta:1.
                else vertex_dist
          in
          G.strong_update_dist df_succ new_dist graph_acc )
        df_succs ~init:graph
    in
    (df_propagated, df_succs)


let all_rules =
  [| {rule= contextual_similarity_rule; label= "contextual_similarity_rule"}
   ; {rule= nodewise_similarity_propagation_rule; label= "nodewise_similarity_propagation_rule"}
   ; {rule= annotation_rule; label= "annotation_rule"}
   ; { rule= mark_api_based_on_relative_position_in_its_trunk
     ; label= "mark_api_based_on_relative_position_in_its_trunk" } |]
