(** Rules for propagating facts *)

open ListMonad
open InfixOperators
open GraphRepr
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
    G.all_vertices_of_graph graph
    |> List.filter ~f:(fun (meth, _, _) -> Method.equal meth new_fact_method)
  in
  let contextual_succs =
    new_fact_method_vertices
    >>= fun vertex -> G.get_succs graph (LV.of_vertex vertex) ~label:ContextualSimilarity
  in
  assert (Int.( >= ) (List.length contextual_succs) 1) ;
  (* print_endline *)
  if dry_run then (graph, [])
  else
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = Vertex.get_dist succ in
          let is_inside_ns_cluster_containing_df_internals =
            let containing_cluster_opt =
              List.find (SimilarityHandler.all_ns_clusters graph) ~f:(fun cluster ->
                  List.mem cluster succ ~equal:Vertex.equal )
            in
            match containing_cluster_opt with
            | None ->
                false
            | Some containing_cluster ->
                List.exists containing_cluster ~f:(fun vertex ->
                    G.is_df_internal (LV.of_vertex vertex) graph )
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
    G.all_vertices_of_graph graph
    |> List.filter ~f:(fun (meth, _, _) -> Method.equal meth new_fact_method)
  in
  let similarity_succs =
    let raw_succs =
      new_fact_method_vertices
      >>= fun vertex -> G.get_succs graph (LV.of_vertex vertex) ~label:NodeWiseSimilarity
    in
    let module VertexSet = Caml.Set.Make (Vertex) in
    raw_succs |> VertexSet.of_list |> VertexSet.elements
  in
  assert (Int.( >= ) (List.length similarity_succs) 1) ;
  (* print_endline *)
  (* @@ F.asprintf "nodewise_similarity_rule chosen for %s" (Response.get_method new_fact) ; *)
  if dry_run then (graph, [])
  else
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_dist = Vertex.get_dist succ in
          let new_dist =
            match new_fact_label with
            | Source ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Source] ~inc_delta:10. ~dec_delta:5.
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Sink] ~inc_delta:10. ~dec_delta:5.
                else
                  (* bump the likelihood of the successor being a source *)
                  DistManipulator.bump succ_dist [Source] ~inc_delta:10. ~dec_delta:5.
            | Sink ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Source] ~inc_delta:10. ~dec_delta:5.
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Sink] ~inc_delta:10. ~dec_delta:5.
                else DistManipulator.bump succ_dist [Sink] ~inc_delta:10. ~dec_delta:5.
            | Sanitizer ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Source] ~inc_delta:10. ~dec_delta:5.
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [Sink] ~inc_delta:10. ~dec_delta:5.
                else DistManipulator.bump succ_dist [Sanitizer] ~inc_delta:10. ~dec_delta:5.
            | None ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [None] ~inc_delta:10. ~dec_delta:5.
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  DistManipulator.bump succ_dist [None] ~inc_delta:10. ~dec_delta:5.
                else DistManipulator.bump succ_dist [None] ~inc_delta:10. ~dec_delta:5.
            | Indeterminate ->
                succ_dist
          in
          G.strong_update_dist succ new_dist acc )
        similarity_succs ~init:graph
    in
    (propagated, similarity_succs)


let internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink : rule =
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  let new_fact_label = Response.get_label new_fact in
  let new_fact_method = Response.get_method new_fact in
  let new_fact_vertices = G.this_method_vertices graph new_fact_method in
  if not @@ TaintLabel.equal TaintLabel.Sink new_fact_label then (
    let trunks_containing_vertices =
      new_fact_vertices >>= (Array.to_list << Trunk.find_trunks_containing_vertex graph)
    in
    assert (not @@ List.is_empty trunks_containing_vertices) ;
    (* print_endline *)
    (* @@ F.asprintf "internal_nonbidirectional_library_node... chosen for %s" *)
    (*      (Response.get_method new_fact) ; *)
    if dry_run then (graph, [])
    else
      let trunk_leaves = trunks_containing_vertices >>| Array.last in
      (* if all of trunk_leaves are sinks, then this may be a source! *)
      if
        List.exists
          ~f:(fun leaf ->
            TaintLabel.equal (ProbQuadruple.determine_label (Vertex.get_dist leaf)) TaintLabel.Sink
            )
          trunk_leaves
      then
        List.fold
          ~f:(fun big_acc trunk ->
            Array.fold
              ~f:(fun ((graph_acc, affected) as smol_acc) vertex ->
                let vertex_meth = Vertex.get_method vertex in
                let ns_clusters_vertices = List.join @@ SimilarityHandler.all_ns_clusters graph in
                if
                  NodeWiseFeatures.SingleFeature.is_library_code vertex_meth
                  && (not @@ G.is_df_leaf (LV.of_vertex vertex) graph)
                  && (not @@ Method.is_initializer vertex_meth)
                  && (not @@ List.mem ~equal:Vertex.equal ns_clusters_vertices vertex)
                then
                  let new_dist =
                    DistManipulator.bump (Vertex.get_dist vertex) [Source] ~inc_delta:3.
                      ~dec_delta:3.
                  in
                  (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected)
                else if Method.equal (Vertex.get_method vertex) new_fact_method then
                  let new_dist =
                    DistManipulator.bump (Vertex.get_dist vertex) [new_fact_label] ~inc_delta:10.
                      ~dec_delta:10.
                  in
                  (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected)
                else smol_acc )
              ~init:big_acc trunk )
          ~init:(graph, []) trunks_containing_vertices
      else (graph, []) )
  else if
    (* UNSURE: is the condition correct? *)
    (* the new knowledge should be about a leaf vertex.
       ==> if the new methods only appear at leaves in the graph, then the above holds. *)
    not
    @@ List.for_all ~f:(fun vertex -> G.is_df_leaf (LV.of_vertex vertex) graph) new_fact_vertices
  then (graph, []) (* Do nothing *)
  else
    let trunks_containing_vertices =
      new_fact_vertices >>= (Array.to_list << Trunk.find_trunks_containing_vertex graph)
    in
    assert (not @@ List.is_empty trunks_containing_vertices) ;
    (* print_endline *)
    (* @@ F.asprintf "internal_nonbidirectional_library_node... chosen for %s" *)
    (*      (Response.get_method new_fact) ; *)
    if dry_run then (graph, [])
    else
      let new_fact_propagated =
        List.fold
          ~f:(fun acc new_fact_vertex ->
            let vertex_dist = Vertex.get_dist new_fact_vertex in
            let updated_dist =
              DistManipulator.bump vertex_dist [Sink] ~inc_delta:10. ~dec_delta:5.
            in
            G.strong_update_dist new_fact_vertex updated_dist acc )
          ~init:graph new_fact_vertices
      in
      List.fold
        ~f:(fun big_acc trunk ->
          Array.fold
            ~f:(fun ((graph_acc, affected) as smol_acc) vertex ->
              let vertex_meth = Vertex.get_method vertex and vertex_dist = Vertex.get_dist vertex in
              let ns_clusters_vertices = List.join @@ SimilarityHandler.all_ns_clusters graph in
              if
                NodeWiseFeatures.SingleFeature.is_library_code vertex_meth
                && (not @@ G.is_df_leaf (LV.of_vertex vertex) graph)
                && (not @@ Method.is_initializer vertex_meth)
                && (not @@ List.mem ~equal:Vertex.equal ns_clusters_vertices vertex)
              then
                let new_dist =
                  DistManipulator.bump vertex_dist [Source] ~inc_delta:10. ~dec_delta:5.
                in
                (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected)
              else if Method.equal (Vertex.get_method vertex) new_fact_method then
                let new_dist =
                  DistManipulator.bump (Vertex.get_dist vertex) [new_fact_label] ~inc_delta:10.
                    ~dec_delta:10.
                in
                (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected)
              else smol_acc )
            ~init:big_acc trunk )
        ~init:(new_fact_propagated, []) trunks_containing_vertices


(** Propagate the same info to nodes with the same @annotations: requires that the new_fact's
      method have successors with nodewise simlarity edge bearing the same @annotation *)
let annotation_rule : rule =
 (* NOTE Under construction *)
 fun (graph : G.t) (new_fact : Response.t) ~(dry_run : bool) : (G.t * Vertex.t list) ->
  if dry_run then (
    assert (Annotations.has_annot (Response.get_method new_fact)) ;
    (* print_endline @@ F.asprintf "annotation_rule chosen for %s" (Response.get_method new_fact) ; *)
    (graph, []) )
  else
    match new_fact with
    | ForLabel (this_method, this_method_label) ->
        let this_method_vertices = G.this_method_vertices graph this_method
        and this_method_annotation = Annotations.get_annots this_method in
        let cs_succs_with_same_annot =
          let all_cs_succs =
            let* this_method_vertex = this_method_vertices in
            G.get_succs graph (LV.of_vertex this_method_vertex) ~label:ContextualSimilarity
          in
          List.filter
            ~f:(fun succ ->
              let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
              Annotations.equal this_method_annotation successor_annotation )
            all_cs_succs
        in
        (* Propagate to CS successors *)
        let cs_propagated =
          List.fold
            ~f:(fun acc vertex ->
              let vertex_dist = Vertex.get_dist vertex in
              let new_dist =
                match this_method_label with
                | Source | Sink | Sanitizer ->
                    if G.is_df_root (LV.of_vertex vertex) graph then
                      DistManipulator.bump vertex_dist [Source] ~inc_delta:10. ~dec_delta:5.
                    else if G.is_df_leaf (LV.of_vertex vertex) graph then
                      DistManipulator.bump vertex_dist [Sink] ~inc_delta:10. ~dec_delta:5.
                    else vertex_dist
                | None | Indeterminate ->
                    vertex_dist
              in
              G.strong_update_dist vertex new_dist acc )
            ~init:graph cs_succs_with_same_annot
        in
        (cs_propagated, cs_succs_with_same_annot)
    | ForYesOrNo (this_method, this_method_label, affirmative) when affirmative ->
        let this_method_vertices = G.this_method_vertices graph this_method
        and this_method_annotation = Annotations.get_annots this_method in
        let cs_succs_with_same_annot =
          let all_cs_succs =
            let* this_method_vertex = this_method_vertices in
            G.get_succs graph (LV.of_vertex this_method_vertex) ~label:ContextualSimilarity
          in
          List.filter
            ~f:(fun succ ->
              let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
              Annotations.equal this_method_annotation successor_annotation )
            all_cs_succs
        in
        (* Propagate to CS successors *)
        let cs_propagated =
          List.fold
            ~f:(fun acc vertex ->
              let new_dist = DistManipulator.oracle_overwrite this_method_label in
              G.strong_update_dist vertex new_dist acc )
            ~init:graph cs_succs_with_same_annot
        in
        (cs_propagated, cs_succs_with_same_annot)
    | _ ->
        (graph, [])


let all_rules =
  [ {rule= contextual_similarity_rule; label= "contextual_similarity_rule"}
  ; {rule= nodewise_similarity_propagation_rule; label= "nodewise_similarity_propagation_rule"}
  ; { rule= internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink
    ; label= "internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink" }
    (* ; { rule= if_method_is_none_once_then_it's_none_everywhere *)
    (*   ; label= "if_method_is_none_once_then_it's_none_everywhere" } *)
  ; {rule= annotation_rule; label= "annotation_rule"} ]
