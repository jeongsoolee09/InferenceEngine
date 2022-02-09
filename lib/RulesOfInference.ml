open InfixOperators
open ListMonad
open GraphRepr
open DistManipulator

exception TODO

exception NotImplemented

module Random = Core_kernel.Random
module LV = G.LiteralVertex

module Question = struct
  type t = AskingForLabel of Method.t | AskingForConfirmation of (Method.t * TaintLabel.t)

  (** make a prompt message out of a question term. *)
  let make_prompt (question : t) : string =
    match question with
    | AskingForLabel meth ->
        F.asprintf "What label does %s bear? [src|sin|san|non]: " (Method.to_string meth)
    | AskingForConfirmation (meth, label) ->
        F.asprintf "Method %s is a %s, right? [yes|no]: " (Method.to_string meth)
          (TaintLabel.to_string label)


  let get_method (question : t) : Method.t =
    match question with AskingForLabel meth -> meth | AskingForConfirmation (meth, _) -> meth


  let get_label (question : t) : TaintLabel.t =
    match question with
    | AskingForConfirmation (_, label) ->
        label
    | _ ->
        raise @@ Invalid_argument "hahaha"
end

module Response = struct
  type t = ForLabel of (Method.t * TaintLabel.t) | ForYesOrNo of (Method.t * TaintLabel.t * bool)

  let to_string (response : t) : string =
    match response with
    | ForLabel (meth_, label) ->
        F.asprintf "ForLabel (%s, %s)" (Method.to_string meth_) (TaintLabel.to_string label)
    | ForYesOrNo (meth_, label, bool) ->
        F.asprintf "ForYesOrNo (%s, %s, %s)" (Method.to_string meth_) (TaintLabel.to_string label)
          (Bool.to_string bool)


  let list_to_string (responses : t list) : string =
    let contents =
      List.fold responses ~f:(fun acc response -> acc ^ to_string response ^ ", ") ~init:""
    in
    F.asprintf "[%s]" contents


  let get_method (res : t) : Method.t =
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


  let response_of_dist (method_ : Method.t) (dist : ProbQuadruple.t) : t =
    let label = ProbQuadruple.determine_label dist in
    ForLabel (method_, label)


  let response_of_string_forlabel (method_ : Method.t) (response_str : string) : t =
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


  let response_of_string_foryesorno (method_ : Method.t) (label : TaintLabel.t)
      (response_str : string) : t =
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
    if List.is_empty list then failwith "cannot select from an empty list" ;
    let random_index = Random.int_incl 0 (List.length list - 1) in
    List.nth_exn list random_index
end

(** Rules for propagating facts *)
module PropagationRules = struct
  type rule = G.t -> Response.t -> Response.t list -> dry_run:bool -> G.t * Vertex.t list

  type t = {rule: rule; label: string}

  (** propagating to contextually similar vertices: requires that the new_fact's method have
      successors with contextual similarity edge *)
  let contextual_similarity_rule : rule =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ~(dry_run : bool) :
       (G.t * Vertex.t list) ->
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _, _) -> Method.equal meth new_fact_method)
    in
    let contextual_succs =
      new_fact_method_vertices
      >>= fun vertex ->
      G.get_succs graph (LV.of_vertex vertex) ~label:EdgeLabel.ContextualSimilarity
    in
    assert (Int.( >= ) (List.length contextual_succs) 1) ;
    print_endline "contextual_similarity_rule chosen" ;
    (* Out_channel.print_endline *)
    (* @@ F.asprintf "contextual_succs: %s" (Vertex.vertex_list_to_string contextual_succs) ; *)
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_meth, succ_label, succ_dist = succ in
          let is_inside_ns_cluster_containing_df_internals =
            let containing_cluster_opt =
              List.find (all_ns_clusters graph) ~f:(fun cluster ->
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
            | Source ->
                if is_inside_ns_cluster_containing_df_internals then succ_dist
                else
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
            | Sink ->
                if is_inside_ns_cluster_containing_df_internals then succ_dist
                else
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
                { ProbQuadruple.src= succ_dist.src -. 0.1
                ; sin= succ_dist.sin -. 0.1
                ; san= succ_dist.san -. 0.1
                ; non= succ_dist.non +. 0.3 }
            | Indeterminate ->
                succ_dist
          in
          (* if not dry_run then *)
          (*   Out_channel.fprintf Out_channel.stdout *)
          (*     "%s propagated its info to %s (contextual), its dist is now %s\n" new_fact_method *)
          (*     (LV.to_string (LV.of_vertex succ)) *)
          (*     (ProbQuadruple.to_string new_dist) ; *)
          G.strong_update_dist succ new_dist acc )
        contextual_succs ~init:graph
    in
    (propagated, contextual_succs)


  (** Propagate the same info to nodes that are similar nodewise: requires that the new_fact's
      method have successors with nodewise simlarity edge *)
  let nodewise_similarity_propagation_rule : rule =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ~(dry_run : bool) :
       (G.t * Vertex.t list) ->
    let new_fact_method = Response.get_method new_fact
    and new_fact_label = Response.get_label new_fact in
    let new_fact_method_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun (meth, _, _) -> Method.equal meth new_fact_method)
    in
    let similarity_succs =
      let raw_succs =
        new_fact_method_vertices
        >>= fun vertex ->
        G.get_succs graph (LV.of_vertex vertex) ~label:EdgeLabel.NodeWiseSimilarity
      in
      let module VertexSet = Caml.Set.Make (Vertex) in
      raw_succs |> VertexSet.of_list |> VertexSet.elements
    in
    assert (Int.( >= ) (List.length similarity_succs) 1) ;
    print_endline "nodewise_similarity_propagation_rule chosen" ;
    (* Out_channel.print_endline *)
    (* @@ F.asprintf "nodewise_succs: %s" (Vertex.vertex_list_to_string similarity_succs) ; *)
    let propagated =
      List.fold
        ~f:(fun acc succ ->
          let succ_meth, succ_label, succ_dist = succ in
          let new_dist =
            match new_fact_label with
            | Source ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
            | Sink ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
            | Sanitizer ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src +. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else
                  (* bump the likelihood of the successor being a source *)
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san +. 0.3
                  ; non= succ_dist.non -. 0.1 }
            | None ->
                if G.is_df_root (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.3
                  ; sin= succ_dist.sin -. 0.1
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non +. 0.1 }
                else if G.is_df_leaf (LV.of_vertex succ) graph then
                  { ProbQuadruple.src= succ_dist.src -. 0.1
                  ; sin= succ_dist.sin +. 0.3
                  ; san= succ_dist.san -. 0.1
                  ; non= succ_dist.non -. 0.1 }
                else
                  (* bump the likelihood of the successor being a none *)
                  { ProbQuadruple.src= succ_dist.src -. 0.3
                  ; sin= succ_dist.sin -. 0.3
                  ; san= succ_dist.san -. 0.3
                  ; non= succ_dist.non +. 0.8 }
            | Indeterminate ->
                succ_dist
          in
          (* if not dry_run then *)
          (*   Out_channel.fprintf Out_channel.stdout *)
          (*     "%s propagated its info to %s (nodewise), its dist is now %s\n" new_fact_method *)
          (*     (LV.to_string (LV.of_vertex succ)) *)
          (*     (ProbQuadruple.to_string new_dist) ; *)
          G.strong_update_dist succ new_dist acc )
        similarity_succs ~init:graph
    in
    (propagated, similarity_succs)


  let internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink : rule =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ~(dry_run : bool) :
       (G.t * Vertex.t list) ->
    let new_fact_label = Response.get_label new_fact in
    let new_fact_method = Response.get_method new_fact in
    let new_fact_vertices = G.this_method_vertices graph new_fact_method in
    if not @@ TaintLabel.equal TaintLabel.Sink new_fact_label then
      let trunks_containing_vertices =
        new_fact_vertices >>= (Array.to_list << Trunk.find_trunks_containing_vertex graph)
      in
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
                let vertex_meth, vertex_loc, vertex_dist = vertex in
                let ns_clusters_vertices = List.join @@ all_ns_clusters graph in
                if
                  NodeWiseFeatures.SingleFeature.is_library_code vertex_meth
                  && (not @@ G.is_df_leaf (LV.of_vertex vertex) graph)
                  && (not @@ Method.is_initializer vertex_meth)
                  && (not @@ List.mem ~equal:Vertex.equal ns_clusters_vertices vertex)
                then
                  let new_dist =
                    { ProbQuadruple.src= vertex_dist.src +. 0.3
                    ; sin= vertex_dist.sin -. 0.1
                    ; san= vertex_dist.san -. 0.1
                    ; non= vertex_dist.non -. 0.1 }
                  in
                  (* if not dry_run then *)
                  (*   Out_channel.fprintf Out_channel.stdout *)
                  (*     "%s propagated its info to %s (internal_src), its dist is now %s\n" *)
                  (*     new_fact_method *)
                  (*     (LV.to_string (LV.of_vertex vertex)) *)
                  (*     (ProbQuadruple.to_string new_dist) ; *)
                  (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected)
                else smol_acc )
              ~init:big_acc trunk )
          ~init:(graph, []) trunks_containing_vertices
      else (graph, [])
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
      let new_fact_propagated =
        List.fold
          ~f:(fun acc new_fact_vertex ->
            let vertex_dist = Vertex.get_dist new_fact_vertex in
            let updated_dist =
              { ProbQuadruple.src= vertex_dist.src -. 0.1
              ; sin= vertex_dist.sin +. 0.3
              ; san= vertex_dist.san -. 0.1
              ; non= vertex_dist.non -. 0.1 }
            in
            G.strong_update_dist new_fact_vertex updated_dist acc )
          ~init:graph new_fact_vertices
      in
      List.fold
        ~f:(fun big_acc trunk ->
          Array.fold
            ~f:(fun ((graph_acc, affected) as smol_acc) vertex ->
              let vertex_meth, vertex_loc, vertex_dist = vertex in
              let ns_clusters_vertices = List.join @@ all_ns_clusters graph in
              if
                NodeWiseFeatures.SingleFeature.is_library_code vertex_meth
                && (not @@ G.is_df_leaf (LV.of_vertex vertex) graph)
                && (not @@ Method.is_initializer vertex_meth)
                && (not @@ List.mem ~equal:Vertex.equal ns_clusters_vertices vertex)
              then (
                let new_dist =
                  { ProbQuadruple.src= vertex_dist.src +. 0.3
                  ; sin= vertex_dist.sin -. 0.1
                  ; san= vertex_dist.san -. 0.1
                  ; non= vertex_dist.non -. 0.1 }
                in
                if not dry_run then
                  Out_channel.fprintf Out_channel.stdout
                    "%s propagated its info to %s (internal_src), its dist is now %s\n"
                    (Method.to_string new_fact_method)
                    (LV.to_string (LV.of_vertex vertex))
                    (ProbQuadruple.to_string new_dist) ;
                (G.strong_update_dist vertex new_dist graph_acc, vertex :: affected) )
              else smol_acc )
            ~init:big_acc trunk )
        ~init:(new_fact_propagated, []) trunks_containing_vertices


  let if_method_is_none_once_then_it's_none_everywhere : rule =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ~(dry_run : bool) :
       (G.t * Vertex.t list) ->
    let new_fact_method = Response.get_method new_fact in
    let new_fact_label = Response.get_label new_fact in
    if TaintLabel.is_none new_fact_label then
      let this_method_vertices = G.this_method_vertices graph new_fact_method in
      List.fold this_method_vertices
        ~f:(fun (graph_acc, affected) this_method_vertex ->
          let vertex_dist = Vertex.get_dist this_method_vertex in
          let new_dist =
            { ProbQuadruple.src= vertex_dist.src -. 0.1
            ; sin= vertex_dist.sin -. 0.1
            ; san= vertex_dist.san -. 0.1
            ; non= vertex_dist.non +. 0.3 }
          in
          (* if not dry_run then *)
          (*   Out_channel.printf "%s propagated its info to %s (internal_src), its dist is now %s\n" *)
          (*     new_fact_method *)
          (*     (LV.to_string (LV.of_vertex this_method_vertex)) *)
          (*     (ProbQuadruple.to_string new_dist) ; *)
          ( G.strong_update_dist this_method_vertex new_dist graph_acc
          , this_method_vertex :: affected ) )
        ~init:(graph, [])
    else (graph, [])


  (** Propagate the same info to nodes with the same @annotations: requires that the new_fact's
      method have successors with nodewise simlarity edge bearing the same @annotation *)

  let annotation_rule : rule =
   fun (graph : G.t) (new_fact : Response.t) (prev_facts : Response.t list) ~(dry_run : bool) :
       (G.t * Vertex.t list) ->
    (* assert that this method has an annotation. *)
    let this_method = Response.get_method new_fact
    and this_method_label = Response.get_label new_fact in
    let this_method_vertices = G.this_method_vertices graph this_method
    and this_method_annotation = Annotations.get_annots this_method in
    let this_method_has_annotation =
      not @@ Annotations.equal this_method_annotation Annotations.empty
    in
    assert this_method_has_annotation ;
    (* assert that there is at least one predecessor with the same annotation. *)
    let recursive_preds_with_same_annot =
      let all_recursive_preds =
        let* this_method_vertex = this_method_vertices in
        get_recursive_preds graph (LV.of_vertex this_method_vertex) ~label:DataFlow
      in
      List.filter
        ~f:(fun pred ->
          let predecessor_annotation = Annotations.get_annots (Vertex.get_method pred) in
          Annotations.equal this_method_annotation predecessor_annotation )
        all_recursive_preds
    (* assert that there is at least one successor with the same annotation. *)
    and recursive_succs_with_same_annot =
      let all_recursive_succs =
        let* this_method_vertex = this_method_vertices in
        get_recursive_succs graph (LV.of_vertex this_method_vertex) ~label:DataFlow
      in
      List.filter
        ~f:(fun succ ->
          let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
          Annotations.equal this_method_annotation successor_annotation )
        all_recursive_succs
    (* assert that there is at least one successor with the same annotation. *)
    and ns_succs_with_same_annot =
      let all_ns_succs =
        let* this_method_vertex = this_method_vertices in
        get_recursive_succs graph (LV.of_vertex this_method_vertex) ~label:NodeWiseSimilarity
      in
      List.filter
        ~f:(fun succ ->
          let successor_annotation = Annotations.get_annots (Vertex.get_method succ) in
          Annotations.equal this_method_annotation successor_annotation )
        all_ns_succs
    in
    let there_is_at_least_one_recursive_pred_with_same_annot =
      not @@ List.is_empty recursive_preds_with_same_annot
    and there_is_at_least_one_recursive_succ_with_same_annot =
      not @@ List.is_empty recursive_succs_with_same_annot
    and there_is_at_least_one_ns_succ_with_same_annot =
      not @@ List.is_empty ns_succs_with_same_annot
    in
    assert (
      there_is_at_least_one_recursive_pred_with_same_annot
      || there_is_at_least_one_recursive_succ_with_same_annot
      || there_is_at_least_one_ns_succ_with_same_annot ) ;
    (* First, propagate to DF recursive preds and succs *)
    let df_propagated =
      List.fold
        ~f:(fun acc vertex ->
          let vertex_dist = Vertex.get_dist vertex in
          let new_dist =
            match this_method_label with
            | Source | Sink | Sanitizer ->
                if G.is_df_root (LV.of_vertex vertex) graph then
                  DistManipulator.bump vertex_dist [Source] ~inc_delta:1. ~dec_delta:0.9
                else if G.is_df_leaf (LV.of_vertex vertex) graph then
                  DistManipulator.bump vertex_dist [Sink] ~inc_delta:1. ~dec_delta:0.9
                else vertex_dist
            | None | Indeterminate ->
                vertex_dist
          in
          G.strong_update_dist vertex new_dist acc )
        ~init:graph
        (recursive_preds_with_same_annot @ recursive_succs_with_same_annot)
    in
    (* Second, propagate to NS successors *)
    let ns_propagated =
      List.fold
        ~f:(fun acc vertex ->
          let vertex_dist = Vertex.get_dist vertex in
          let new_dist =
            match this_method_label with
            | Source | Sink | Sanitizer ->
                if G.is_df_root (LV.of_vertex vertex) graph then
                  DistManipulator.bump vertex_dist [Source] ~inc_delta:1. ~dec_delta:0.9
                else if G.is_df_leaf (LV.of_vertex vertex) graph then
                  DistManipulator.bump vertex_dist [Sink] ~inc_delta:1. ~dec_delta:0.9
                else vertex_dist
            | None | Indeterminate ->
                vertex_dist
          in
          (* if not dry_run then *)
          (*   Out_channel.fprintf Out_channel.stdout *)
          (*     "%s propagated its info to %s (nodewise), its dist is now %s\n" new_fact_method *)
          (*     (LV.to_string (LV.of_vertex succ)) *)
          (*     (ProbQuadruple.to_string new_dist) ; *)
          G.strong_update_dist vertex new_dist acc )
        ~init:df_propagated ns_succs_with_same_annot
    in
    ( ns_propagated
    , recursive_preds_with_same_annot @ recursive_succs_with_same_annot @ ns_succs_with_same_annot
    )


  let all_rules =
    [ {rule= contextual_similarity_rule; label= "contextual_similarity_rule"}
    ; {rule= nodewise_similarity_propagation_rule; label= "nodewise_similarity_propagation_rule"}
    ; { rule= internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink
      ; label= "internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink" }
    ; { rule= if_method_is_none_once_then_it's_none_everywhere
      ; label= "if_method_is_none_once_then_it's_none_everywhere" }
    ; {rule= annotation_rule; label= "annotation_rule"} ]
end

(* Use Random.int_incl for making a random integer. *)

module AskingRules = struct
  type rule = G.t -> Response.t list -> NodeWiseFeatures.NodeWiseFeatureMap.t -> Question.t

  type t = {rule: rule; label: string}

  let ask_if_leaf_is_sink : rule =
   fun (snapshot : G.t) (received_responses : Response.t list)
       (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : Question.t ->
    (* TODO: consider featuremaps *)
    let all_non_sus_leaves =
      G.collect_df_leaves snapshot
      |> List.filter ~f:(fun leaf ->
             let containing_cluster_opt =
               List.find (all_ns_clusters snapshot) ~f:(fun cluster ->
                   List.mem cluster leaf ~equal:Vertex.equal )
             in
             match containing_cluster_opt with
             | None ->
                 true
             | Some containing_cluster ->
                 not
                 @@ List.exists containing_cluster ~f:(fun vertex ->
                        G.is_df_internal (LV.of_vertex vertex) snapshot
                        || List.exists
                             (get_recursive_preds snapshot (LV.of_vertex vertex)
                                ~label:EdgeLabel.DataFlow )
                             ~f:(fun vertex ->
                               NodeWiseFeatures.SingleFeature.is_main_method
                                 (Vertex.get_method vertex) ) ) )
    in
    let all_leaves_are_determined =
      List.for_all all_non_sus_leaves ~f:(fun leaf ->
          G.Saturation.dist_is_saturated (Vertex.get_dist leaf) )
    in
    assert (not all_leaves_are_determined) ;
    let random_leaf = Utils.random_select_elem all_non_sus_leaves in
    Question.AskingForLabel (Vertex.get_method random_leaf)


  let ask_if_root_is_source : rule =
   fun (snapshot : G.t) (received_responses : Response.t list)
       (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : Question.t ->
    (* TODO consider featuremaps *)
    let all_roots_are_determined =
      List.for_all (G.collect_df_roots snapshot) ~f:(fun root ->
          G.Saturation.dist_is_saturated (Vertex.get_dist root) )
    in
    assert (not all_roots_are_determined) ;
    let all_roots = G.collect_df_roots snapshot in
    let random_root =
      let random_index = Random.int_incl 0 (List.length all_roots - 1) in
      List.nth_exn all_roots random_index
    in
    Question.AskingForLabel (Vertex.get_method random_root)


  (** ask a method from a foreign package of its label. *)
  let ask_foreign_package_label : rule =
   fun (snapshot : G.t) (received_responses : Response.t list)
       (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : Question.t ->
    let all_foreign_codes_are_determined =
      let all_foreign_codes =
        G.fold_vertex
          (fun vertex acc ->
            if NodeWiseFeatures.SingleFeature.is_framework_method (Vertex.get_method vertex) then
              vertex :: acc
            else acc )
          snapshot []
      in
      List.for_all all_foreign_codes ~f:(fun foreign_code ->
          G.Saturation.dist_is_saturated (Vertex.get_dist foreign_code) )
    in
    assert (not all_foreign_codes_are_determined) ;
    let all_foreign_package_vertices =
      G.fold_vertex
        (fun vertex acc ->
          let open NodeWiseFeatures in
          if SingleFeature.is_framework_method (Vertex.get_method vertex) then vertex :: acc
          else acc )
        snapshot []
    in
    let random_foreign_vertex = Utils.random_select_elem all_foreign_package_vertices in
    Question.AskingForLabel (Vertex.get_method random_foreign_vertex)


  let ask_indeterminate : rule =
   (* NOTE this should *NOT* be run at the first round of Loop. *)
   fun (snapshot : G.t) (received_responses : Response.t list)
       (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : Question.t ->
    let all_indeterminates =
      List.filter (G.all_vertices_of_graph snapshot) ~f:(fun vertex ->
          ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) )
    in
    (* TODO: don't randomly pick one; make it consider what should be an influential indeterminate node. *)
    let random_indeterminate_vertex = Utils.random_select_elem all_indeterminates in
    Question.AskingForLabel (Vertex.get_method random_indeterminate_vertex)


  let ask_from_ns_cluster_if_it_contains_internal_src_or_sink : rule =
   fun (snapshot : G.t) (received_responses : Response.t list)
       (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : Question.t ->
    let there_is_some_cluster_that_has_internal_src_or_sink =
      List.for_all (all_ns_clusters snapshot) ~f:(fun ns_cluster ->
          List.exists ns_cluster ~f:(fun vertex ->
              G.is_df_internal (LV.of_vertex vertex) snapshot
              && ( ProbQuadruple.is_source (Vertex.get_dist vertex)
                 || ProbQuadruple.is_sin (Vertex.get_dist vertex) ) ) )
    in
    assert there_is_some_cluster_that_has_internal_src_or_sink ;
    let not_asked_clusters =
      List.filter (all_ns_clusters snapshot) ~f:(fun cluster ->
          not
          @@ List.exists received_responses ~f:(fun received_response ->
                 List.mem ~equal:Method.equal
                   (List.map ~f:Vertex.get_method cluster)
                   (Response.get_method received_response) )
          && List.exists cluster ~f:(fun vertex ->
                 ProbQuadruple.is_source (Vertex.get_dist vertex)
                 || ProbQuadruple.is_none (Vertex.get_dist vertex) ) )
    in
    let random_cluster = Utils.random_select_elem not_asked_clusters in
    let random_vertex_in_picked_cluster = Utils.random_select_elem random_cluster in
    if G.is_df_internal (LV.of_vertex random_vertex_in_picked_cluster) snapshot then
      Question.AskingForConfirmation
        (Vertex.get_method random_vertex_in_picked_cluster, TaintLabel.None)
    else Question.AskingForLabel (Vertex.get_method random_vertex_in_picked_cluster)


  let all_rules : t list =
    [ {label= "ask_if_leaf_is_sink"; rule= ask_if_leaf_is_sink}
    ; {label= "ask_if_root_is_source"; rule= ask_if_root_is_source}
    ; {label= "ask_foreign_package_label"; rule= ask_foreign_package_label}
    ; {label= "ask_indeterminate"; rule= ask_indeterminate}
    ; { label= "ask_from_ns_cluster_if_it_contains_internal_src_or_sink"
      ; rule= ask_from_ns_cluster_if_it_contains_internal_src_or_sink } ]
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
               ignore @@ prop_rule.rule graph new_fact prev_facts ~dry_run:true ;
               prop_rule :: acc
             with Assert_failure _ -> acc )
           ~init:[] prop_rules


    let assign_priority_on_propagation_rules prop_rules (graph : G.t) =
      (* TODO static for now, but could be dynamic *)
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
           ~compare:(fun (_, priority1) (_, priority2) -> -Int.compare priority1 priority2)
           priority_assigned
  end

  (** the priority of asking rules represent their current adequacy of application. *)
  module ForAsking = struct
    let take_subset_of_applicable_asking_rules (snapshot : G.t) (responses : Response.t list)
        (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) (asking_rules : AskingRules.t list) =
      (* rule R is applicable to vertex V iff (def) V has successor with labeled edge required by rule R
                                          iff (def) the embedded assertion succeeds *)
      List.rev
      @@ List.fold
           ~f:(fun acc asking_rule ->
             try
               ignore @@ asking_rule.rule snapshot responses nfeaturemap ;
               asking_rule :: acc
             with _ -> acc )
           ~init:[] asking_rules


    let assign_priority_on_asking_rules (asking_rules : AskingRules.t list) (graph : G.t) :
        (AskingRules.t * int) list =
      let open AskingRules in
      asking_rules
      >>= fun asking_rule ->
      match asking_rule.label with
      | "ask_if_leaf_is_sink" ->
          return (asking_rule, 5)
      | "ask_if_root_is_source" ->
          return (asking_rule, 4)
      | "ask_foreign_package_label" ->
          return (asking_rule, 3)
      | "ask_indeterminate" ->
          return (asking_rule, 1)
      | "ask_from_ns_cluster_if_it_contains_internal_src_or_sink" ->
          return (asking_rule, 2)
      | otherwise ->
          failwith (otherwise ^ " is not covered!")


    (** choose the most applicable asking rule. *)
    let asking_rules_selector (graph : G.t) (responses : Response.t list)
        (nfeaturemap : NodeWiseFeatures.NodeWiseFeatureMap.t) : AskingRules.t =
      let priority_assigned =
        assign_priority_on_asking_rules
          (take_subset_of_applicable_asking_rules graph responses nfeaturemap AskingRules.all_rules)
          graph
      in
      (* sort the asking rules by the priority, and get the first one. *)
      let asking_rule =
        fst @@ List.hd_exn
        @@ List.sort
             ~compare:(fun (_, priority1) (_, priority2) -> -Int.compare priority1 priority2)
             priority_assigned
      in
      asking_rule
  end
end
