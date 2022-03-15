open InfixOperators
open ListMonad
open GraphRepr
module LV = G.LiteralVertex

type rule = G.t -> Response.t list -> dry_run:bool -> Question.t

type t = {rule: rule; label: string}

let ask_if_leaf_is_sink : rule =
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
  let all_non_sus_leaves =
    G.collect_df_leaves snapshot
    |> List.filter ~f:(fun leaf ->
           let containing_cluster_opt =
             List.find (SimilarityHandler.all_ns_clusters snapshot) ~f:(fun cluster ->
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
                           (get_recursive_preds snapshot (LV.of_vertex vertex) ~label:DataFlow)
                           ~f:(fun vertex ->
                             NodeWiseFeatures.SingleFeature.is_main_method
                               (Vertex.get_method vertex) ) ) )
    |> List.filter ~f:(fun leaf ->
           ProbQuadruple.is_indeterminate (Vertex.get_dist leaf)
           && (not @@ Method.classname_implements_some_interface (Vertex.get_method leaf))
           && not
              @@ List.mem
                   (received_responses >>| Response.get_method)
                   (Vertex.get_method leaf) ~equal:Method.equal )
  in
  let all_leaves_are_determined =
    List.for_all all_non_sus_leaves ~f:(fun leaf ->
        G.Saturation.dist_is_saturated (Vertex.get_dist leaf) )
  in
  assert (not all_leaves_are_determined) ;
  if dry_run then Question.dummy
  else
    let random_leaf =
      MethodSelector.select_by_degree
        (GraphRepr.vertex_list_to_method_list all_non_sus_leaves)
        received_responses snapshot
    in
    Question.AskingForLabel random_leaf


let ask_if_root_is_source : rule =
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
  let all_roots_are_determined =
    List.for_all (G.collect_df_roots snapshot) ~f:(fun root ->
        G.Saturation.dist_is_saturated (Vertex.get_dist root) )
  in
  assert (not all_roots_are_determined) ;
  if dry_run then Question.dummy
  else
    let all_roots = G.collect_df_roots snapshot in
    let random_root =
      let unasked_indeterminate_roots =
        List.filter
          ~f:(fun root ->
            ProbQuadruple.is_indeterminate (Vertex.get_dist root)
            && (not @@ Method.classname_implements_some_interface (Vertex.get_method root))
            && not
               @@ List.mem
                    (received_responses >>| Response.get_method)
                    (Vertex.get_method root) ~equal:Method.equal )
          all_roots
      in
      MethodSelector.select_by_degree
        (vertex_list_to_method_list unasked_indeterminate_roots)
        received_responses snapshot
    in
    Question.AskingForLabel random_root


(** ask a method from a foreign package of its label. *)
let ask_foreign_package_label : rule =
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
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
  if dry_run then Question.dummy
  else
    let all_foreign_package_vertices =
      G.fold_vertex
        (fun vertex acc ->
          let open NodeWiseFeatures in
          if SingleFeature.is_framework_method (Vertex.get_method vertex) then vertex :: acc
          else acc )
        snapshot []
    in
    let random_foreign_method =
      let unasked_indeterminate_roots =
        List.filter
          ~f:(fun vertex ->
            ProbQuadruple.is_indeterminate (Vertex.get_dist vertex)
            && not
               @@ List.mem
                    (received_responses >>| Response.get_method)
                    (Vertex.get_method vertex) ~equal:Method.equal )
          all_foreign_package_vertices
      in
      MethodSelector.select_by_degree
        (vertex_list_to_method_list unasked_indeterminate_roots)
        received_responses snapshot
    in
    Question.AskingForLabel random_foreign_method


let ask_indeterminate : rule =
 (* NOTE this should *NOT* be run at the first round of Loop. *)
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
  let all_indeterminates =
    List.filter (G.all_vertices_of_graph snapshot) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) )
  in
  assert (not @@ List.is_empty all_indeterminates) ;
  if dry_run then Question.dummy
  else
    let random_indeterminate_method =
      let unasked_indeterminates =
        List.filter
          ~f:(fun vertex ->
            not
            @@ List.mem
                 (received_responses >>| Response.get_method)
                 (Vertex.get_method vertex) ~equal:Method.equal )
          all_indeterminates
      in
      MethodSelector.select_by_degree
        (vertex_list_to_method_list unasked_indeterminates)
        received_responses snapshot
    in
    Question.AskingForLabel random_indeterminate_method


let ask_from_ns_cluster_if_it_contains_internal_src_or_sink : rule =
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
  let there_is_some_cluster_that_has_internal_src_or_sink =
    List.for_all (SimilarityHandler.all_ns_clusters snapshot) ~f:(fun ns_cluster ->
        List.exists ns_cluster ~f:(fun vertex ->
            G.is_df_internal (LV.of_vertex vertex) snapshot
            && ( ProbQuadruple.is_source (Vertex.get_dist vertex)
               || ProbQuadruple.is_sin (Vertex.get_dist vertex) ) ) )
  in
  assert there_is_some_cluster_that_has_internal_src_or_sink ;
  if dry_run then Question.dummy
  else
    let not_asked_clusters =
      List.filter (SimilarityHandler.all_ns_clusters snapshot) ~f:(fun cluster ->
          not
          @@ List.exists received_responses ~f:(fun received_response ->
                 List.mem ~equal:Method.equal
                   (List.map ~f:Vertex.get_method cluster)
                   (Response.get_method received_response) )
          && List.exists cluster ~f:(fun vertex ->
                 ProbQuadruple.is_source (Vertex.get_dist vertex)
                 || ProbQuadruple.is_none (Vertex.get_dist vertex) ) )
    in
    let random_cluster = Utils.random_elem not_asked_clusters in
    let random_method_in_picked_cluster =
      let unasked_indeterminates =
        List.filter
          ~f:(fun vertex ->
            ProbQuadruple.is_indeterminate (Vertex.get_dist vertex)
            && not
               @@ List.mem
                    (received_responses >>| Response.get_method)
                    (Vertex.get_method vertex) ~equal:Method.equal )
          random_cluster
      in
      MethodSelector.select_by_degree
        (vertex_list_to_method_list unasked_indeterminates)
        received_responses snapshot
    in
    Question.AskingForLabel random_method_in_picked_cluster


let ask_annotated_method : rule =
 fun (snapshot : G.t) (received_responses : Response.t list) ~(dry_run : bool) : Question.t ->
  (* for annotated methods that both appear at the root and the leaf *)
  let forlabel_response_is_paired_up (forlabel : Response.t) =
    List.exists received_responses ~f:(fun received_response ->
        Response.is_foryesorno received_response
        && Method.equal (Response.get_method forlabel) (Response.get_method received_response) )
  in
  let indeterminate_annotated_methods =
    List.filter ~f:(fun method_ ->
        Annotations.has_annot method_
        && (not @@ List.mem (received_responses >>| Response.get_method) method_ ~equal:Method.equal)
        && List.exists ~f:ProbQuadruple.is_indeterminate
             (G.this_method_vertices snapshot method_ >>| Vertex.get_dist) )
    @@ G.all_methods_of_graph snapshot
  in
  let all_received_forlabels = List.filter received_responses ~f:Response.is_forlabel in
  let all_received_forlabels_are_pairedup =
    let all_received_forlabels_that_are_not_none =
      List.filter all_received_forlabels ~f:(fun forlabel ->
          not @@ TaintLabel.is_none @@ Response.get_label forlabel )
    in
    List.for_all all_received_forlabels_that_are_not_none ~f:forlabel_response_is_paired_up
  in
  assert (
    (not @@ List.is_empty indeterminate_annotated_methods)
    || not all_received_forlabels_are_pairedup ) ;
  print_endline "ask_annotated_method chosen." ;
  if dry_run then Question.dummy
  else
    let all_annotated_vertices =
      List.filter
        ~f:(fun vertex -> Annotations.has_annot (Vertex.get_method vertex))
        (G.all_vertices_of_graph snapshot)
      |> List.filter ~f:(fun vertex ->
             ProbQuadruple.is_indeterminate (Vertex.get_dist vertex)
             && not
                @@ List.mem
                     (received_responses >>| Response.get_method)
                     (Vertex.get_method vertex) ~equal:Method.equal )
    in
    let unpaired_forlabel_opt =
      List.find ~f:(not << forlabel_response_is_paired_up) all_received_forlabels
    in
    match unpaired_forlabel_opt with
    | None ->
        let random_annotated_method =
          MethodSelector.select_by_degree
            (vertex_list_to_method_list all_annotated_vertices)
            received_responses snapshot
        in
        Question.AskingForLabel random_annotated_method
    | Some forlabel -> (
        let forlabel_method = Response.get_method forlabel in
        match Response.get_label forlabel with
        | Source ->
            Question.AskingForConfirmation (forlabel_method, Sink)
        | Sink ->
            Question.AskingForConfirmation (forlabel_method, Source)
        | Sanitizer | None ->
            let random_annotated_method =
              MethodSelector.select_by_degree
                (vertex_list_to_method_list all_annotated_vertices)
                received_responses snapshot
            in
            Question.AskingForLabel random_annotated_method
        | Indeterminate ->
            failwith "WTF" )


let all_rules : t list =
  [ {label= "ask_if_leaf_is_sink"; rule= ask_if_leaf_is_sink}
  ; {label= "ask_if_root_is_source"; rule= ask_if_root_is_source}
  ; {label= "ask_foreign_package_label"; rule= ask_foreign_package_label}
  ; {label= "ask_indeterminate"; rule= ask_indeterminate}
  ; { label= "ask_from_ns_cluster_if_it_contains_internal_src_or_sink"
    ; rule= ask_from_ns_cluster_if_it_contains_internal_src_or_sink }
  ; {label= "ask_annotated_method"; rule= ask_annotated_method} ]
