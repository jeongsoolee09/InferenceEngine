open ListMonad
open InfixOperators
open GraphRepr
module Hashtbl = Caml.Hashtbl

exception TODO

type axiom = G.t -> G.t

let is_library_code =
  let cache = Hashtbl.create 777 in
  fun (method_ : Method.t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        let out =
          let classname = Method.get_class_name method_
          and methname = Method.get_method_name method_ in
          let classname_methname = F.asprintf "%s.%s" classname methname in
          List.exists
            ~f:(fun line -> String.is_substring ~substring:classname_methname line)
            (Deserializer.deserialize_skip_func ())
        in
        Hashtbl.add cache method_ out ;
        out
    | Some res ->
        res


module Distribution = struct
  let mark_well_known_java_methods : axiom =
   fun (graph : G.t) : G.t ->
    (* Out_channel.print_endline "mark_well_kwown_java_methods" ; *)
    let marked = ref [] in
    graph
    |> (* fold_over_sources *)
    fun graph ->
    G.fold_vertex
      (fun vertex current_graph ->
        if Method.is_well_known_java_source_method (Vertex.get_method vertex) then (
          marked := vertex :: !marked ;
          let new_dist =
            DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.)
          in
          G.strong_update_dist vertex new_dist current_graph )
        else current_graph )
      graph graph
    |> (* fold_over_sinks *)
    fun graph ->
    G.fold_vertex
      (fun vertex current_graph ->
        if Method.is_well_known_java_sink_method (Vertex.get_method vertex) then (
          marked := vertex :: !marked ;
          let new_dist =
            DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.)
          in
          G.strong_update_dist vertex new_dist current_graph )
        else current_graph )
      graph graph
    |> (* fold_over_sanitizers *)
    fun graph ->
    G.fold_vertex
      (fun vertex current_graph ->
        if Method.is_well_known_java_sanitizer_method (Vertex.get_method vertex) then (
          marked := vertex :: !marked ;
          let new_dist =
            DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.)
          in
          G.strong_update_dist vertex new_dist current_graph )
        else current_graph )
      graph graph
    |> (* folding over nones *)
    fun graph ->
    G.fold_vertex
      (fun vertex current_graph ->
        if Method.is_well_known_java_none_method (Vertex.get_method vertex) then (
          marked := vertex :: !marked ;
          let new_dist =
            DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100.
          in
          G.strong_update_dist vertex new_dist current_graph )
        else current_graph )
      graph graph


  let getters_setters_predicates_equals_are_none : axiom =
   fun (graph : G.t) : G.t ->
    G.fold_vertex
      (fun vertex current_graph ->
        let method_ = Vertex.get_method vertex in
        if
          GetterSetter.is_getter method_ || GetterSetter.is_setter method_
          || GetterSetter.is_predicate method_
          || GetterSetter.is_user_defined_equals method_
        then
          let new_dist =
            DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100.
          in
          G.strong_update_dist vertex new_dist current_graph
        else current_graph )
      graph graph


  (* oops, this should be run at every loop!!! *)
  let sink_can't_be_a_pred_of_sink : axiom =
   fun (graph : G.t) : G.t ->
    let all_sinks =
      G.all_vertices_of_graph graph
      >>| (fun vertex -> (vertex, ProbQuadruple.determine_label (Vertex.get_dist vertex)))
      |> List.filter ~f:(fun (_, label) -> TaintLabel.equal label TaintLabel.Sink)
      >>| fst
    in
    List.fold
      ~f:(fun big_acc sink_vertex ->
        let recursive_df_preds =
          List.map ~f:fst
          @@ get_recursive_preds graph
               (G.LiteralVertex.of_vertex sink_vertex)
               ~label:EdgeLabel.DataFlow
        in
        List.fold
          ~f:(fun smol_acc pred_vertex ->
            if
              TaintLabel.equal
                (ProbQuadruple.determine_label (Vertex.get_dist pred_vertex))
                TaintLabel.Sink
            then
              let pred_vertex_dist = Vertex.get_dist pred_vertex in
              (* Uh-oh. Time to amend *)
              let new_dist = {pred_vertex_dist with sin= pred_vertex_dist.sin -. 0.3} in
              G.strong_update_dist pred_vertex new_dist graph
            else smol_acc )
          ~init:big_acc recursive_df_preds )
      ~init:graph all_sinks


  (* oops, this too should be run at every loop!!! *)
  let init_that_doesn't_call_lib_code_is_none : axiom =
   fun (graph : G.t) : G.t ->
    let all_init_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun vertex ->
             let vertex_method = Vertex.get_method vertex in
             Method.is_initializer vertex_method )
    in
    let all_init_vertices_with_no_immediate_lib_code =
      List.filter all_init_vertices ~f:(fun vertex ->
          let df_succ_vertices =
            G.get_succs graph (G.LiteralVertex.of_vertex vertex) ~label:EdgeLabel.DataFlow
          in
          List.for_all df_succ_vertices ~f:(fun vertex ->
              not @@ is_library_code (Vertex.get_method vertex) ) )
    in
    List.fold all_init_vertices_with_no_immediate_lib_code
      ~f:(fun acc init_vertex ->
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist init_vertex new_dist acc )
      ~init:graph


  let this_project_main_is_none : axiom =
   fun (graph : G.t) : G.t ->
    let marked = ref [] in
    let this_project_main_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if Method.is_main_method (Vertex.to_string vertex) then vertex :: acc else acc )
        graph []
    in
    List.fold this_project_main_vertices
      ~f:(fun acc main_vertex ->
        marked := main_vertex :: !marked ;
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist main_vertex new_dist acc )
      ~init:graph


  let is_internal_udf_vertex =
    let cache = Hashtbl.create 777 in
    let all_udfs = Array.of_list (Deserializer.deserialize_method_txt ()) in
    fun (graph : G.t) (vertex : G.LiteralVertex.t) ->
      match Hashtbl.find_opt cache (graph, vertex) with
      | None ->
          let out =
            let data_flows_in =
              Int.( >= ) (List.length @@ G.get_preds graph vertex ~label:EdgeLabel.DataFlow) 1
            and data_flows_out =
              Int.( >= ) (List.length @@ G.get_succs graph vertex ~label:EdgeLabel.DataFlow) 1
            and is_udf = Array.exists ~f:Method.is_udf all_udfs in
            data_flows_in && data_flows_out && is_udf
          in
          Hashtbl.add cache (graph, vertex) out ;
          out
      | Some res ->
          res


  let internal_udf_vertex_is_none : axiom =
   fun (graph : G.t) : G.t ->
    let internal_udf_vertices_without_annot =
      G.fold_vertex
        (fun vertex acc ->
          if
            Method.is_udf (Vertex.get_method vertex)
            && G.is_df_internal graph (G.LiteralVertex.of_vertex vertex)
            &&
            let annot = vertex |> Vertex.get_method |> Annotations.get_annots in
            Annotations.equal annot Annotations.empty
          then vertex :: acc
          else acc )
        graph []
    in
    List.fold
      ~f:(fun acc succ ->
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist succ new_dist acc )
      ~init:graph internal_udf_vertices_without_annot


  let api_that_has_prefix_get_and_intype_is_void_is_none : axiom =
   fun (graph : G.t) : G.t ->
    let is_api_with_get_and_no_intypes (method_ : Method.t) : bool =
      Method.is_api method_
      && String.is_prefix (Method.get_method_name method_) ~prefix:"get"
      && String.is_substring method_ ~substring:"()"
    in
    let all_apis_with_get_and_no_intypes =
      List.filter (G.all_methods_of_graph graph) ~f:is_api_with_get_and_no_intypes
    in
    List.fold
      (all_apis_with_get_and_no_intypes >>= G.this_method_vertices graph)
      ~f:(fun acc api ->
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist api new_dist acc )
      ~init:graph


  let api_that_has_prefix_set_and_rtntype_is_void_is_none : axiom =
   fun (graph : G.t) : G.t ->
    let is_api_with_set_prefix_set_and_void_rtntype (method_ : Method.t) : bool =
      Method.is_api method_
      && String.is_prefix (Method.get_method_name method_) ~prefix:"set"
      && String.equal (Method.get_return_type method_) "void"
    in
    let all_apis_with_set_and_void_rtntypes =
      List.filter (G.all_methods_of_graph graph) ~f:is_api_with_set_prefix_set_and_void_rtntype
    in
    List.fold
      (all_apis_with_set_and_void_rtntypes >>= G.this_method_vertices graph)
      ~f:(fun acc api ->
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist api new_dist acc )
      ~init:graph
end

module Topology = struct
  let api_inits_are_not_contextually_similar_to_non_inits : axiom =
   fun (graph : G.t) : G.t ->
    G.fold_edges_e
      (fun ((v1, _, v2) as edge) current_graph ->
        let v1_is_api_init =
          Method.is_api (Vertex.get_method v1) && Method.is_initializer (Vertex.get_method v1)
        and v2_is_api_init =
          Method.is_api (Vertex.get_method v2) && Method.is_initializer (Vertex.get_method v2)
        in
        if v1_is_api_init || v2_is_api_init then G.remove_edge_e current_graph edge
        else current_graph )
      graph graph
end

let all_distribution_axioms : axiom array =
  [| (*Distribution.mark_well_known_java_methods
       ;*)
     Distribution.getters_setters_predicates_equals_are_none
   ; Distribution.sink_can't_be_a_pred_of_sink
   ; Distribution.init_that_doesn't_call_lib_code_is_none
   ; Distribution.this_project_main_is_none
   ; Distribution.internal_udf_vertex_is_none |]


let all_topology_axioms : axiom array =
  [|Topology.api_inits_are_not_contextually_similar_to_non_inits|]


let all_axioms : axiom array = Array.append all_distribution_axioms all_topology_axioms

let apply_distribution_axioms (graph : G.t) : G.t =
  Array.fold all_distribution_axioms ~f:(fun acc axiom -> axiom acc) ~init:graph


let apply_topology_axioms (graph : G.t) : G.t =
  Array.fold all_topology_axioms ~f:(fun acc axiom -> axiom acc) ~init:graph


let apply_axioms (graph : G.t) : G.t =
  Array.fold all_axioms ~f:(fun acc axiom -> axiom acc) ~init:graph
