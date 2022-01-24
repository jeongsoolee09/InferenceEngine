open ListMonad
open InfixOperators
open GraphRepr
module Hashtbl = Caml.Hashtbl

exception TODO

type axiom = G.t -> G.t

let mark_well_known_java_methods : axiom =
 fun (graph : G.t) : G.t ->
  Out_channel.print_endline "mark_well_kwown_java_methods" ;
  let marked = ref [] in
  graph
  |> (* fold_over_sources *)
  fun graph ->
  G.fold_vertex
    (fun vertex current_graph ->
      if Method.is_well_known_java_source_method (Vertex.get_method vertex) then (
        marked := vertex :: !marked ;
        let new_dist = DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.) in
        G.strong_update_dist vertex new_dist current_graph )
      else current_graph )
    graph graph
  |> (* fold_over_sinks *)
  fun graph ->
  G.fold_vertex
    (fun vertex current_graph ->
      if Method.is_well_known_java_sink_method (Vertex.get_method vertex) then (
        marked := vertex :: !marked ;
        let new_dist = DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.) in
        G.strong_update_dist vertex new_dist current_graph )
      else current_graph )
    graph graph
  |> (* fold_over_sanitizers *)
  fun graph ->
  G.fold_vertex
    (fun vertex current_graph ->
      if Method.is_well_known_java_sanitizer_method (Vertex.get_method vertex) then (
        marked := vertex :: !marked ;
        let new_dist = DistManipulator.overwrite ~src:100. ~sin:(-100.) ~san:(-100.) ~non:(-100.) in
        G.strong_update_dist vertex new_dist current_graph )
      else current_graph )
    graph graph
  |> (* folding over nones *)
  fun graph ->
  let out =
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
  in
  Out_channel.print_endline @@ F.asprintf "marked %d vertices." (List.length !marked) ;
  out


let getters_setters_and_predicates_are_none : axiom =
 fun (graph : G.t) : G.t ->
  Out_channel.print_endline "getters_setters_and_predicates_are_none" ;
  G.fold_vertex
    (fun vertex current_graph ->
      let method_ = Vertex.get_method vertex in
      if
        GetterSetter.is_getter method_ || GetterSetter.is_setter method_
        || GetterSetter.is_predicate method_
      then
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist vertex new_dist current_graph
      else current_graph )
    graph graph


(* oops, this should be run at every loop!!! *)
let sink_can't_be_a_pred_of_sink : axiom =
 fun (graph : G.t) : G.t ->
  Out_channel.print_endline "sink_can't_be_a_pred_of_sink" ;
  let all_sinks =
    G.all_vertices_of_graph graph
    >>| (fun vertex -> (vertex, ProbQuadruple.determine_label (Vertex.get_dist vertex)))
    |> List.filter ~f:(fun (vertex, label) -> TaintLabel.equal label TaintLabel.Sink)
    >>| fst
  in
  List.fold
    ~f:(fun big_acc sink_vertex ->
      let recursive_df_preds =
        get_recursive_preds graph (G.LiteralVertex.of_vertex sink_vertex) ~label:EdgeLabel.DataFlow
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
  Out_channel.print_endline "init_that_doesn't_call_lib_code_is_none" ;
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
            not @@ NodeWiseFeatures.SingleFeature.is_library_code (Vertex.get_method vertex) ) )
  in
  List.fold all_init_vertices_with_no_immediate_lib_code
    ~f:(fun acc init_vertex ->
      let init_vertex_dist = Vertex.get_dist init_vertex in
      let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
      G.strong_update_dist init_vertex new_dist acc )
    ~init:graph


let this_project_main_is_none : axiom =
 fun (graph : G.t) : G.t ->
  Out_channel.print_endline "this_project_main_is_none" ;
  let marked = ref [] in
  let this_project_main_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if Method.is_main_method (Vertex.to_string vertex) then vertex :: acc else acc )
      graph []
  in
  let out =
    List.fold this_project_main_vertices
      ~f:(fun acc main_vertex ->
        marked := main_vertex :: !marked ;
        let main_vertex_dist = Vertex.get_dist main_vertex in
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist main_vertex new_dist acc )
      ~init:graph
  in
  Out_channel.print_endline @@ F.asprintf "marked %d vertices." (List.length !marked) ;
  out


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
          and is_udf =
            let meth = fst vertex in
            Array.exists ~f:Method.is_udf all_udfs
          in
          data_flows_in && data_flows_out && is_udf
        in
        Hashtbl.add cache (graph, vertex) out ;
        out
    | Some res ->
        res


let internal_udf_vertex_is_none : axiom =
 fun (graph : G.t) : G.t ->
  Out_channel.print_endline "internal_udf_vertex_is_none" ;
  let internal_udf_vertices_without_annot =
    G.fold_vertex
      (fun vertex acc ->
        if
          Method.is_udf (Vertex.get_method vertex)
          && G.is_df_internal (G.LiteralVertex.of_vertex vertex) graph
          &&
          let annot = vertex |> Vertex.get_method |> Annotations.get_annots in
          Annotations.equal annot Annotations.empty
        then vertex :: acc
        else acc )
      graph []
  in
  let propagated =
    List.fold
      ~f:(fun acc succ ->
        let succ_dist = Vertex.get_dist succ in
        let new_dist = DistManipulator.overwrite ~src:(-100.) ~sin:(-100.) ~san:(-100.) ~non:100. in
        G.strong_update_dist succ new_dist acc )
      ~init:graph internal_udf_vertices_without_annot
  in
  Out_channel.print_endline
  @@ F.asprintf "marked %d vertices." (List.length internal_udf_vertices_without_annot) ;
  propagated


let all_axioms : axiom array =
  [| mark_well_known_java_methods
   ; getters_setters_and_predicates_are_none
   ; sink_can't_be_a_pred_of_sink
   ; init_that_doesn't_call_lib_code_is_none
   ; this_project_main_is_none
   ; internal_udf_vertex_is_none |]


let apply_axioms (graph : G.t) : G.t =
  Array.fold all_axioms ~f:(fun acc axiom -> axiom acc) ~init:graph
