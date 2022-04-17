open InfixOperators
open ListMonad
open GraphRepr
open TaintLabel
open Propagator
open Utils
open AutoTest
module Hashtbl = Caml.Hashtbl

let manual_test_spechunter_for_snapshot_once (current_snapshot : G.t)
    (received_responses : Response.t list) (to_ask_question : Question.t) =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
    (current_snapshot, received_responses)
  else
    let response = responder to_ask_question in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
    in
    let propagated =
      fst
      @@ propagator response current_snapshot propagation_rules_to_apply received_responses [||]
           PropagationRules.all_rules
    in
    let propagated' = Axioms.apply_axioms propagated in
    let stats =
      let correct_vertices_count =
        G.fold_vertex
          (fun vertex acc ->
            if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
          propagated' 0
      in
      F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
        correct_vertices_count (G.nb_vertex propagated')
        (Scoring.get_vertexwise_precision_of_snapshot propagated')
        ( List.length
        @@ List.filter
             (G.all_vertices_of_graph propagated')
             ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
        (G.nb_vertex propagated')
    in
    print_endline stats ;
    let srm_stats =
      let correct_srms_count, all_srms_count, accuracy =
        Scoring.srm_report_of_snapshot propagated'
      in
      F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
    in
    print_endline srm_stats ;
    print_endline @@ srm_map_of_snapshot propagated' ;
    (propagated', response :: received_responses)


(** test the methods for initial snapshots repeatedly. *)
let manual_test_queue_initial (initial_snapshot : G.t) (methods_to_ask : Method.t list) : unit =
  List.iteri
    ~f:(fun round method_to_ask ->
      print_endline
      @@ F.asprintf "======================== Round %d: Asking for %s ======================== "
           round method_to_ask ;
      let once_result =
        Axioms.apply_axioms @@ fst
        @@ manual_test_spechunter_for_snapshot_once initial_snapshot []
             (Question.AskingForLabel method_to_ask)
      in
      let stats =
        let correct_vertices_count =
          G.fold_vertex
            (fun vertex acc ->
              if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
            once_result 0
        in
        F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
          correct_vertices_count (G.nb_vertex once_result)
          (Scoring.get_vertexwise_precision_of_snapshot once_result)
          ( List.length
          @@ List.filter
               (G.all_vertices_of_graph once_result)
               ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
          (G.nb_vertex once_result)
      in
      print_endline stats ;
      let srm_stats =
        let correct_srms_count, all_srms_count, accuracy =
          Scoring.srm_report_of_snapshot once_result
        in
        F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
      in
      print_endline srm_stats )
    methods_to_ask
