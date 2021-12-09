open ListMonad
open InfixOperators
open GraphRepr

exception TODO

module HealMisPropagation = struct
  let sink_can't_be_a_pred_of_sink (graph : G.t) : G.t =
    let all_sinks =
      G.all_vertices_of_graph graph
      >>| (fun vertex -> (vertex, ProbQuadruple.determine_label (trd3 vertex)))
      |> List.filter ~f:(fun (vertex, label) -> TaintLabel.equal label TaintLabel.Sink)
      >>| fst
    in
    List.fold
      ~f:(fun big_acc sink_vertex ->
        let recursive_df_preds =
          recursively_find_preds graph
            (G.LiteralVertex.of_vertex sink_vertex)
            ~label:EdgeLabel.DataFlow
        in
        List.fold
          ~f:(fun smol_acc pred_vertex ->
            if TaintLabel.equal (ProbQuadruple.determine_label (trd3 pred_vertex)) TaintLabel.Sink
            then
              let pred_vertex_dist = trd3 pred_vertex in
              (* Uh-oh. Time to amend *)
              let new_dist = {pred_vertex_dist with sin= pred_vertex_dist.sin -. 0.3} in
              G.strong_update_dist pred_vertex new_dist graph
            else smol_acc )
          ~init:big_acc recursive_df_preds )
      ~init:graph all_sinks


  let init_that_doesn't_call_lib_code_is_none (graph : G.t) : G.t =
    let all_init_vertices =
      G.all_vertices_of_graph graph
      |> List.filter ~f:(fun vertex ->
             let vertex_method = fst3 vertex in
             String.is_substring ~substring:"<init>" vertex_method )
    in
    let all_init_vertices_with_no_immediate_lib_code =
      List.filter all_init_vertices ~f:(fun vertex ->
          let df_succ_vertices =
            G.get_succs graph (G.LiteralVertex.of_vertex vertex) ~label:EdgeLabel.DataFlow
          in
          List.for_all df_succ_vertices ~f:(fun vertex ->
              not @@ NodeWiseFeatures.SingleFeature.bool_of_feature
              @@ NodeWiseFeatures.SingleFeature.is_library_code (fst3 vertex) ) )
    in
    List.fold all_init_vertices_with_no_immediate_lib_code
      ~f:(fun acc init_vertex ->
        let init_vertex_dist = trd3 init_vertex in
        let new_dist =
          { ProbQuadruple.src= init_vertex_dist.src -. 0.1
          ; ProbQuadruple.sin= init_vertex_dist.sin -. 0.1
          ; ProbQuadruple.san= init_vertex_dist.san -. 0.1
          ; ProbQuadruple.non= init_vertex_dist.non +. 0.3 }
        in
        G.strong_update_dist init_vertex new_dist acc )
      ~init:graph


  let this_project_main_is_none (graph : G.t) : G.t =
    let this_project_main_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if not @@ String.is_substring ~substring:"main(" (fst3 vertex) then acc
          else
            let vertex_method_id =
              NodeWiseFeatures.SingleFeature.find_unique_identifier_of_methstring (fst3 vertex)
            in
            let vertex_method_package =
              NodeWiseFeatures.SingleFeature.string_of_feature
              @@ NodeWiseFeatures.SingleFeature.extract_package_name_from_id vertex_method_id
            in
            if
              String.equal vertex_method_package
                NodeWiseFeatures.SingleFeature.this_project_package_name
            then vertex :: acc
            else acc )
        graph []
    in
    List.fold this_project_main_vertices
      ~f:(fun acc main_vertex ->
        let main_vertex_dist = trd3 main_vertex in
        let new_dist =
          { ProbQuadruple.src= main_vertex_dist.src -. 0.1
          ; ProbQuadruple.sin= main_vertex_dist.sin -. 0.1
          ; ProbQuadruple.san= main_vertex_dist.san -. 0.1
          ; ProbQuadruple.non= main_vertex_dist.non +. 0.3 }
        in
        G.strong_update_dist main_vertex new_dist acc )
      ~init:graph


  let heal_all (graph : G.t) : G.t =
    List.fold
      [ sink_can't_be_a_pred_of_sink
      ; init_that_doesn't_call_lib_code_is_none
      ; this_project_main_is_none ]
      ~f:(fun acc healer -> healer acc)
      ~init:graph
end

(* 가장 직관적인 건, sink_can't_be_a_pred_of_sink를 매 loop iteration마다 쓰는 것. *)

module HealTopology = struct
  (* TODO Coming Soon... *)
end
