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
end

module HealTopology = struct
  (* TODO Coming Soon... *)
end
