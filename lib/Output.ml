open InfixOperators
open GraphRepr
open Utils
module JSON = Yojson.Basic

let merge_labels_for_method (graph : G.t) (method_ : Method.t) : TaintLabel.t list =
  let this_method_vertices = G.this_method_vertices graph method_ in
  List.stable_dedup
  @@ List.map ~f:(ProbQuadruple.determine_label << Vertex.get_dist) this_method_vertices


let make_report (loop_finished_graph : G.t) : G.t * (Method.t, TaintLabel.t list) Hashtbl.t =
  let acc = Hashtbl.create 777 in
  List.iter (G.all_methods_of_graph loop_finished_graph) ~f:(fun method_ ->
      Hashtbl.add acc method_ (merge_labels_for_method loop_finished_graph method_) ) ;
  (loop_finished_graph, acc)


let to_json_repr ((graph : G.t), (table : (Method.t, TaintLabel.t list) Hashtbl.t)) :
    string * JSON.t =
  ( graph.comp_unit
  , `Assoc
      [ ( graph.comp_unit
        , `Assoc
            (Hashtbl.fold
               (fun method_ label_list acc ->
                 ( method_
                 , `List
                     (List.map label_list ~f:(fun label ->
                          `String (TaintLabel.to_string_short label) ) ) )
                 :: acc )
               table [] ) ) ] )


let output_json (filename, json) =
  let out_chan = Out_channel.create @@ F.asprintf "%s_inference_results.json" filename in
  JSON.pretty_to_channel out_chan json ;
  Out_channel.close out_chan


let output_result : G.t -> unit = make_report >> to_json_repr >> output_json
