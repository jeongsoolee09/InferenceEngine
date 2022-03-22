open InfixOperators
open ListMonad
open Utils
open GraphRepr
module ResultMap = Caml.Map.Make (Method)
module JSON = Yojson.Basic

module DistResultMap = struct
  include ResultMap

  type t = ProbQuadruple.t list ResultMap.t
end

module LabelResultMap = struct
  include ResultMap

  type t = TaintLabel.t list ResultMap.t

  let merge_two_maps (map1 : t) (map2 : t) : t =
    fold (fun k v current_map -> add k v current_map) map2 map1
end

let merge_dists_for_method (graph : G.t) (method_ : Method.t) : ProbQuadruple.t list =
  let this_method_vertices = G.this_method_vertices graph method_ in
  List.stable_dedup @@ List.map ~f:Vertex.get_dist this_method_vertices


(* kernel function *)
let dist_list_to_label_list (dists : ProbQuadruple.t list) : TaintLabel.t list =
  dists >>| ProbQuadruple.determine_label |> List.stable_dedup


let dist_map_to_label_map (dist_result_map : DistResultMap.t) : LabelResultMap.t =
  LabelResultMap.map dist_list_to_label_list dist_result_map


let make_dist_result_map (loop_finished_graph : G.t) : DistResultMap.t =
  List.fold
    (G.all_methods_of_graph loop_finished_graph)
    ~f:(fun map_acc method_ ->
      ResultMap.add method_ (merge_dists_for_method loop_finished_graph method_) map_acc )
    ~init:ResultMap.empty


let make_label_result_map (loop_finished_graph : G.t) : LabelResultMap.t =
  dist_map_to_label_map @@ make_dist_result_map loop_finished_graph


module Serializer = struct
  let to_json_repr (graph : G.t) (table : LabelResultMap.t) : JSON.t =
    `Assoc
      [ ( graph.comp_unit
        , `Assoc
            (LabelResultMap.fold
               (fun method_ label_list acc ->
                 ( method_
                 , `List
                     (List.map label_list ~f:(fun label ->
                          `String (TaintLabel.to_string_short label) ) ) )
                 :: acc )
               table [] ) ) ]


  let output_json filename json =
    let out_chan = Out_channel.create @@ F.asprintf "%s_inference_results.json" filename in
    JSON.pretty_to_channel out_chan json ;
    Out_channel.close out_chan


  let output_result (graph : G.t) : unit =
    let dist_result_map = make_dist_result_map graph in
    let label_result_map = dist_map_to_label_map dist_result_map in
    let label_result_map_json_repr = to_json_repr graph label_result_map in
    output_json graph.comp_unit label_result_map_json_repr
end

let deserialize_label_result_map (filename : string) (comp_unit : string) : LabelResultMap.t =
  let in_channel = In_channel.create filename in
  let json_deserialized = JSON.from_channel in_channel in
  In_channel.close in_channel ;
  let this_comp_unit_contents = JSON.Util.member comp_unit json_deserialized in
  let all_methods = JSON.Util.keys this_comp_unit_contents in
  List.fold all_methods
    ~f:(fun acc method_ ->
      LabelResultMap.add method_
        (List.map
           ~f:(JSON.to_string >> TaintLabel.of_string)
           (JSON.Util.to_list @@ JSON.Util.member method_ this_comp_unit_contents) )
        acc )
    ~init:LabelResultMap.empty
