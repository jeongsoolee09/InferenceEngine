open GraphRepr
open Probability
open RulesOfInference
open FeatureMaps

exception TODO

module Saturation = struct
  let saturated_parameter = 0.2 (* TEMP: subject to change *)

  let dist_is_saturated (quad : ProbQuadruple.t) : bool =
    let sorted = List.sort ~compare:Float.compare [quad.src; quad.sin; quad.san; quad.non] in
    let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
    Float.( >= ) (first -. second) saturated_parameter


  let distmap_is_saturated (distmap : ProbMap.t) : bool =
    ProbMap.for_all (fun _ quad -> dist_is_saturated quad) distmap
end

module In_channel = Core_kernel.In_channel
module Out_channel = Core_kernel.Out_channel

let rec loop (current_distmap : ProbMap.t) (received_responses : Response.t list) (graph : G.t)
    (nodewise_featuremap : FeatureMaps.NodeWiseFeatureMap.t) : ProbMap.t =
  if Saturation.distmap_is_saturated current_distmap then current_distmap
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker = MetaRules.ForAsking.asking_rules_selector graph in
    let question = question_maker graph received_responses nodewise_featuremap in
    let prompt = Question.make_prompt question in
    Out_channel.output_string Out_channel.stdout prompt ;
    let response =
      match In_channel.input_line In_channel.stdin with
      | Some response_str ->
          Response.response_of_string (Question.get_method question) response_str
      | None ->
          failwith "no response ahahahah"
    in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority graph
    in
    let propagated =
      List.fold
        ~f:(fun acc prop_rule -> prop_rule acc response received_responses graph)
        ~init:current_distmap propagation_rules_to_apply
    in
    loop propagated (response :: received_responses) graph nodewise_featuremap
