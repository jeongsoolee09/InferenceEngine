open GraphRepr
open Probability
open RulesOfInference
open FeatureMaps

exception TODO

module In_channel = Core_kernel.In_channel
module Out_channel = Core_kernel.Out_channel

module Saturation = struct
  let saturated_parameter = 0.2 (* TEMP: subject to change *)

  let dist_is_saturated (quad : ProbQuadruple.t) : bool =
    let sorted = List.sort ~compare:Float.compare [quad.src; quad.sin; quad.san; quad.non] in
    let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
    Float.( >= ) (first -. second) saturated_parameter


  let distmap_is_saturated (distmap : ProbMap.t) : bool =
    ProbMap.for_all (fun _ quad -> dist_is_saturated quad) distmap
end

let rec loop (current_distmap : ProbMap.t) (received_responses : Response.t list) (graph : G.t)
    (nodewise_featuremap : FeatureMaps.NodeWiseFeatureMap.t) (count : int) : ProbMap.t =
  if Saturation.distmap_is_saturated current_distmap then current_distmap
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      (* MetaRules.ForAsking.asking_rules_selector graph received_responses nodewise_featuremap *)
      (* TEMP Hardcoded *)
      AskingRules.ask_if_leaf_is_sink
    in
    let question = question_maker graph received_responses nodewise_featuremap in
    let prompt = Question.make_prompt question in
    Out_channel.output_string Out_channel.stdout prompt ;
    Out_channel.flush Out_channel.stdout ;
    let response =
      match In_channel.input_line In_channel.stdin with
      | Some response_str -> (
        match question with
        | AskingForLabel meth ->
            Response.response_of_string_forlabel meth response_str
        | AskingForConfirmation (meth, label) ->
            Response.response_of_string_foryesorno meth label response_str )
      | None ->
          failwith "no response ahahahah"
    in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority current_distmap response
        received_responses graph
    in
    let propagated =
      List.fold
        ~f:(fun acc prop_rule -> fst @@ prop_rule acc response received_responses graph)
        ~init:current_distmap propagation_rules_to_apply
    in
    loop propagated (response :: received_responses) graph nodewise_featuremap (count + 1)


module Visualizer = struct
  (** (1) output a dot file of this snapshot, (2) render a svg off the dot file, and (3) show the
      svg file. *)
  let visualize_at_the_face (snapshot : G.t) : unit =
    let open MakeGraph.GraphMaker in
    let now_timestring = make_now_string () in
    graph_to_dot snapshot ~filename:(F.asprintf "%s.dot" now_timestring) ;
    let dot_in_chan, dot_out_chan =
      Unix.open_process (F.asprintf "dot -Tsvg -o %s.svg %s.dot" now_timestring now_timestring)
    in
    In_channel.close dot_in_chan ;
    Out_channel.close dot_out_chan ;
    let open_in_chan, open_out_chan = Unix.open_process (F.asprintf "open %s.svg" now_timestring) in
    In_channel.close open_in_chan ;
    Out_channel.close open_out_chan
end
