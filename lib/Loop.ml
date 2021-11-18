open GraphRepr
open Probability
open RulesOfInference

exception TODO

module Saturation = struct
  let saturated_parameter = 0.2 (* TEMP *)

  let dist_is_saturated (quad : ProbQuadruple.t) : bool =
    let sorted = List.sort ~compare:Float.compare [quad.src; quad.sin; quad.san; quad.non] in
    let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
    Float.( >= ) (first -. second) saturated_parameter


  let distmap_is_saturated (distmap : ProbMap.t) : bool =
    ProbMap.for_all (fun _ quad -> dist_is_saturated quad) distmap
end

module In_channel = Core_kernel.In_channel

let loop (current_distmap : ProbMap.t) (received_responses : Response.t list) (graph : G.t) :
    ProbMap.t =
  if Saturation.distmap_is_saturated current_distmap then current_distmap
  else
    let prompt = Question.make_prompt @@ raise TODO in
    let response = In_channel.input_line In_channel.stdin in
    raise TODO
