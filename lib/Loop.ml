open GraphRepr
open ListMonad
open Propagator
open RulesOfInference
(* open FeatureMaps *)

exception TODO

module In_channel = Core_kernel.In_channel
module Out_channel = Core_kernel.Out_channel

module Visualizer = struct
  (** (1) output a dot file of this snapshot, (2) render a svg off the dot file, and (3) show the
      svg file. *)
  let visualize_snapshot (snapshot : G.t) ~(micro : bool) ~(autoopen : bool) : unit =
    let open DataFlowEdges in
    let now_timestring = make_now_string 9 in
    let filename_without_extension =
      if micro then F.asprintf "%s_micro" now_timestring else now_timestring
    in
    graph_to_dot snapshot ~filename:(filename_without_extension ^ ".dot") ;
    ignore
    @@ Unix.system
         (F.asprintf "dot -Tsvg -o %s.svg %s.dot" filename_without_extension
            filename_without_extension ) ;
    if autoopen then ignore @@ Unix.system (F.asprintf "open %s.svg" filename_without_extension)
end

let rec loop_inner (current_snapshot : G.t) (received_responses : Response.t list)
    (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) : G.t =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then current_snapshot
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
    in
    let question = question_maker.rule current_snapshot received_responses in
    let prompt = Question.make_prompt question in
    Out_channel.output_string Out_channel.stdout prompt ;
    Out_channel.flush Out_channel.stdout ;
    let input = In_channel.input_line In_channel.stdin in
    if Option.is_none input || String.equal (Option.value_exn input) "stop" then current_snapshot
    else
      let response =
        match input with
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
        MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
      in
      let propagated =
        fst
        @@ propagator response current_snapshot propagation_rules_to_apply received_responses []
             PropagationRules.all_rules
      in
      let propagated' = Axioms.apply_axioms propagated in
      Visualizer.visualize_snapshot propagated' ~micro:false ~autoopen:true ;
      loop_inner propagated' (response :: received_responses) nodewise_featuremap


let loop (current_snapshot : G.t) (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t)
    ~(auto_test : bool) =
  print_endline "Starting question-&-answer loop." ;
  G.snapshot_to_json current_snapshot ;
  if auto_test then
    fst
    @@ AutoTest.auto_test_spechunter_for_snapshot_inner current_snapshot [] nodewise_featuremap 0 []
  else loop_inner current_snapshot [] nodewise_featuremap
