open InfixOperators
open ListMonad
open GraphRepr
open TaintLabel
open RulesOfInference
open Propagator
module Hashtbl = Caml.Hashtbl

let prepare_solution (raw_solution : (string * string list) array) :
    (string, TaintLabel.t list) Hashtbl.t =
  let acc = Hashtbl.create 777 in
  Array.iter
    ~f:(fun (method_, label_strs) ->
      Hashtbl.add acc method_
        ( label_strs
        >>| fun label_str ->
        match label_str with
        | "src" ->
            Source
        | "sin" ->
            Sink
        | "san" ->
            Sanitizer
        | "non" ->
            None
        | otherwise ->
            raise @@ Invalid_argument otherwise ) )
    raw_solution ;
  acc


let solution_table = prepare_solution @@ Deserializer.deserialize_solution ()

let get_solution (method_ : Method.t) : TaintLabel.t list = Hashtbl.find solution_table method_

let vertex_inference_result_is_correct ~(inferred : TaintLabel.t) ~(ground : TaintLabel.t list) :
    bool =
  List.mem ground inferred ~equal:TaintLabel.equal


let method_inference_result_is_correct ~(inferred : TaintLabel.t list) ~(ground : TaintLabel.t list)
    : bool =
  List.fold
    ~f:(fun acc inferred_label ->
      if TaintLabel.is_none inferred_label then acc
      else
        let inferred_label_is_correct = List.mem ground inferred_label ~equal:TaintLabel.equal in
        inferred_label_is_correct && acc )
    ~init:true inferred


(** get the percentage of correct vertices in this snapshot. *)
let get_vertexwise_precision_of_snapshot (snapshot : G.t) : Float.t =
  let correct_vertices_count =
    G.fold_vertex
      (fun vertex acc ->
        if
          vertex_inference_result_is_correct
            ~inferred:(ProbQuadruple.determine_label (Vertex.get_dist vertex))
            ~ground:(get_solution (Vertex.get_method vertex))
        then acc + 1
        else acc )
      snapshot 0
  in
  let num_of_all_vertices = List.length @@ G.all_vertices_of_graph snapshot in
  Float.of_int (correct_vertices_count / num_of_all_vertices) *. 100.


(** get the percentage of correct methods in this snapshot. *)
let get_methodwise_precision_of_snapshot (snapshot : G.t) : Float.t =
  let correct_methods_count =
    List.fold
      ~f:(fun acc method_ ->
        let all_inferred_labels_for_method =
          G.this_method_vertices snapshot method_
          >>| Vertex.get_dist >>| ProbQuadruple.determine_label
        in
        if
          method_inference_result_is_correct ~inferred:all_inferred_labels_for_method
            ~ground:(get_solution method_)
        then acc + 1
        else acc )
      (G.all_methods_of_graph snapshot) ~init:0
  in
  let num_of_all_methods = List.length @@ G.all_methods_of_graph snapshot in
  Float.of_int (correct_methods_count / num_of_all_methods) *. 100.


(** auto-question-n-answer version of Loop.loop_inner. *)
let rec auto_test_spechunter_for_snapshot_inner (current_snapshot : G.t)
    (received_responses : Response.t list)
    (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) (count : int)
    (log_data_acc : string list) : G.t * string =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
    (current_snapshot, List.rev log_data_acc |> List.map ~f:(fun str -> str ^ "\n") |> String.concat)
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
        nodewise_featuremap
    in
    let question = question_maker.rule current_snapshot received_responses nodewise_featuremap in
    let response =
      match question with
      | AskingForLabel meth ->
          Response.ForLabel (meth, get_solution meth)
      | AskingForConfirmation (meth, label) ->
          Response.ForLabel (meth, get_solution meth)
    in
    (* sort applicable Propagation Rules by adequacy. *)
    let propagation_rules_to_apply =
      MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
        received_responses
    in
    let propagated =
      fst
      @@ propagator response current_snapshot None propagation_rules_to_apply received_responses []
           PropagationRules.all_rules
    in
    let propagated' = Axioms.apply_axioms propagated in
    (* output to stdout *)
    let stats =
      F.asprintf "%d: %f (v), %f (m)" count
        (get_vertexwise_precision_of_snapshot propagated')
        (get_methodwise_precision_of_snapshot propagated')
    in
    print_endline stats ;
    auto_test_spechunter_for_snapshot_inner propagated' (response :: received_responses)
      nodewise_featuremap (count + 1) (stats :: log_data_acc)


let auto_test (initial_snapshot : G.t) : unit =
  let loop_finished_snapshot, log_data =
    auto_test_spechunter_for_snapshot_inner initial_snapshot []
      NodeWiseFeatures.NodeWiseFeatureMap.empty 0 []
  in
  let out_chan =
    Out_channel.create @@ F.asprintf "%s_%s.txt" (make_now_string 9) initial_snapshot.comp_unit
  in
  Out_channel.output_string out_chan log_data ;
  Out_channel.close out_chan
