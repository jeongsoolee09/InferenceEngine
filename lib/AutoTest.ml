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


(* - 루프를 도는데:
   - 현재의 최신 스냅샷에 대한 정확도를 채점하고, determinate한 메소드와 버텍스의 개수를 센다.
   - 물음에 자동으로 답한다 (정답을 답해야 한다).

   ==> 이걸 stdout에도 출력하고, 로그파일에도 쓰게 하자. 야호! *)

(** auto-question-n-answer version of Loop.loop_inner. *)
let rec auto_test_spechunter_for_snapshot (current_snapshot : G.t)
    (received_responses : Response.t list)
    (nodewise_featuremap : NodeWiseFeatures.NodeWiseFeatureMap.t) (count : int) : G.t =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then current_snapshot
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
          Response.ForLabel (meth, Hashtbl.find solution_table meth)
      | AskingForConfirmation (meth, label) ->
          Response.ForLabel (meth, Hashtbl.find solution_table meth)
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
    print_endline
    @@ F.asprintf "%d: %f (v) %f (m)" count
         (get_vertexwise_precision_of_snapshot propagated')
         (get_methodwise_precision_of_snapshot propagated') ;
    auto_test_spechunter_for_snapshot propagated' (response :: received_responses)
      nodewise_featuremap (count + 1)
