(* Module for easy debugging. *)

open InfixOperators
open ListMonad
open GraphRepr
open TaintLabel
open RulesOfInference
open Propagator
open Utils
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


let solution_table =
  prepare_solution
  @@ Array.concat
       [ Sagan_solution.sagan_udf_solution
       ; Sagan_solution.sagan_api_solution
       ; Sagan_solution.sagan_interface_solution ]


let get_solution (method_ : Method.t) : TaintLabel.t list =
  if Method.is_initializer method_ then [None] else Hashtbl.find solution_table method_


let vertex_inference_result_is_correct (vertex : Vertex.t) : bool =
  let ground = get_solution (Vertex.get_method vertex)
  and inferred = ProbQuadruple.determine_label (Vertex.get_dist vertex) in
  List.mem ground inferred ~equal:TaintLabel.equal


(** get the percentage of correct vertices in this snapshot. *)
let get_vertexwise_precision_of_snapshot (snapshot : G.t) : Float.t =
  let correct_vertices_count =
    G.fold_vertex
      (fun vertex acc -> if vertex_inference_result_is_correct vertex then acc + 1 else acc)
      snapshot 0
  in
  let num_of_all_vertices = List.length @@ G.all_vertices_of_graph snapshot in
  Float.of_int correct_vertices_count /. Float.of_int num_of_all_vertices *. 100.


let watch (snapshot : G.t) (methods : Method.t list) (count : int) : unit =
  let vertices_to_watch = methods >>= G.this_method_vertices snapshot in
  let txt_file = Out_channel.create @@ F.asprintf "%d_%s.txt" count snapshot.comp_unit in
  List.iter vertices_to_watch ~f:(fun vertex ->
      let label = ProbQuadruple.determine_label @@ Vertex.get_dist vertex in
      Out_channel.output_string txt_file
      @@ F.asprintf "%s: %s\n"
           (G.LiteralVertex.to_string (G.LiteralVertex.of_vertex vertex))
           (TaintLabel.to_string label) ) ;
  Out_channel.flush txt_file ;
  Out_channel.close txt_file


let to_watch =
  [ "ResourceSupport IndexController.index()"
  ; "Resources GuidesController.listGuides()"
  ; "ResponseEntity GuidesController.renderGuide(String,String)"
  ; "ResponseEntity GuidesController.showGuide(String,String)"
  ; "ResponseEntity MarkupController.renderMarkup(MediaType,String)" ]


let responder =
  let memory : (Method.t, TaintLabel.t list) Hashtbl.t = Hashtbl.create 777 in
  fun (question : Question.t) : Response.t ->
    match question with
    | AskingForLabel meth ->
        (* give any label not in memory *)
        let solution_labels = get_solution meth in
        let not_responded_labels =
          match Hashtbl.find_opt memory meth with
          | None ->
              solution_labels
          | Some responded_labels ->
              List.filter solution_labels ~f:(fun label ->
                  not @@ List.mem responded_labels label ~equal:TaintLabel.equal )
        in
        let not_responded_label = random_select_elem not_responded_labels in
        Response.ForLabel (meth, not_responded_label)
    | AskingForConfirmation (meth, asked_label) ->
        (* see if the asked_label is in the solution_labels. *)
        let is_correct = List.mem (get_solution meth) asked_label ~equal:TaintLabel.equal in
        Response.ForYesOrNo (meth, asked_label, is_correct)


let auto_test_spechunter_for_snapshot_once (current_snapshot : G.t)
    (received_responses : Response.t list) =
  if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
    (current_snapshot, received_responses)
  else
    (* find the most appropriate Asking Rule. *)
    let question_maker =
      MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
    in
    let question = question_maker.rule current_snapshot received_responses in
    print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
    let response = responder question in
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
    let stats =
      let correct_vertices =
        List.filter (G.all_vertices_of_graph propagated') ~f:vertex_inference_result_is_correct
      in
      F.asprintf "stats: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
        (List.length correct_vertices) (G.nb_vertex propagated')
        (get_vertexwise_precision_of_snapshot propagated')
        (List.length correct_vertices)
        ( List.length
        @@ List.filter
             (G.all_vertices_of_graph propagated')
             ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
    in
    print_endline stats ;
    (propagated', response :: received_responses)


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
    in
    let question = question_maker.rule current_snapshot received_responses in
    print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
    let response = responder question in
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
    (* output to stdout *)
    let stats =
      let correct_vertices =
        List.filter (G.all_vertices_of_graph propagated') ~f:vertex_inference_result_is_correct
      in
      F.asprintf "%d: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>" count
        (List.length correct_vertices) (G.nb_vertex propagated')
        (get_vertexwise_precision_of_snapshot propagated')
        (List.length correct_vertices)
        ( List.length
        @@ List.filter
             (G.all_vertices_of_graph propagated')
             ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
    in
    print_endline stats ;
    watch propagated' to_watch count ;
    Visualizer.visualize_snapshot propagated' ~autoopen:false ~micro:false ;
    auto_test_spechunter_for_snapshot_inner propagated' (response :: received_responses)
      nodewise_featuremap (count + 1) (stats :: log_data_acc)


let auto_test (initial_snapshot : G.t) : unit =
  watch initial_snapshot to_watch (-1) ;
  let _, log_data =
    auto_test_spechunter_for_snapshot_inner initial_snapshot []
      NodeWiseFeatures.NodeWiseFeatureMap.empty 0 []
  in
  let out_chan =
    Out_channel.create @@ F.asprintf "%s_%s.txt" (make_now_string 9) initial_snapshot.comp_unit
  in
  Out_channel.output_string out_chan log_data ;
  Out_channel.close out_chan
