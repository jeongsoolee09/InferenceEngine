open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open SimilarityHandler
open Loop
open RulesOfInference
open NodeWiseFeatures
open Yojson.Basic
open DataFlowEdges
open DirectoryManager
open Chain
open Method
open GraphSplitter
open Annotations
open Trunk
open SpawnPython
open Main
module Json = Yojson.Basic

exception End

type json = Json.t

let json = Deserializer.deserialize_json ()

let received_responses = []

let trunk_finder ~(start : G.LiteralVertex.t) ~(end_ : G.LiteralVertex.t) (graph : G.t) :
    trunk array =
  let all_trunks = identify_longest_trunks graph in
  Array.filter
    ~f:(fun trunk ->
      Vertex.equal (G.LiteralVertex.to_vertex start graph.graph) trunk.(0)
      && Vertex.equal (G.LiteralVertex.to_vertex end_ graph.graph) (Array.last trunk) )
    all_trunks


let df_edges_added =
  match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" ~finished:false with
  | None ->
      let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
      G.serialize_to_bin result ~suffix:"df_edges" ;
      result
  | Some filename ->
      Deserializer.deserialize_graph filename


let splitted = split_graph_by_comp_unit df_edges_added

let renderer_graph = List.nth_exn splitted 0

let site_graph = List.nth_exn splitted 1

module Notebook94 = struct
  let renderer_finished = build_graph renderer_graph

  let parse = "Document Jsoup.parse(String)"

  let response = Response.ForLabel (parse, TaintLabel.Sanitizer)

  let test, _ =
    PropagationRules.internal_nonbidirectional_library_node_is_a_src_if_leaf_is_sink
      renderer_finished response [] ~dry_run:false


  let _ = Visualizer.visualize_snapshot test ~micro:false ~autoopen:true

  (* Inspection *)

  let parse_vertex1 = (parse, "{ line -2 }")

  let parse_vertex2 = (parse, "{ line 47 }")

  let _ = G.LiteralVertex.to_vertex parse_vertex1 test.graph

  let _ = G.LiteralVertex.to_vertex parse_vertex2 test.graph

  (* applying all rules *)

  let _ = End
end
