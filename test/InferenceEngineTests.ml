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

type delimiter = Start | End

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

module Notebook95 = struct
  let _ = Start

  let vertices = [("v1", "l1"); ("v2", "l2"); ("v3", "l3"); ("v4", "l4")]

  let initial =
    List.fold
      ~f:(fun current_graph (v, l) -> G.add_vertex current_graph (v, l, ProbQuadruple.initial))
      ~init:G.empty vertices


  module TestMap = struct
    module WithGraphDomain = Caml.Map.Make (G)
    include WithGraphDomain

    type t = Int.t WithGraphDomain.t
  end

  let added = TestMap.add initial 777 TestMap.empty

  (* now, bump v1's dist. *)

  let bumped =
    G.strong_update_dist
      (G.LiteralVertex.to_vertex ("v1", "l1") initial.graph)
      {src= 1.; san= 0.; sin= 0.; non= 0.}
      initial


  let _ = G.equal initial bumped

  let _ = TestMap.find bumped added

  let _ = TestMap.find initial added

  let _ = assert (Int.equal (TestMap.find bumped added) (TestMap.find initial added))

  (* Good! *)
  let _ = End
end
