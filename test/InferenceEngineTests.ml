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

module Notebook97 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let _ = loop renderer_finished NodeWiseFeatureMap.empty ~auto_test:false

  let _ = End
end

module Notebook98 = struct
  let _ = Start

  let is_src (_, labels) = List.mem ~equal:String.equal labels "src"

  let is_sin (_, labels) = List.mem ~equal:String.equal labels "sin"

  let is_san (_, labels) = List.mem ~equal:String.equal labels "san"

  let is_non (_, labels) = List.mem ~equal:String.equal labels "non"

  (* ============ UDFs ============ *)

  let num_sagan_udfs = Array.length Sagan_solution.sagan_udf_solution

  let sagan_udf_src = Array.filter ~f:is_src Sagan_solution.sagan_udf_solution

  let num_sagan_udf_srcs = Array.length sagan_udf_src

  let sagan_udf_sin = Array.filter ~f:is_sin Sagan_solution.sagan_udf_solution

  let num_sagan_udf_sins = Array.length sagan_udf_sin

  let sagan_udf_san = Array.filter ~f:is_san Sagan_solution.sagan_udf_solution

  let num_sagan_udf_sans = Array.length sagan_udf_san

  let sagan_udf_non = Array.filter ~f:is_non Sagan_solution.sagan_udf_solution

  let num_sagan_udf_nons = Array.length sagan_udf_non

  let sagan_udf_src_and_sin =
    Array.filter ~f:(fun sol -> is_src sol && is_sin sol) Sagan_solution.sagan_udf_solution


  let num_sagan_udf_src_and_sin = Array.length sagan_udf_src_and_sin

  let num_sagan_udf_srms = num_sagan_udf_srcs + num_sagan_udf_sins + num_sagan_udf_sans

  let sagan_udf_src_percentage = Float.of_int num_sagan_udf_srcs /. Float.of_int num_sagan_udf_srms

  let sagan_udf_sin_percentage = Float.of_int num_sagan_udf_sins /. Float.of_int num_sagan_udf_srms

  let sagan_udf_san_percentage = Float.of_int num_sagan_udf_sans /. Float.of_int num_sagan_udf_srms

  let sagan_udf_src_sin_percentage =
    Float.of_int num_sagan_udf_src_and_sin /. Float.of_int num_sagan_udf_srms


  (* ============ APIs ============ *)

  let num_sagan_apis = Array.length Sagan_solution.sagan_api_solution

  let sagan_api_src = Array.filter ~f:is_src Sagan_solution.sagan_api_solution

  let num_sagan_api_srcs = Array.length sagan_api_src

  let sagan_api_sin = Array.filter ~f:is_sin Sagan_solution.sagan_api_solution

  let num_sagan_api_sins = Array.length sagan_api_sin

  let sagan_api_san = Array.filter ~f:is_san Sagan_solution.sagan_api_solution

  let num_sagan_api_sans = Array.length sagan_api_san

  let sagan_api_non = Array.filter ~f:is_non Sagan_solution.sagan_api_solution

  let num_sagan_api_nons = Array.length sagan_api_non

  let sagan_api_src_and_sin =
    Array.filter ~f:(fun sol -> is_src sol && is_sin sol) Sagan_solution.sagan_api_solution


  let num_sagan_api_src_and_sin = Array.length sagan_api_src_and_sin

  let num_sagan_api_srms = num_sagan_api_srcs + num_sagan_api_sins + num_sagan_api_sans

  let sagan_api_src_percentage = Float.of_int num_sagan_api_srcs /. Float.of_int num_sagan_api_srms

  let sagan_api_sin_percentage = Float.of_int num_sagan_api_sins /. Float.of_int num_sagan_api_srms

  let sagan_api_san_percentage = Float.of_int num_sagan_api_sans /. Float.of_int num_sagan_api_srms

  let sagan_api_src_sin_percentage =
    Float.of_int num_sagan_api_src_and_sin /. Float.of_int num_sagan_api_srms


  let _ =
    Array.length @@ Array.append Sagan_solution.sagan_udf_solution Sagan_solution.sagan_api_solution

  let _ = End
end
