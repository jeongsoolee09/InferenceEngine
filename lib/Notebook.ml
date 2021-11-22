(* behold the power of Emacs... *)

open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open MakeGraph
open SimilarityHandler
open Probability
open Loop

module GraphTest = struct
  let sample_graph =
    G.empty
    |> (fun graph -> G.add_edge graph ("nextLine", "26") ("create", "26"))
    |> (fun graph -> G.add_edge graph ("create", "26") ("create", "31"))
    |> (fun graph -> G.add_edge graph ("create", "31") ("run", "31"))
    |> (fun graph -> G.add_edge graph ("run", "31") ("append", "32"))
    |> (fun graph -> G.add_edge graph ("append", "32") ("run", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("append", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("toString", "32"))
    |> (fun graph -> G.add_edge graph ("toString", "32") ("run", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("batchUpdate", "34"))
    |> fun graph -> G.add_edge graph ("run", "33") ("batchUpdate", "34")


  (** How will identify_trunk work on sample_graph? *)

  let test () =
    let trunks = identify_trunks sample_graph in
    List.iter ~f:(fun trunk -> Trunk.pp trunk) trunks
end

module GraphMakerTest = struct
  open GraphRepr

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let out_channel = Out_channel.create "graphmakertest.dot"

  let test () = Dot.output_graph out_channel graph

  (* works well!!! *)
end

module DistMapTest = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let distmap = make_map_for_graph graph

  let vertices = G.all_vertices_of_graph graph

  let distmap_keys = ProbMap.fold (fun k _ acc -> k :: acc) distmap []

  let test () =
    ProbMap.iter
      (fun k v ->
        Out_channel.output_string Out_channel.stdout @@ Vertex.to_string k ;
        Out_channel.output_string Out_channel.stdout @@ ProbQuadruple.to_string v )
      distmap


  let test2 () =
    List.for_all ~f:(fun elem -> List.mem ~equal:Vertex.equal vertices elem) distmap_keys


  let test3 () =
    List.for_all ~f:(fun elem -> List.mem ~equal:Vertex.equal distmap_keys elem) vertices

  (* works well! *)
end

module TestLoop = struct
  (* does it even run smoothly? *)

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let initial_distmap = make_map_for_graph graph

  let received_responses = []

  let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph

  let test () = loop initial_distmap received_responses graph nodewise_featuremap 1
end

module TestIO = struct
  let test () =
    Out_channel.output_string Out_channel.stdout "hihi how are you?: " ;
    Out_channel.flush Out_channel.stdout ;
    In_channel.input_line In_channel.stdin

  (* oh we need to flush the out_channel first! *)
end

module TestSimilarityEdges = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let with_sim_edges =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge
    @@ SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph


  let target_vertex = ("void PrintStream.println(String)", "{ line 43 }")

  let test () = G.cs_succs target_vertex with_sim_edges

  (* no... this is not working.... *)
end

module TestRedefineHandler = struct
  module ChainSliceSet = Set.Make (ChainSlice)
  module Util = Yojson.Basic.Util

  let collect_redefines_for_single_chain (json_assoc : json) (* : ChainSlice.t list *) =
    let collected =
      match json_assoc with
      | `List alist ->
          List.fold
            ~f:(fun acc assoc ->
              let alist = Util.to_assoc assoc in
              match List.Assoc.find_exn alist "status" ~equal:String.equal with
              | `String "Redefine" ->
                  let current_method =
                    Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
                  in
                  let location =
                    Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
                  in
                  let access_path =
                    Util.to_string @@ List.Assoc.find_exn alist "access_path" ~equal:String.equal
                  in
                  let redefine_slice =
                    ChainSlice.RedefineSlice (current_method, location, access_path)
                  in
                  redefine_slice :: acc
              | otherwise ->
                  acc )
            ~init:[] alist
      | _ ->
          failwith "Type Error3"
    in
    (* deduping process (order is irrelevant) *)
    collected |> ChainSliceSet.of_list |> ChainSliceSet.elements


  let collect_redefines (json : json) =
    match json with
    | `List list ->
        list
        >>| (fun json_assoc -> Util.member "chain" json_assoc)
        >>= collect_redefines_for_single_chain
    | _ ->
        failwith "Type Error4"

  (* now working fine!!! *)
end

(* the repl makes tedious debugging very viable! *)

module TrunkSimilarityTest = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let all_trunks = identify_trunks graph

  let all_methods = G.all_methods_of_graph graph

  (* noooo there are too many of them!!! *)
  (* utop # List.length all_trunks;; *)
  (* - : int = 36 *)

  (* meh.... whatever... *)

  (* contextualsimilarity가 잘 매겨지고 있나...?? *)

  (* --> we need to examine TrunkSimilarityMap!!! *)
  (* --> ..which leads us to each of the extractors. *)

  let sample_tsmap =
    SimilarityHandler.SimilarVertexPairExtractor.ContextualPairExtractor
    .update_contextual_similarity_map all_trunks all_methods graph


  open Core_kernel.Out_channel

  let test () =
    ContextualSimilarityMap.iter
      (fun k v ->
        output_string stdout (string_of_int v) ;
        newline stdout )
      sample_tsmap


  let test1 () =
    ContextualSimilarityMap.iter
      (fun k v ->
        if v >= 1 then (
          output_string stdout (string_of_int v) ;
          newline stdout ) )
      sample_tsmap

  (* ok, there are some contextual similarities captured: they are all six. *)
  (* maybe I need some granular investigation! *)

  (* --> all contextual extractors working. *)
end
