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
    List.iter ~f:(fun trunk -> G.Trunk.pp trunk) trunks
end

module GraphMakerTest = struct
  open GraphRepr

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let out_channel = Out_channel.create "graphmakertest.dot"

  let test () =
    Dot.output_graph out_channel graph ;
    Out_channel.close out_channel

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

module TestSimilarityEdges2 = struct
  (* 이제, sim edge가 왜 안 생기는지를 보자. *)
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let with_sim_edges =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge
    @@ SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph


  let target_vertex = ("void PrintStream.println(String)", "{ line 43 }")

  let test () = G.cs_succs target_vertex with_sim_edges

  let df_edges = G.get_df_edges graph

  let ns_edges = G.get_ns_edges graph

  let cs_edges = G.get_cs_edges graph

  (* no sim edges at all!! *)

  let nodewise_map =
    SimilarVertexPairExtractor.NodewisePairExtractor.update_nodewise_similarity_map
      (G.all_methods_of_graph graph)


  open Out_channel

  let test2 () =
    NodeWiseSimilarityMap.iter
      (fun k v ->
        if v >= 2 then (
          output_string stdout (StringPair.to_string k) ;
          output_string stdout (string_of_int v) ;
          newline stdout ) )
      nodewise_map
end

module TestNodewiseSim = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let all_methods = G.all_methods_of_graph graph

  open Out_channel

  let _ = G.get_ns_edges graph

  let make_nodewise_sim_edge (graph : G.t) : G.t =
    let open SimilarVertexPairExtractor in
    let all_vertices = G.all_vertices_of_graph graph in
    let contextual_similarity_map =
      NodewisePairExtractor.update_nodewise_similarity_map (G.all_methods_of_graph graph)
    in
    let above_threshold_entries =
      NodeWiseSimilarityMap.filter
        (fun _ similarity -> similarity >= NodewisePairExtractor.threshold)
        contextual_similarity_map
    in
    NodeWiseSimilarityMap.fold
      (fun (method1, method2) _ acc ->
        let method1_vertices =
          List.filter ~f:(fun (meth, _) -> String.equal meth method1) all_vertices
        and method2_vertices =
          List.filter ~f:(fun (meth, _) -> String.equal meth method2) all_vertices
        in
        (* we'll use smart_pairup_vertices to ensure we don't connect two distant vertices. *)
        let smart_pairedup : (Vertex.t * Vertex.t) list =
          ContextualPairExtractor.smart_pairup_vertices method1_vertices method2_vertices
            (method1, method2)
        in
        List.fold
          ~f:(fun smol_acc (v1, v2) ->
            G.add_edge_e smol_acc (v1, EdgeLabel.NodeWiseSimilarity, v2)
            |> fun graph -> G.add_edge_e graph (v2, EdgeLabel.NodeWiseSimilarity, v1) )
          ~init:acc smart_pairedup )
      above_threshold_entries graph


  open SimilarVertexPairExtractor

  let all_vertices = G.all_vertices_of_graph graph

  let contextual_similarity_map =
    NodewisePairExtractor.update_nodewise_similarity_map (G.all_methods_of_graph graph)


  let above_threshold_entries =
    NodeWiseSimilarityMap.filter
      (fun _ similarity -> similarity >= NodewisePairExtractor.threshold)
      contextual_similarity_map


  (* let _ = *)
  (*   NodeWiseSimilarityMap.iter *)
  (*     (fun (k1, k2) v -> *)
  (*       print_string k1 ; *)
  (*       print_string k2 ; *)
  (*       print_int v ; *)
  (*       print_newline () ) *)
  (*     above_threshold_entries *)

  let testgraph = SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph

  let _ = G.get_ns_edges testgraph

  let method1 = "Map JdbcTemplate.queryForMap(String,Object[])"

  let method2 = "int[] JdbcTemplate.batchUpdate(String,List)"

  let method1_vertices = List.filter ~f:(fun (meth, _) -> String.equal meth method1) all_vertices

  and method2_vertices = List.filter ~f:(fun (meth, _) -> String.equal meth method2) all_vertices

  let smart_pairedup : (Vertex.t * Vertex.t) list =
    ContextualPairExtractor.smart_pairup_vertices method1_vertices method2_vertices
      (method1, method2)


  let graph_added =
    List.fold
      ~f:(fun acc (v1, v2) ->
        G.add_edge_e acc (v1, EdgeLabel.NodeWiseSimilarity, v2)
        |> fun graph -> G.add_edge_e graph (v2, EdgeLabel.NodeWiseSimilarity, v1) )
      ~init:graph smart_pairedup

  (* pairwise similarity now working fine! *)
end

module TestContextualSimilarity = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let testgraph = SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge graph

  let _ = G.get_cs_edges testgraph

  let _ = List.length @@ G.get_cs_edges testgraph

  (* working fine *)
end

module TestAllSimilarities = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let testgraph =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge graph
    |> SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge


  let _ = G.all_vertices_of_graph graph

  let _ =
    G.all_vertices_of_graph (SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge graph)


  let _ = G.all_vertices_of_graph (SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph)

  (* make_contextual_sim_edge is making a ("", "") vertex! *)
end

module TestContextualSimEdge = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  open SimilarVertexPairExtractor

  let all_trunks = identify_trunks graph

  let trunk_similarity_map = TrunkPairExtractor.update_trunk_similarity_map all_trunks

  let _ = List.nth all_trunks 5

  (* [("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }");
     ("Map RelationalDataAccessApplication.query()", "{ line 37 }");
     ("void RelationalDataAccessApplication.bridge()", "{ line 47 }");
     ("void RelationalDataAccessApplication.printer(Map)", "{ line 41 }");
     ("Collection Map.values()", "{ line 42 }");
     ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }");
     ("Iterator Collection.iterator()", "{ line 42 }");
     ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }");
     ("Object Iterator.next()", "{ line 42 }");
     ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }");
     ("StringBuilder StringBuilder.append(Object)", "{ line 43 }");
     ("void RelationalDataAccessApplication.printer(Map)", "{ line 43 }")]

      trunk list에 이게 왜 들어있는지 모르겠네... line43의 printer가 왜 리프? *)

  (* 이제 왜 empty vertex가 생기는지를 봅시다. *)
  (* 일단 similarity가 높은 trunk pair부터 찾아 봅시다. *)

  let pair_with_max_value (map : TrunkSimilarityMap.t) =
    fst
    @@ TrunkSimilarityMap.fold
         (fun key value (current_max_key, current_max_value) ->
           if value > current_max_value then (key, value) else (current_max_key, current_max_value)
           )
         map
         (([], []), Int.min_value)


  let update_trunk_similarity_map (all_trunks : trunk list) : TrunkSimilarityMap.t =
    let initial_map = TrunkSimilarityMap.init all_trunks in
    TrunkSimilarityMap.fold
      (fun ((t1, t2) as pair) _ acc ->
        assert (not @@ G.Trunk.equal t1 t2) ;
        let trunk_similarity = TrunkPairExtractor.get_trunk_similarity pair in
        TrunkSimilarityMap.remove pair acc |> TrunkSimilarityMap.add pair trunk_similarity )
      initial_map initial_map


  let trunk_similarity_map = update_trunk_similarity_map all_trunks

  let _ = pair_with_max_value trunk_similarity_map

  let trunk1 =
    [ ("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")
    ; ("Map RelationalDataAccessApplication.query()", "{ line 37 }")
    ; ("void RelationalDataAccessApplication.bridge()", "{ line 47 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 41 }")
    ; ("Collection Map.values()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("Iterator Collection.iterator()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("Object Iterator.next()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("boolean Iterator.hasNext()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("StringBuilder StringBuilder.append(Object)", "{ line 43 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 43 }")
    ; ("String StringBuilder.toString()", "{ line 43 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 43 }") ]


  let trunk2 =
    [ ("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")
    ; ("Map RelationalDataAccessApplication.query()", "{ line 37 }")
    ; ("void RelationalDataAccessApplication.bridge()", "{ line 47 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 41 }")
    ; ("Collection Map.values()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("Iterator Collection.iterator()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("boolean Iterator.hasNext()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("Object Iterator.next()", "{ line 42 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 42 }")
    ; ("StringBuilder StringBuilder.append(Object)", "{ line 43 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 43 }")
    ; ("String StringBuilder.toString()", "{ line 43 }")
    ; ("void RelationalDataAccessApplication.printer(Map)", "{ line 43 }") ]


  (* ㅋㅋㅋㅋ 같은 줄 알았는데 아니었네. *)
  (* 어쨌든 찾았다. *)

  let _ = TrunkSimilarityMap.find (trunk1, trunk2) trunk_similarity_map

  let trunk_pair = (trunk1, trunk2)

  let contextually_similar_methods =
    ContextualPairExtractor.identify_similar_method_from_similar_trunk trunk_pair graph


  let smart_pairedup =
    contextually_similar_methods
    >>= fun (method1, method2) ->
    ContextualPairExtractor.smart_pairup_vertices trunk1 trunk2 (method1, method2)


  let testgraph =
    List.fold
      ~f:(fun smol_acc (v1, v2) -> G.add_edge_e smol_acc (v1, EdgeLabel.ContextualSimilarity, v2))
      ~init:graph smart_pairedup
end

module TestInitGraph = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json
end

module TestLoop2 = struct
  (* now it should run smoothly... *)

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let initial_distmap = make_map_for_graph graph

  let received_responses = []

  let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph

  let test () =
    loop initial_distmap received_responses graph nodewise_featuremap 1
end

