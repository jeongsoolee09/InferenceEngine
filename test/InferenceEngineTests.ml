(* behold the power of Emacs... *)

open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open MakeGraph
open SimilarityHandler
open Loop
open RulesOfInference

let json = Deserializer.deserialize_json ()

let graph = GraphMaker.init_graph json

let received_responses = []

let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph

let test () = loop graph received_responses nodewise_featuremap 1

let trunk_finder ~(start : G.LiteralVertex.t) ~(end_ : G.LiteralVertex.t) (graph : G.t) : trunk list
    =
  let all_trunks = identify_trunks graph in
  List.filter
    ~f:(fun trunk ->
      Vertex.equal (G.LiteralVertex.to_vertex start graph) (List.hd_exn trunk)
      && Vertex.equal (G.LiteralVertex.to_vertex end_ graph) (List.last_exn trunk) )
    all_trunks


module Notebook1 = struct
  open GraphRepr

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let out_channel = Out_channel.create "graphmakertest.dot"

  let test () =
    Dot.output_graph out_channel graph ;
    Out_channel.close out_channel

  (* works well!!! *)
end

module Notebook2 = struct
  let test () =
    Out_channel.output_string Out_channel.stdout "hihi how are you?: " ;
    Out_channel.flush Out_channel.stdout ;
    In_channel.input_line In_channel.stdin

  (* oh we need to flush the out_channel first! *)
end

module Notebook3 = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let with_sim_edges =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge
    @@ SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph


  let target_vertex = ("void PrintStream.println(String)", "{ line 43 }")

  let test () = G.cs_succs target_vertex with_sim_edges

  (* no... this is not working.... *)
end

module Notebook4 = struct
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

module Notebook5 = struct
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

module Notebook6 = struct
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

module Notebook7 = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let testgraph = SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge graph

  let _ = G.get_cs_edges testgraph

  let _ = List.length @@ G.get_cs_edges testgraph

  (* working fine *)
end

module Notebook8 = struct
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

module Notebook9 = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json
end

module Notebook10 = struct
  (* now it should run smoothly... *)

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let received_responses = []

  let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph

  let test () = loop graph received_responses nodewise_featuremap 1
end

module Notebook11 = struct
  let response1 = Response.ForLabel ("void PrintStream.println(String)", TaintLabel.Sink)

  let response2 =
    Response.ForLabel ("Map JdbcTemplate.queryForMap(String,Object[])", TaintLabel.Source)


  let _ = MetaRules.ForPropagation.sort_propagation_rules_by_priority graph response1 []

  let _ = MetaRules.ForPropagation.sort_propagation_rules_by_priority graph response2 []

  let queryForMap_vertices =
    G.this_method_vertices graph "Map JdbcTemplate.queryForMap(String,Object[])"


  let _ = queryForMap_vertices >>= find_trunks_containing_vertex graph

  let find_trunks_containing_vertex graph vertex =
    let all_trunks = identify_trunks graph in
    List.filter ~f:(fun trunk -> List.mem ~equal:Vertex.equal trunk vertex) all_trunks


  (* identify_trunks is the problem! *)

  let vertex = ("void RelationalDataAccessApplication.printer(Map)", "{ line 41 }")

  let data_flows_in = Int.( >= ) (List.length @@ G.df_preds vertex graph) 1

  let data_flows_out = Int.( >= ) (List.length @@ G.df_succs vertex graph) 1

  let is_udf =
    let meth = fst vertex in
    let all_udfs = Deserializer.deserialize_method_txt () in
    List.mem ~equal:String.equal all_udfs meth

  (* no... *)
end

module Notebook12 = struct
  (** given a trunk pair, get the pairs of methods that are contextually similar to each other. *)

  let v1 = ("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")

  let v2 = ("void PrintStream.println(String)", "{ line 43 }")

  let v3 = ("Scanner.<init>(InputStream)", "{ line 24 }")

  let v4 = ("int[] JdbcTemplate.batchUpdate(String,List)", "{ line 33 }")

  let _ = G.collect_df_roots graph

  let _ = G.collect_df_leaves graph

  let df_only_graph = G.leave_only_df_edges graph

  let _ = trunk_finder ~start:v1 ~end_:v2 graph

  let _ = trunk_finder ~start:v3 ~end_:v4 df_only_graph

  let _ = Visualizer.visualize_at_the_face df_only_graph

  let _ = Visualizer.visualize_at_the_face graph

  let _ =
    PathUtils.is_reachable
      (G.LiteralVertex.to_vertex v1 df_only_graph)
      (G.LiteralVertex.to_vertex v2 df_only_graph)
      df_only_graph


  let identify_trunks (graph : G.t) : G.Trunk.t list =
    let df_only_graph = G.leave_only_df_edges graph in
    let roots = G.collect_df_roots df_only_graph in
    let leaves = G.collect_df_leaves df_only_graph in
    let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
    (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
    let reachable_root_and_leaf_pairs =
      List.filter ~f:(fun (root, leaf) -> PathUtils.is_reachable root leaf df_only_graph) carpro
    in
    (* now, find the path between the root and the leaf. *)
    reachable_root_and_leaf_pairs
    >>= fun (root, leaf) ->
    PathUtils.find_path_from_source_to_dest df_only_graph (G.LiteralVertex.of_vertex root) leaf


  (** Find all paths from the given source to the given destination. **)
  let find_path_from_source_to_dest (graph : G.t) (source : G.LiteralVertex.t) (dest : G.V.t) :
      G.V.t list list =
    PathUtils.enumerate_paths_from_source_to_leaves graph source
    |> List.filter ~f:(fun path -> List.mem ~equal:G.V.equal path dest)
    >>| List.take_while ~f:(fun vertex -> not @@ G.V.equal vertex dest)
    >>| fun list -> List.append list [dest]


  let _ = find_path_from_source_to_dest df_only_graph v1 (G.LiteralVertex.to_vertex v2 graph)

  (* find_path_from_source_to_dest is malfunctioning! *)

  (* done! *)
end

module Notebook13 = struct
  (* TODO *)
  let trunk1 =
    [ ( "Map JdbcTemplate.queryForMap(String,Object[])"
      , "{ line 37 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "Map RelationalDataAccessApplication.query()"
      , "{ line 37 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.bridge()"
      , "{ line 47 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 41 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "Collection Map.values()"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "Iterator Collection.iterator()"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "Object Iterator.next()"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 42 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "StringBuilder StringBuilder.append(Object)"
      , "{ line 43 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 43 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "String StringBuilder.toString()"
      , "{ line 43 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void RelationalDataAccessApplication.printer(Map)"
      , "{ line 43 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} )
    ; ( "void PrintStream.println(String)"
      , "{ line 43 }"
      , {InferenceEngineLib.GraphRepr.ProbQuadruple.src= 0.25; sin= 0.25; san= 0.25; non= 0.25} ) ]
end

module Notebook14 = struct
  open EdgeMaker

  let get_all_edges (raw_json : json) : G.E.t list =
    ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
    >>| ChainSliceManager.chain_slice_list_of_wrapped_chain
    >>| (*ChainRefiners.remove_define_frontend_tmp_var_at_the_end >> *)
    ChainRefiners.delete_inner_deads >>= edge_list_of_chain_slice_list


  open ChainSliceManager

  let all_edges_real = get_all_edges @@ Deserializer.deserialize_json ()

  let _ =
    List.filter
      ~f:(fun ((meth, _, _), _, _) ->
        String.equal meth "int[] JdbcTemplate.batchUpdate(String,List)" )
      all_edges_real


  (* just for confirmation... *)
  let _ =
    List.filter
      ~f:(fun ((meth, _, _), _, _) -> String.equal meth "String Scanner.nextLine()")
      all_edges_real


  let _ =
    G.iter_edges_e
      (fun (((meth, _, _), _, (meth2, _, _)) as edge) ->
        if String.equal meth "int[] JdbcTemplate.batchUpdate(String,List)" then print_endline meth2
        )
      graph

  (* done! *)
end
