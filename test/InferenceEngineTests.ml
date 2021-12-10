(* behold the power of Emacs... *)

open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open SimilarityHandler
open Loop
open RulesOfInference
open NodeWiseFeatures
open Yojson.Basic
module Json = Yojson.Basic

type json = Json.t

let json = Deserializer.deserialize_json ()

let graph = GraphMaker.init_graph json ~debug:false

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

  let graph = GraphMaker.init_graph json ~debug:false

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

  let graph = GraphMaker.init_graph json ~debug:false

  let with_sim_edges =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge
    @@ SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph


  let target_vertex = ("void PrintStream.println(String)", "{ line 43 }")

  let test () = G.get_succs with_sim_edges target_vertex ~label:EdgeLabel.ContextualSimilarity

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

module Notebook6 = struct
  (* 이제, sim edge가 왜 안 생기는지를 보자. *)
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json ~debug:false

  let with_sim_edges =
    SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge
    @@ SimilarityHandler.EstablishSimEdges.make_nodewise_sim_edge graph


  let target_vertex = ("void PrintStream.println(String)", "{ line 43 }")

  let test () = G.get_succs with_sim_edges target_vertex ~label:EdgeLabel.ContextualSimilarity

  let df_edges = G.get_edges graph ~label:EdgeLabel.DataFlow

  let ns_edges = G.get_edges graph ~label:EdgeLabel.NodeWiseSimilarity

  let cs_edges = G.get_edges graph ~label:EdgeLabel.ContextualSimilarity

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

  let graph = GraphMaker.init_graph json ~debug:false

  let testgraph = SimilarityHandler.EstablishSimEdges.make_contextual_sim_edge graph

  let _ = G.get_edges testgraph ~label:EdgeLabel.ContextualSimilarity

  let _ = List.length @@ G.get_edges testgraph ~label:EdgeLabel.ContextualSimilarity

  (* working fine *)
end

module Notebook8 = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json ~debug:false

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

  let graph = GraphMaker.init_graph json ~debug:false
end

module Notebook10 = struct
  (* now it should run smoothly... *)

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json ~debug:false

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

  let data_flows_in =
    Int.( >= ) (List.length @@ G.get_preds graph vertex ~label:EdgeLabel.DataFlow) 1


  let data_flows_out =
    Int.( >= ) (List.length @@ G.get_succs graph vertex ~label:EdgeLabel.DataFlow) 1


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

  let _ = trunk_finder ~start:v3 ~end_:v4 graph

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

(* Notebook13 appears after Notebook16. *)

module Notebook15 = struct
  (* the path to println is borken. *)

  open EdgeMaker

  let sample_json =
    let in_channel = In_channel.create "sample.json" in
    let out = Json.from_channel in_channel in
    In_channel.close in_channel ;
    out


  let all_edges_sample = get_all_edges sample_json

  let with_inner_dead =
    ChainSliceManager.wrapped_chain_list_of_raw_json sample_json
    >>| ChainSliceManager.chain_slice_list_of_wrapped_chain
    (* >>| ChainRefiners.delete_inner_deads *) >>= edge_list_of_chain_slice_list


  (* we need to tweak ChainRefiners.delete_inner_deads. *)

  let chain_slices =
    List.hd_exn
      ( ChainSliceManager.wrapped_chain_list_of_raw_json sample_json
      >>| ChainSliceManager.chain_slice_list_of_wrapped_chain )


  let all_but_last = List.drop_last_exn chain_slices

  let dead_filtered =
    List.filter
      ~f:(fun chain_slice ->
        (not @@ ChainSlice.is_dead chain_slice) && (not @@ ChainSlice.is_deadbycycle chain_slice) )
      all_but_last


  let _ =
    List.last_exn (ChainRefiners.delete_inner_deads chain_slices |> edge_list_of_chain_slice_list)


  (* oops, edge_list_of_chain_slice_list was the real problem! *)

  let edge_list_of_chain_slice_list (chain_slices : ChainSlice.t list) : G.E.t list =
    let processed = ChainRefiners.process_chainslices chain_slices in
    let vertices = processed >>| VertexMaker.vertex_of_chain_slice in
    let bicycle_chain = make_bicycle_chain vertices in
    bicycle_chain >>| fun (v1, v2) -> (v1, EdgeLabel.DataFlow, v2)
end

module Notebook16 = struct
  open EdgeMaker

  let sample_json =
    let in_channel = In_channel.create "sample.json" in
    let out = Json.from_channel in_channel in
    In_channel.close in_channel ;
    out


  let chain_slices =
    List.hd_exn
      ( ChainSliceManager.wrapped_chain_list_of_raw_json sample_json
      >>| ChainSliceManager.chain_slice_list_of_wrapped_chain )


  let bicycle_chain =
    let processed =
      let processors = [ChainRefiners.delete_inner_deads; ChainRefiners.process_head_define] in
      List.fold ~f:(fun acc processor -> processor acc) ~init:chain_slices processors
    in
    (* unlike in the actual program, we first make a bicycle chain and then convert them to a list of vertex pairs. *)
    make_bicycle_chain processed


  (* bicycle_chain의 맨 끝을 봐봐. 엣지를 맞은 놈이 Define이고, 그게 만약에 frontend_tmp_var이라면, 그 튜플을 삭제해. *)

  let refine_bicycle_chain (bicycle_chain : (ChainSlice.t * ChainSlice.t) list) =
    let slice1, slice2 = List.last_exn bicycle_chain in
    match slice2 with
    | DefineSlice (_, ap, _, _) ->
        let is_frontend_tmp_var_ap = String.is_prefix ~prefix:"($" in
        if is_frontend_tmp_var_ap ap then List.slice bicycle_chain 0 (List.length bicycle_chain - 1)
        else bicycle_chain
    | _ ->
        bicycle_chain
end

module Notebook13 = struct
  (* TODO *)

  let trunk1, trunk2 =
    let res =
      trunk_finder
        ~start:("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")
        ~end_:("void PrintStream.println(String)", "{ line 43 }")
        graph
    in
    (List.nth_exn res 0, List.nth_exn res 1)


  let _ = ContextualFeatures.TrunkFeatures.trunks_share_same_suffixes_length (trunk1, trunk2)

  (* trunks_share_same_suffixes_length is borken... *)

  let fst3 (a, _, _) = a

  (* if the two trunks are not equal in size, prepends some fillers *)
  let trunk1_only_methods = trunk1 >>| fst3

  and trunk2_only_methods = trunk2 >>| fst3

  let trunk1_length = List.length trunk1

  and trunk2_length = List.length trunk2

  let trunk1_revised, trunk2_revised =
    match Int.compare trunk1_length trunk2_length with
    | -1 ->
        (* trunk1 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk2_length - trunk1_length) in
        (fillers @ trunk1_only_methods, trunk2_only_methods)
    | 0 ->
        (trunk1_only_methods, trunk2_only_methods)
    | 1 ->
        (* trunk2 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk1_length - trunk2_length) in
        (trunk1_only_methods, fillers @ trunk2_only_methods)
    | _ ->
        failwith "this is impossible"


  (* oh $hit!! *)

  let zipped = List.zip_exn (List.rev trunk1_revised) (List.rev trunk2_revised)

  let suffix = List.take_while ~f:(fun (v1, v2) -> String.equal v1 v2) zipped

  let _ = List.length suffix

  let _ = ContextualFeatures.TrunkFeatures.trunks_share_same_prefixes_length (trunk1, trunk2)

  let _ = ContextualFeatures.TrunkFeatures.trunks_share_same_suffixes_length (trunk1, trunk2)
end

module Notebook17 = struct
  let trunk1, trunk2 =
    let res =
      trunk_finder
        ~start:("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")
        ~end_:("void PrintStream.println(String)", "{ line 43 }")
        graph
    in
    (List.nth_exn res 0, List.nth_exn res 1)


  (* TODO *)

  open SimilarVertexPairExtractor.ContextualPairExtractor

  let _ = identify_similar_method_from_similar_trunk (trunk1, trunk2) graph

  let _ = find_bidirectionals_in_trunk trunk1 graph

  let _ = find_bidirectionals_in_trunk trunk2 graph

  (* ok, understandable. *)
end

module Notebook18 = struct
  let trunk1 =
    let res =
      trunk_finder
        ~start:("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")
        ~end_:("void PrintStream.println(String)", "{ line 43 }")
        graph
    in
    List.hd_exn res


  let trunk2 =
    let res =
      trunk_finder
        ~start:("Scanner.<init>(InputStream)", "{ line 24 }")
        ~end_:("int[] JdbcTemplate.batchUpdate(String,List)", "{ line 33 }")
        graph
    in
    List.hd_exn res


  open SimilarVertexPairExtractor.ContextualPairExtractor

  (* trunk1 and trunk2 are completely different trunks! *)

  let similar_methods = identify_similar_method_from_similar_trunk (trunk1, trunk2) graph

  (* 😮 it's working fine! *)

  (* then... the problem must be at smart_pairup_vertices! *)

  let _ = smart_pairup_vertices trunk1 trunk2 (List.hd_exn similar_methods)

  let _ = smart_pairup_vertices trunk1 trunk2 (List.nth_exn similar_methods 1)

  let _ = smart_pairup_vertices trunk1 trunk2 (List.nth_exn similar_methods 2)

  let _ = smart_pairup_vertices trunk1 trunk2 (List.nth_exn similar_methods 3)

  (* hmmmmm..... *)

  (* then whats the problem?? *)

  (* ===== let make_contextual_sim_edge (graph : G.t) : G.t ===== *)

  (* we use smart_pairup here, to translate method simliarity to vertex similarity. *)
  let _ =
    let open SimilarVertexPairExtractor in
    let all_trunks = identify_trunks graph in
    let trunk_similarity_map = TrunkPairExtractor.update_trunk_similarity_map all_trunks in
    TrunkSimilarityMap.fold
      (fun ((trunk1, trunk2) as trunk_pair) similarity acc ->
        if similarity >= TrunkSimilarityMap.threshold then
          let contextually_similar_methods =
            ContextualPairExtractor.identify_similar_method_from_similar_trunk trunk_pair graph
          in
          let smart_pairedup =
            contextually_similar_methods
            >>= fun (method1, method2) ->
            ContextualPairExtractor.smart_pairup_vertices trunk1 trunk2 (method1, method2)
          in
          List.fold
            ~f:(fun smol_acc (v1, v2) ->
              print_endline @@ F.asprintf "(%s, %s)" (Vertex.to_string v1) (Vertex.to_string v2) ;
              G.add_edge_e smol_acc (v1, EdgeLabel.ContextualSimilarity, v2) )
            ~init:acc smart_pairedup
        else acc )
      trunk_similarity_map graph


  let get_trunk_similarity (trunk_pair : trunk * trunk) : int =
    (* execute all extractors. *)
    let extractors_list =
      [ ContextualFeatures.TrunkFeatures.same_callee_in_trunk_count
      ; ContextualFeatures.TrunkFeatures.trunks_share_same_suffixes_length
      ; ContextualFeatures.TrunkFeatures.trunks_share_same_prefixes_length ]
    in
    List.fold ~f:(fun acc extractor -> acc + extractor trunk_pair) ~init:0 extractors_list


  let _ = ContextualFeatures.TrunkFeatures.same_callee_in_trunk_count (trunk1, trunk2)

  let _ = ContextualFeatures.TrunkFeatures.trunks_share_same_suffixes_length (trunk1, trunk2)

  let _ = ContextualFeatures.TrunkFeatures.trunks_share_same_prefixes_length (trunk1, trunk2)

  (* 아 이런....ㅠㅠㅠ... 뭐가 문제인지 알겠다. *)

  (* TODO 1: bidirectional 세기 *)
  (* --> meh. 이건 별로인듯 *)

  (* TODO 2: same_callee_in_trunk_count 고치기 *)

  (* 흠냐... 나중에 dynamic하게 엣지를 만드는 것도 좀 생각해 봐야겠다. *)
end

module Notebook19 = struct
  let methstring_regex = Str.regexp "\\([a-zA-Z]*\\) ?\\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let initstring_regex = Str.regexp "\\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let toString = "String StringBuilder.toString()"

  let append = "StringBuilder StringBuilder.append(Object)"

  let arraylist_init = "ArrayList.<init>()"

  let _ =
    assert (Str.string_match methstring_regex toString 0) ;
    print_endline @@ Str.matched_group 1 toString ;
    print_endline @@ Str.matched_group 2 toString ;
    print_endline @@ Str.matched_group 3 toString


  let _ =
    assert (Str.string_match methstring_regex append 0) ;
    print_endline @@ Str.matched_group 1 append ;
    print_endline @@ Str.matched_group 2 append ;
    print_endline @@ Str.matched_group 3 append


  let _ =
    assert (Str.string_match initstring_regex arraylist_init 0) ;
    print_endline @@ Str.matched_group 1 arraylist_init ;
    print_endline @@ Str.matched_group 2 arraylist_init
end

module Notebook20 = struct
  let toString = "String StringBuilder.toString()"

  let append = "StringBuilder StringBuilder.append(Object)"

  let map_values = "Collection Map.values()"

  let iterator = "Iterator Collection.iterator()"

  let arraylist_init = "ArrayList.<init>()"

  open NodeWiseFeatures

  let _ = PairwiseFeature.belong_to_same_class (toString, append)

  let _ = PairwiseFeature.return_type_is_another's_class (map_values, iterator)

  let _ =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_rtntype_from_methstring map_values))
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_methstring iterator))


  let normalstring_regex = Str.regexp "\\(.*\\) \\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let _ =
    assert (Str.string_match normalstring_regex map_values 0) ;
    print_endline @@ Str.matched_group 1 map_values ;
    print_endline @@ Str.matched_group 2 map_values ;
    print_endline @@ Str.matched_group 3 map_values


  let _ =
    assert (Str.string_match SingleFeature.initstring_regex arraylist_init 0) ;
    print_endline @@ Str.matched_group 1 arraylist_init ;
    print_endline @@ Str.matched_group 2 arraylist_init
end

module Notebook21 = struct
  (* testing why Iterator family's not gotten a NS edge. *)

  (* ns edge?? *)

  let next = ("Object Iterator.next()", "{ line 42 }")

  let iterator = ("Iterator Collection.iterator()", "{ line 42 }")

  open SimilarVertexPairExtractor.NodewisePairExtractor
  open NodeWiseFeatures.SingleFeature
  open NodeWiseFeatures.PairwiseFeature

  let _ = return_type_is_another's_class (fst next, fst iterator)

  let _ = return_type_is_another's_class (fst iterator, fst next)

  let _ = get_nodewise_similarity (fst next, fst iterator)

  let _ = get_nodewise_similarity (fst iterator, fst next)

  (* okay, get_nodewise_similarity ain't borken. *)

  (* --> it was because of the threshold value for constructing a Nodewise Similarity Edge. *)
end

(* now we need to make a scoring system. *)

module Notebook22 = struct
  let scanner_init = ("Scanner.<init>(InputStream)", "{ line 24 }")

  let queryForMap = ("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")

  open SimilarVertexPairExtractor.NodewisePairExtractor

  let _ = get_nodewise_similarity (fst scanner_init, fst queryForMap)

  open NodeWiseFeatures.PairwiseFeature

  (* [ (is_both_framework_code, 5) *)
  (* ; (belong_to_same_class, 2) *)
  (* ; (belong_to_same_package, 2) *)
  (* ; (return_type_is_another's_class, 3) ] *)

  let _ = is_both_framework_code (fst scanner_init, fst queryForMap)

  let _ = belong_to_same_class (fst scanner_init, fst queryForMap)

  let _ = belong_to_same_package (fst scanner_init, fst queryForMap)

  let _ = return_type_is_another's_class (fst scanner_init, fst queryForMap)

  let _ = return_type_is_another's_class (fst queryForMap, fst scanner_init)

  (* oops, is_both_framework_code is borken *)
  open NodeWiseFeatures.SingleFeature

  let _ = is_framework_method (fst scanner_init)

  (* scanner_init is not a framework method!! *)

  let _ =
    not
      ( (bool_of_feature @@ is_this_project_class_initializer (fst scanner_init))
      || (bool_of_feature @@ is_java_builtin_class_initializer (fst scanner_init)) )


  let _ = bool_of_feature @@ is_this_project_class_initializer (fst scanner_init)

  let _ = bool_of_feature @@ is_java_builtin_class_initializer (fst scanner_init)

  let line = "java.util.Scanner.<init>(java.io.InputStream)"

  let _ =
    let classname = string_of_feature @@ extract_class_name_from_initstring (fst scanner_init)
    and methodname = string_of_feature @@ extract_method_name_from_initstring (fst scanner_init) in
    String.is_substring ~substring:(F.asprintf "%s.%s" classname methodname) line
    && String.is_prefix ~prefix:"java." line
end

(* good.. but why no cs edges *)
(* --> 없을 수밖에 없었네! *)

module Notebook23 = struct
  (* we need another feature: return_value is not used in caller *)

  (* nodewise featuremap *)

  (* however, we need something to debug... *)

  (* 1. DONE we first change what method gets serialized (to .lisp) *)
  (* 2. DONE we then add void calls at the end of each chain.
     --> this is conveniently checked by the status value of each slice. (is it VoidCall??)
     --> this means... we need to collect all VoidCalls. How do we *efficiently* to this??
     --> I have an idea *)
end

(* root/leaf이라는 것은 강력한 힌트가 될 수는 있지만 그 자체로 충분하지는 않다. *)
module Notebook24 = struct
  let trunk1_root = ("Map JdbcTemplate.queryForMap(String,Object[])", "{ line 37 }")

  let trunk2_leaf = ("int[] JdbcTemplate.batchUpdate(String,List)", "{ line 33 }")

  let _ =
    G.is_pointing_to_each_other trunk1_root trunk2_leaf graph ~label:EdgeLabel.NodeWiseSimilarity
end

(* NOTE Scanner.nextLine까지 해낼 수 있다!!! 왜냐? batchUpdate가 sink인건 확실하니까. *)

module Notebook25 = struct
  (* DONE *)

  (* Scanner.nextLine까지 해낼 수 있다!!! 왜냐? batchUpdate가 sink인건 확실하니까. *)
  (* 위의 아이디어를 코드로 표현해 봅시다. *)

  (* 그 trunk의 맨 끝에 sink라면, 그 위로 올라가면서 java/framework가 있으면 고놈은 source이다. *)

  (* our code should go into... PropagationRules. *)

  (* PropagationRules... 🤔 *)
end

(* TODO: replace expensive calls to Nodewise single features into a dataframe lookup. *)

module Notebook26 = struct
  (* DONE *)
  let next = ("Object Iterator.next()", "{ line 42 }")

  let append = ("StringBuilder StringBuilder.append(Object)", "{ line 43 }")

  let _ = G.is_bidirectional_vertex append graph ~label:EdgeLabel.DataFlow

  let vertex = append

  let vertex_succs = G.get_succs graph vertex ~label:EdgeLabel.DataFlow

  let _ =
    List.fold
      ~f:(fun acc succ_vertex ->
        print_endline @@ F.asprintf "%s" (Vertex.to_string succ_vertex) ;
        let succ_vertex_preds =
          G.get_preds graph (G.LiteralVertex.of_vertex succ_vertex) ~label:EdgeLabel.DataFlow
        in
        let found =
          List.exists
            ~f:(fun ((meth, _, _) as succ_vertex_pred) ->
              G.LiteralVertex.equal vertex (G.LiteralVertex.of_vertex succ_vertex_pred) )
            succ_vertex_preds
        in
        found || acc )
      ~init:false vertex_succs
end

module Notebook27 = struct
  (* making a nodewise cluster *)

  let find_ns_cluster (graph : G.t) : G.V.t list list =
    let rec inner (vertex : G.V.t) (acc : G.V.t list) : G.V.t list =
      let all_ns_bidirectionals =
        List.filter
          ~f:(fun other_vertex ->
            G.is_pointing_to_each_other
              (G.LiteralVertex.of_vertex vertex)
              (G.LiteralVertex.of_vertex other_vertex)
              graph ~label:EdgeLabel.NodeWiseSimilarity )
          (G.all_vertices_of_graph graph)
      in
      let vertices_to_explore =
        List.filter
          ~f:(fun vertex -> not @@ List.mem ~equal:Vertex.equal acc vertex)
          all_ns_bidirectionals
      in
      if
        not
        @@ G.is_bidirectional_vertex
             (G.LiteralVertex.of_vertex vertex)
             graph ~label:EdgeLabel.NodeWiseSimilarity
        || List.is_empty vertices_to_explore
      then acc (* we can't recurse anymore *)
      else
        List.fold
          ~f:(fun smol_acc new_vertex -> inner new_vertex (vertex :: new_vertex :: acc))
          ~init:acc vertices_to_explore
    in
    List.fold
      ~f:(fun acc vertex ->
        if not @@ List.mem ~equal:G.V.equal (List.join acc) vertex then
          let res = inner vertex [] in
          if List.is_empty res then acc else res :: acc
        else acc )
      ~init:[] (G.all_vertices_of_graph graph)

  (* DONE working nicely!! *)
end

(* TODO: propagator에 마이크로스냅샷을 dot으로 제공하는 기능 탑재 *)

(* ContextualSimilarity...? Dunno *)

module Notebook28 = struct
  let recursively_find_preds (graph : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) :
      G.V.t list =
    let rec inner (current_vertex : G.V.t) (big_acc : G.V.t list) =
      let current_vertex_preds =
        G.get_preds graph (G.LiteralVertex.of_vertex current_vertex) ~label
      in
      let to_explore =
        List.filter current_vertex_preds ~f:(fun pred ->
            (not @@ List.mem big_acc pred ~equal:Vertex.equal)
            && (not @@ Vertex.equal pred (G.LiteralVertex.to_vertex vertex graph)) )
      in
      if List.is_empty current_vertex_preds || List.is_empty to_explore then big_acc
      else
        List.fold
          ~f:(fun smol_acc vertex -> inner vertex (vertex :: smol_acc))
          ~init:big_acc to_explore
    in
    inner (G.LiteralVertex.to_vertex vertex graph) []


  let _ =
    recursively_find_preds graph
      ("int[] JdbcTemplate.batchUpdate(String,List)", "{ line 33 }")
      ~label:EdgeLabel.DataFlow


  let _ =
    recursively_find_preds graph
      ("Collection Map.values()", "{ line 42 }")
      ~label:EdgeLabel.NodeWiseSimilarity
end

module Notebook29 = struct
  (* let's automate the Asking Rule!! *)

  (* 1. TODO ask on the Nodewise clusters *)
  (* 2. TODO guide the question to println *)
end

module Notebook30 = struct
  (* Debugging the problem where it keeps asking about batchUpdate *)
  let responses =
    [Response.ForLabel ("int[] JdbcTemplate.batchUpdate(String,List)", TaintLabel.Sink)]


  let selected_rule = MetaRules.ForAsking.asking_rules_selector graph responses nodewise_featuremap

  let snapshot = Deserializer.deserialize_graph "2021-12-10_16:47:39.bin"

  let all_leaves_are_determined =
    List.for_all (G.collect_df_leaves snapshot) ~f:(fun leaf ->
        G.Saturation.dist_is_saturated (trd3 leaf) )


  let all_roots_are_determined =
    List.for_all (G.collect_df_roots snapshot) ~f:(fun root ->
        G.Saturation.dist_is_saturated (trd3 root) )


  let all_foreign_codes_are_determined =
    let all_foreign_codes =
      G.fold_vertex
        (fun vertex acc ->
          if
            NodeWiseFeatures.SingleFeature.bool_of_feature
            @@ NodeWiseFeatures.SingleFeature.is_framework_method (fst3 vertex)
          then vertex :: acc
          else acc )
        snapshot []
    in
    List.for_all all_foreign_codes ~f:(fun foreign_code ->
        G.Saturation.dist_is_saturated (trd3 foreign_code) )


  let all_ns_clusters_have_been_asked =
    List.for_all (all_ns_clusters snapshot) ~f:(fun ns_cluster ->
        List.for_all ns_cluster ~f:(fun vertex -> G.Saturation.dist_is_saturated (trd3 vertex)) )


  (* these were the culprit!! *)

  let batchUpdate_dist =
    G.lookup_dist_for_meth_and_loc "int[] JdbcTemplate.batchUpdate(String,List)" "{ line 33 }"
      snapshot


  let saturated_parameter = 0.2 (* TEMP: subject to change *)

  let dist_is_saturated (quad : ProbQuadruple.t) : bool =
    let sorted = List.sort ~compare:Float.compare [quad.src; quad.sin; quad.san; quad.non] in
    let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
    Float.( >= ) (first -. second) saturated_parameter


  let _ = dist_is_saturated batchUpdate_dist

  let sorted =
    List.sort
      ~compare:(fun p1 p2 -> -Float.compare p1 p2)
      [batchUpdate_dist.src; batchUpdate_dist.sin; batchUpdate_dist.san; batchUpdate_dist.non]


  let first = List.nth_exn sorted 0

  let second = List.nth_exn sorted 1

  let _ = Float.( >= ) (first -. second) saturated_parameter
end
