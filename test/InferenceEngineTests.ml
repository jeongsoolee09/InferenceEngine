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
open GraphMaker
module Json = Yojson.Basic

exception End

type json = Json.t

let json = Deserializer.deserialize_json ()

let graph = GraphMaker.init_graph json ~debug:false

let received_responses = []

let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph

let demo () = loop graph received_responses nodewise_featuremap 1

let trunk_finder ~(start : G.LiteralVertex.t) ~(end_ : G.LiteralVertex.t) (graph : G.t) : trunk list
    =
  let all_trunks = identify_trunks graph in
  List.filter
    ~f:(fun trunk ->
      Vertex.equal (G.LiteralVertex.to_vertex start graph) (List.hd_exn trunk)
      && Vertex.equal (G.LiteralVertex.to_vertex end_ graph) (List.last_exn trunk) )
    all_trunks


module Notebook35 = struct
  (* this thing erroneously starts from CallSlice. *)
  let json_piece_str =
    "{\n\
    \    \"defining_method\":\n\
    \      \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \    \"access_path\": \"(text, [])\",\n\
    \    \"location\": \"{ line 27, line 31 }\",\n\
    \    \"chain\": [\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Call\",\n\
    \        \"callee\": \"String TextNode.getText()\",\n\
    \        \"location\": \"{ line 27 }\",\n\
    \        \"with\": \"(param_getText_27_0, [])\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"(text, [])\",\n\
    \        \"location\": \"{ line 27, line 31 }\",\n\
    \        \"using\": \"String TextNode.getText()\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Call\",\n\
    \        \"callee\": \"String String.substring(int)\",\n\
    \        \"location\": \"{ line 31 }\",\n\
    \        \"with\": \"(param_substring_31_0, [])\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"($irvar13, [])\",\n\
    \        \"location\": \"{ line 31 }\",\n\
    \        \"using\": \"String String.substring(int)\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"(text, [])\",\n\
    \        \"location\": \"{ line 27, line 31 }\",\n\
    \        \"using\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Call\",\n\
    \        \"callee\": \"char String.charAt(int)\",\n\
    \        \"location\": \"{ line 29 }\",\n\
    \        \"with\": \"(param_charAt_29_0, [])\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"($irvar11, [])\",\n\
    \        \"location\": \"{ line 29 }\",\n\
    \        \"using\": \"char String.charAt(int)\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void PrettifyVerbatimSerializer.serialize(VerbatimNode,Printer)\",\n\
    \        \"status\": \"Dead\"\n\
    \      }\n\
    \    ]\n\
    \  }"


  let parsed = Json.from_string json_piece_str

  let _ = End
end

module Notebook36 = struct
  (* gotta do some profiling on making NS edges. *)

  let graph =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  open SimilarVertexPairExtractor

  (* OK *)
  let all_methods = G.all_non_frontend_methods_of_graph graph

  (* TODO: THIS IS THE CULPRIT *)
  let nodewise_similarity_map =
    NodewisePairExtractor.update_nodewise_similarity_map (G.all_non_frontend_methods_of_graph graph)


  (* Not the culprit. *)
  let initial_map = NodeWiseSimilarityMap.init (G.all_non_frontend_methods_of_graph graph)

  let length = NodeWiseSimilarityMap.fold (fun _ _ acc -> acc + 1) initial_map 0
  (* val length : int = 708122 🤯 *)

  (* how should we deal with the NS explosion problem? *)
  (* --> meh, no choice but to split the graph up. *)

  (* but first, let's filter out the tests/ code and try again. *)
  let _ = End
end

module Notebook37 = struct
  (* let's filter out the test/ directory. *)

  let all_java_files =
    DirectoryManager.walk_for_extension
      "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan" ".java"


  let _ = List.length all_java_files

  let non_test_java_files =
    List.filter ~f:(not << String.is_substring ~substring:"/test/") all_java_files


  let _ = List.length non_test_java_files

  let _ = List.iter ~f:print_endline non_test_java_files

  (* has not decreased that much... well, anyways let's go for it *)

  let sample_filename =
    "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan/sagan-site/src/main/java/sagan/site/events/InvalidCalendarException.java"


  let extract_filename_without_extension_and_dirs (full_filename : string) : string =
    let filename_only = List.last_exn @@ String.split ~on:'/' full_filename in
    List.hd_exn @@ String.split ~on:'.' filename_only


  (* I don't like string splits that much, but it works nonetheless!! *)

  let test_java_files = List.filter ~f:(String.is_substring ~substring:"/test/") all_java_files

  let test_java_classnames = test_java_files >>| extract_filename_without_extension_and_dirs

  (* Very nice! *)

  (* This is the initial map. *)

  let graph =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let initial_map = NodeWiseSimilarityMap.init (G.all_non_frontend_methods_of_graph graph)

  (* how many entries will disappear when we exclude all the test classes? *)

  let _ = NodeWiseSimilarityMap.cardinal initial_map

  let test_class_filtered =
    NodeWiseSimilarityMap.filter
      (fun (m1, m2) _ ->
        let open NodeWiseFeatures.SingleFeature in
        let m1_classname = extract_class_name_from_method m1
        and m2_classname = extract_class_name_from_method m2 in
        not
          ( List.mem test_java_classnames m1_classname ~equal:String.equal
          || List.mem test_java_classnames m2_classname ~equal:String.equal ) )
      initial_map


  let _ = NodeWiseSimilarityMap.cardinal test_class_filtered

  let _ = 708122 - 660156

  (* oh... not many are disappearing. A bit disappointing result I guess. *)

  (* So, this means we must split the graph. *)

  let _ = End
end

module Notebook38 = struct
  let nextLine = Method.of_string "String Scanner.nextLine()"

  let hasNext = Method.of_string "boolean Iterator.hasNext()"

  let iterator = Method.of_string "Iterator Collection.iterator()"

  let values = Method.of_string "Collection Map.values()"

  open NodeWiseFeatures.PairwiseFeature

  let _ = is_both_framework_code (nextLine, hasNext)

  let _ = belong_to_same_class (nextLine, hasNext)

  let _ = belong_to_same_package (nextLine, hasNext)

  let _ = return_type_is_another's_class (nextLine, hasNext)

  let _ = is_both_java_builtin (nextLine, hasNext)

  let _ = is_both_initializer (nextLine, hasNext)

  let _ =
    SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity (nextLine, hasNext)


  let _ =
    SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity (nextLine, iterator)


  let _ = SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity (nextLine, values)

  let _ = NodeWiseSimilarityMap.threshold

  let _ = End
end

module Notebook39 = struct
  (* how do we make a closure to memoize the output? *)

  let f =
    let x = ref "" in
    fun y ->
      if not @@ String.equal !x "" then !x
      else (
        for i = 0 to 1000000000 do
          ()
        done ;
        let out = "done!" in
        x := "done!" ;
        out )


  (* --> This is how we memoize the $hit bro! *)

  let _ = Hashtbl.hash graph

  let new_snapshot = demo ()

  let _ = Hashtbl.hash graph <> Hashtbl.hash new_snapshot

  let _ = End
end
