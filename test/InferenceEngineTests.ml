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
open DirectoryManager
open Chain
open Method
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
      Vertex.equal (G.LiteralVertex.to_vertex start graph.graph) (List.hd_exn trunk)
      && Vertex.equal (G.LiteralVertex.to_vertex end_ graph.graph) (List.last_exn trunk) )
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
        let m1_classname = Method.get_class_name m1 and m2_classname = Method.get_class_name m2 in
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

module Notebook40 = struct
  let root_dir = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan"

  (* let () = Sys.chdir root_dir *)

  let _ = Sys.readdir root_dir

  let has_java (dir : string) : bool = not @@ List.is_empty @@ walk_for_extension dir ".java"

  let _ = has_java "./sagan-renderer" (* true *)

  let _ = has_java "./sagan-client" (* false *)

  let _ = has_java "./sagan-site" (* true *)

  let _ = get_compilation_unit_subdirs root_dir

  (* get_compilation_unit_subdirs works nicely *)

  let _ = End
end

module Notebook41 = struct
  (* 1. get_compilation_unit_subdirs를 project_root에 대해 실행
     2. Chain.json을 읽어서 알아낸 Chain들 중, Chain을 compilation_unit_subdir에 대해 파티션
     3. 각 파티션들에 대해, 파라미터화된 get_all_vertices를 적용 -> 각 compilation unit에 대한 vertices
     4. edge_list_of_chain_slice_list를 각 파티션들에 대해 적용 -> 각 compilation unit에 대한 edges *)

  let root_dir = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan"

  let compilation_unit_subdir_classnames_no_test =
    get_compilation_unit_subdirs root_dir
    >>| (fun subdir -> walk_for_extension subdir ".java")
    >>| List.filter ~f:(not << String.is_substring ~substring:"/test/")
    (* leave only the *.java *)
    >>| List.map ~f:(fun absdir -> List.last_exn @@ String.split ~on:'/' absdir)
    (* leave only the classname *)
    >>| List.map ~f:(fun filename -> List.hd_exn @@ String.split ~on:'.' filename)


  (* wrapping above into a function, we get: *)

  let classnames_by_compilation_unit_no_test (root_dir : string) : string list list =
    get_compilation_unit_subdirs root_dir
    >>| (fun subdir -> walk_for_extension subdir ".java")
    >>| List.filter ~f:(not << String.is_substring ~substring:"/test/")
    (* leave only the *.java *)
    >>| List.map ~f:(fun absdir -> List.last_exn @@ String.split ~on:'/' absdir)
    (* leave only the classname *)
    >>| List.map ~f:(fun filename -> List.hd_exn @@ String.split ~on:'.' filename)


  (* now we prototype the above "collect_chains_belonging_to_compilation_unit". *)

  let renderer_classname_list = List.hd_exn @@ classnames_by_compilation_unit_no_test root_dir

  let all_chains = all_chains_of_json json

  let renderer_chains =
    List.filter
      ~f:(fun chain ->
        List.for_all
          ~f:(fun chain_slice ->
            let current_method = ChainSlice.get_current_method chain_slice in
            if
              Method.is_frontend current_method
              || NodeWiseFeatures.SingleFeature.is_library_code (Method.of_string current_method)
            then true
            else
              (* UNSURE Uh... will this introduce a dependency cycle..?? *)
              let current_method_classname =
                Method.get_class_name (Method.of_string current_method)
              in
              List.mem ~equal:String.equal renderer_classname_list current_method_classname )
          chain )
      all_chains


  let collect_chains_belonging_to_compilation_unit all_chains classname_list =
    List.filter
      ~f:(fun chain ->
        List.exists
          ~f:(fun chain_slice ->
            let current_method = ChainSlice.get_current_method chain_slice in
            if
              Method.is_frontend current_method
              || NodeWiseFeatures.SingleFeature.is_library_code (Method.of_string current_method)
            then false
            else
              (* UNSURE Uh... will this introduce a dependency cycle..?? *)
              let current_method_classname =
                Method.get_class_name (Method.of_string current_method)
              in
              List.mem ~equal:String.equal classname_list current_method_classname )
          chain )
      all_chains


  let partition_chains_by_classname_list (classname_lists : string list list)
      (all_chains : ChainSlice.t list list) : ChainSlice.t list list list =
    classname_lists >>| collect_chains_belonging_to_compilation_unit all_chains


  let _ =
    partition_chains_by_classname_list (classnames_by_compilation_unit_no_test root_dir) all_chains


  (* let _ = collect_chains_belonging_to_compilation_unit all_chains renderer_classname_list *)

  (* it works, but it takes too long. We need a more efficient solution. *)

  let _ = End
end

module Notebook42 = struct
  (* we switch for_all to exists, with true to false. *)

  let collect_chains_belonging_to_compilation_unit all_chains classname_list =
    List.filter
      ~f:(fun chain ->
        List.exists
          ~f:(fun chain_slice ->
            let current_method = ChainSlice.get_current_method chain_slice in
            if
              Method.is_frontend current_method
              || NodeWiseFeatures.SingleFeature.is_library_code (Method.of_string current_method)
            then false
            else
              (* UNSURE Uh... will this introduce a dependency cycle..?? *)
              let current_method_classname =
                Method.get_class_name (Method.of_string current_method)
              in
              List.mem ~equal:String.equal classname_list current_method_classname )
          chain )
      all_chains


  let partition_chains_by_classname_list (classname_lists : string list list)
      (all_chains : ChainSlice.t list list) : ChainSlice.t list list list =
    classname_lists >>| collect_chains_belonging_to_compilation_unit all_chains


  let classnames_by_compilation_unit_no_test (root_dir : string) : string list list =
    get_compilation_unit_subdirs root_dir
    >>| (fun subdir -> walk_for_extension subdir ".java")
    >>| List.filter ~f:(not << String.is_substring ~substring:"/test/")
    (* leave only the *.java *)
    >>| List.map ~f:(fun absdir -> List.last_exn @@ String.split ~on:'/' absdir)
    (* leave only the classname *)
    >>| List.map ~f:(fun filename -> List.hd_exn @@ String.split ~on:'.' filename)


  let root_dir = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan"

  let renderer_classname_list = List.hd_exn @@ classnames_by_compilation_unit_no_test root_dir

  let all_chains = all_chains_of_json json

  let _ = collect_chains_belonging_to_compilation_unit all_chains renderer_classname_list

  (* still takes a lot. *)

  let _ = End
end

module Notebook43 = struct
  (* then, we only look at the header of each wrapped_chain. *)

  let all_wrapped_chains = Chain.wrapped_chain_list_of_raw_json json

  let sample_wrapped_chain = List.hd_exn all_wrapped_chains

  let sample_defining_method = Util.member "defining_method" sample_wrapped_chain

  let kernel current_method classname_list =
    if
      Method.is_frontend current_method
      || NodeWiseFeatures.SingleFeature.is_library_code (Method.of_string current_method)
    then false
    else
      (* UNSURE Uh... will this introduce a dependency cycle..?? *)
      let current_method_classname = Method.get_class_name (Method.of_string current_method) in
      List.mem ~equal:String.equal classname_list current_method_classname


  let collect_chains_belonging_to_compilation_unit json classname_list =
    let all_wrapped_chains = Chain.wrapped_chain_list_of_raw_json json in
    let* wrapped_chain = all_wrapped_chains in
    let sample_defining_method =
      Util.to_string @@ Util.member "defining_method" sample_wrapped_chain
    in
    if kernel sample_defining_method classname_list then
      return @@ chain_slice_list_of_wrapped_chain wrapped_chain
    else []


  let root_dir = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan"

  (* let renderer_classname_list = List.hd_exn @@ classnames_by_compilation_unit_no_test root_dir *)

  (* let _ = collect_chains_belonging_to_compilation_unit json renderer_classname_list *)

  (* still takes a lot. *)

  let _ = End
end

module Notebook44 = struct
  (* meh, we then split the graph directly. *)
  let df_graph =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  (* let's write the logic that splits the graph. *)

  let root_dir = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan"

  (* let renderer_classname_list = List.hd_exn @@ classnames_by_compilation_unit_no_test root_dir *)

  let all_df_edges = G.fold_edges_e List.cons df_graph []

  exception ThisIsImpossible

  exception NotIndependent

  let renderer_graph =
    let relevant_edges =
      List.filter all_df_edges ~f:(fun (v1, _, v2) ->
          let m1 = Vertex.get_method v1 and m2 = Vertex.get_method v2 in
          match (Method.get_kind m1, Method.get_kind m2) with
          | UDF {methname= s1}, UDF {methname= s2} ->
              if String.equal s1 "renderer" && String.equal s2 "renderer" then true
              else raise NotIndependent
          | UDF {methname}, API _ ->
              if String.equal methname "renderer" then true else false
          | API _, UDF {methname} ->
              if String.equal methname "renderer" then true else false
          | API _, API _ ->
              raise ThisIsImpossible )
    in
    List.fold relevant_edges
      ~f:(fun current_graph edge -> G.add_edge_e current_graph edge)
      ~init:G.empty


  (* whoa I just killed Utop *)

  let _ = End
end

(*   (Failure "Could not find comp unit for void BadgeSvg$Path.setDraw(String)")   *)

module Notebook45 = struct
  let sample = "void BadgeSvg$Path.setDraw(String)"

  let _ = assert (Str.string_match NormalString.normalstring_regex sample 0)

  let _ =
    print_endline @@ Str.matched_group 1 sample ;
    print_endline @@ Str.matched_group 2 sample ;
    print_endline @@ Str.matched_group 3 sample


  let sample2 = "LinkRenderer$Rendering MarkdownToHtmlSerializer.createAnchorLink(String)"

  let _ = assert (Str.string_match NormalString.normalstring_regex sample2 0)

  let _ =
    print_endline @@ Str.matched_group 1 sample2 ;
    print_endline @@ Str.matched_group 2 sample2 ;
    print_endline @@ Str.matched_group 3 sample2


  let _ = Classnames.classnames_by_compilation_unit (Deserializer.deserialize_config ())

  let _ = Method.comp_unit_of_methname sample2

  (* Oops. This is emitting the error! *)

  let root_dir = Deserializer.deserialize_config ()

  let abs_dirs_and_classnames = Classnames.classnames_by_compilation_unit root_dir

  let methname = sample2

  let out =
    List.find
      ~f:(fun (abs_dir, classnames) ->
        let class_name = get_class_name_of_methname methname in
        List.mem classnames class_name ~equal:String.equal )
      abs_dirs_and_classnames


  let _ =
    match out with
    | None ->
        failwithf "Could not find comp unit for %s" methname ()
    | Some (abs_dir, _) ->
        List.last_exn @@ String.split ~on:'/' abs_dir


  let _ = walk_for_extension (root_dir ^ "sagan-renderer/") ".java"

  (* 허미.... 이런 게 있네;; LinkRenderer$Rendering LinkRenderer$Rendering.withAttribute(String,String) 은 API인데
     skip_func에 없네;;; *)

  (* 이런 경우는 그냥 API로 넘겨버리자. *)

  let model = "Model Model.addAttribute(String,Object)"

  let _ = Method.is_udf_code_methname model

  let _ = End
end

(* we should check if we can find comp_units for every udf_methods. *)

module Notebook46 = struct
  let sample = "CacheManager CloudFoundryCacheConfig.redisCacheManager(RedisConnectionFactory,ObjectMapper,SiteProperties)"

  let _ = Method.is_udf_code_methname sample

  (* 으아아악 젠장 뭐가 문젠지 알겠다ㅏㅏㅏ *)

  let _ = End
end
