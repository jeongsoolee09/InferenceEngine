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
open GraphSplitter
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

  (* let _ = Method.comp_unit_of_methname sample2 *)

  (* Oops. This is emitting the error! *)

  let root_dir = Deserializer.deserialize_config ()

  let abs_dirs_and_classnames = Classnames.classnames_by_compilation_unit root_dir

  let methname = sample2

  let out =
    List.find
      ~f:(fun (abs_dir, classnames) ->
        let class_name = get_class_name methname in
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

  let _ = Method.is_udf model

  let _ = End
end

(* we should check if we can find comp_units for every udf_methods. *)

module Notebook46 = struct
  let sample =
    "CacheManager \
     CloudFoundryCacheConfig.redisCacheManager(RedisConnectionFactory,ObjectMapper,SiteProperties)"


  let _ = Method.is_udf sample

  (* 으아아악 젠장 뭐가 문젠지 알겠다ㅏㅏㅏ *)

  let _ = End
end

module Notebook47 = struct
  let df_edges_added =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let _ = create_comp_unit_lookup_table (G.all_methods_of_graph df_edges_added)

  let method_ =
    "CacheManager \
     CloudFoundryCacheConfig.redisCacheManager(RedisConnectionFactory,ObjectMapper,SiteProperties)"


  let root_dir = Deserializer.deserialize_config ()

  (* this was the bottleneck, and we memoized it!! *)
  let abs_dirs_and_classnames = DirectoryManager.Classnames.classnames_by_compilation_unit root_dir

  let out =
    List.find
      ~f:(fun (abs_dir, classnames) ->
        List.mem classnames (Method.get_class_name method_) ~equal:String.equal )
      abs_dirs_and_classnames


  let _ = get_comp_unit method_

  let _ = End

  (* Memoization Rocks!!!!!!!!!!!! *)
end
