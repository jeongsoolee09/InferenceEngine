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
open Annotations
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

  let _ = List.length abs_dirs_and_classnames

  let out =
    List.find
      ~f:(fun (abs_dir, classnames) ->
        List.mem classnames (Method.get_class_name method_) ~equal:String.equal )
      abs_dirs_and_classnames


  let _ = get_comp_unit method_

  (* Memoization Rocks!!!!!!!!!!!! *)

  let all_vertices = G.all_vertices_of_graph df_edges_added

  let _ = List.length all_vertices

  let renderer_graph = List.nth_exn splitted 0

  let renderer_vertices = G.all_vertices_of_graph renderer_graph

  let _ = List.length renderer_vertices

  let site_graph = List.nth_exn splitted 1

  let site_vertices = G.all_vertices_of_graph site_graph

  let _ = List.mem (site_vertices >>| Vertex.get_method) method_ ~equal:String.equal (* OK *)

  let _ = List.length site_vertices

  let _ =
    List.iter all_vertices ~f:(fun vertex ->
        let is_in_renderer = List.mem ~equal:Vertex.equal renderer_vertices vertex
        and is_in_site = List.mem ~equal:Vertex.equal site_vertices vertex in
        if not (is_in_renderer || is_in_site) then print_endline (Vertex.get_method vertex) )


  (* GuideType GuideType.fromName(String) *)
  (* GuideType GuideType.fromRepositoryName(String) *)
  (* LocalDate ZonedDateTime.toLocalDate() *)
  (* Predicate GuideType.callsite_sagan.renderer.guides.GuideType$Lambda$_3_7(String) *)
  (* Predicate GuideType.callsite_sagan.renderer.guides.GuideType$Lambda$_4_7(String) *)
  (* Predicate GuideType.callsite_sagan.site.renderer.GuideType$Lambda$_3_7(String) *)
  (* String GuideType.stripPrefix(String) *)
  (* String GuideType.stripPrefix(String) *)
  (* String String.replaceFirst(String,String) *)
  (* boolean LocalDate.isAfter(ChronoLocalDate) *)
  (* boolean LocalDate.isBefore(ChronoLocalDate) *)
  (* boolean Period.lambda$toCalendarFilter$0(VEvent) *)
  (* void BadgeSvg$GraphicElement.setText(String) *)
  (* void BadgeSvg$GraphicElement.setText(String) *)
  (* void ConfigurableEnvironment.addActiveProfile(String) *)
  (* void RedisAccessor.setConnectionFactory(RedisConnectionFactory) *)

  let _ = End
end

module Comment = struct
  exception End

  let sample = "Model Model.addAttribute(String,Object)"

  let get_annots (method_ : Method.t) : t =
    let annot_str_alist = Deserializer.deserialize_annots () in
    let this_method_annot =
      List.Assoc.find_exn
        ~equal:(fun this method_unique_id ->
          String.equal (Method.find_unique_identifier this) method_unique_id )
        annot_str_alist method_
    in
    of_string this_method_annot


  (* let x = Annotations.get_annots (Method.of_string sample) *)

  (* oh no this is taking too long *)

  let method_ = "Model Model.addAttribute(String,Object)"

  let annot_str_alist = Deserializer.deserialize_annots ()

  let this_method_annot =
    List.Assoc.find_exn
      ~equal:(fun this method_unique_id ->
        String.equal (Method.find_unique_identifier this) method_unique_id )
      annot_str_alist method_


  (* Hey, we need to make the above into a Hashtbl.t to make an O(1) lookup. *)

  let _ = of_string this_method_annot

  (* oooh, library codes don't have annots, which is obvious *)

  let sample2 = "ProjectAdminController.<init>(ProjectMetadataService,PostContentRenderer)"

  (* let x = Annotations.get_annots sample2 *)

  let this_method_annot = List.Assoc.find_exn ~equal:Method.equal annot_str_alist sample2

  let _ = End
end

module Notebook48 = struct
  let sample =
    "<_org.springframework.web.bind.annotation.RequestMapping(value=\"/{username}\", \
     method=\".GET, .HEAD\")> (<_org.springframework.web.bind.annotation.PathVariable> <>)"


  let _ = capture_nonempty_angled_brackets sample

  let string_list = split_single_annot_string sample

  let _ =
    match string_list with
    | [] ->
        failwith "Invalid input string list (is empty)"
    | [name] ->
        {name; params= []}
    | [name; input_sig] ->
        {name; params= split_up_input_sig input_sig}
    | _ ->
        failwith "Invalid input string list (is too long)"


  let input_sig = "(value=\"/{username}\", method=\".GET, .HEAD\")"

  let split_on_comma = String.split ~on:',' input_sig

  let _ =
    split_on_comma
    >>| fun split ->
    let split_on_equal = String.split ~on:'=' split in
    let filter char = Char.equal '(' char || Char.equal ')' char || Char.equal ' ' char in
    ( String.strip ~drop:filter @@ List.nth_exn split_on_equal 0
    , String.strip ~drop:filter @@ List.nth_exn split_on_equal 1 )


  let regex = Re2.create_exn "([^()<> ]+(\([^()<>]*\))?)"

  let sample =
    "<_org.springframework.web.bind.annotation.GetMapping(value=\"/{topical}/images/{image:[a-zA-Z0-9._-]+}\")> \
     (<_org.springframework.web.bind.annotation.PathVariable> \
     <_org.springframework.web.bind.annotation.PathVariable>)"


  let _ = Re2.find_all_exn regex sample

  let sample =
    "_org.springframework.web.bind.annotation.GetMapping(value=\"/{topical}/images/{image:[a-zA-Z0-9._-]+}\")"


  let _ = Re2.find_all_exn regex sample

  let sample =
    "<_org.springframework.context.annotation.Bean(name=\"uih, viewRenderingHelper\") \
     _org.springframework.context.annotation.Scope(value=\"request\")> ()"


  let angled_brackets = capture_nonempty_angled_brackets sample

  let angled_bracket = List.hd_exn angled_brackets

  let split = split_single_annot_string angled_bracket

  let _ = of_string sample

  let sample = "<_com.fasterxml.jackson.annotation.JsonIgnore> ()"

  let brackets = capture_nonempty_angled_brackets sample

  let _ = split_single_annot_string (List.hd_exn brackets)

  let _ = End
end
