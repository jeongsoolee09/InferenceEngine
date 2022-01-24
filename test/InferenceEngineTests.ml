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
open Trunk
module Json = Yojson.Basic

exception End

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
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
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

module Notebook49 = struct
  let test_dist : ProbQuadruple.t = {src= 0.4; sin= 0.2; san= 0.2; non= 0.2}

  let _ = DistManipulator.bump test_dist [Source] ~inc_delta:0.3 ~dec_delta:0.1

  let _ = DistManipulator.bump test_dist [Source; Sink] ~inc_delta:0.3 ~dec_delta:0.1

  let _ = DistManipulator.bump test_dist [] ~inc_delta:0. ~dec_delta:0.1

  (* DistManipulator.dump is working well!! *)

  let _ = End
end

module Notebook50 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let all_vertices = G.all_vertices_of_graph df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let renderer_vertices = G.all_vertices_of_graph renderer_graph

  (* ============ site ============ *)
  let site_graph = List.nth_exn splitted 1

  let site_vertices = G.all_vertices_of_graph site_graph

  (* now, get all the ns edges of renderer_vertices. *)

  let renderer_methods =
    List.filter ~f:(not << is_frontend) @@ G.all_methods_of_graph renderer_graph


  let _ =
    SimilarVertexPairExtractor.NodewisePairExtractor.init_nodewise_similarity_map "sagan_renderer"
      renderer_methods


  (* Exception: (Not_found_s "List.find_exn: not found")
     --> uh... what??? *)

  (* --> we need a package scraper. *)

  let _ = End
end

module Notebook51 = struct
  let regex = Re2.create_exn "import ([a-z.]+\.[A-Za-z]+);"

  let is_import_stmt = Re2.matches regex

  let extract_package_from_import_stmt (import_stmt : string) : string =
    Option.value_exn (Re2.find_submatches_exn regex import_stmt |> Array.last)


  (* TODO: memoize this shit *)
  let scrape_packages_in_single_file file_absdir =
    let file_line_by_line = In_channel.read_lines file_absdir in
    let import_lines = List.filter file_line_by_line ~f:is_import_stmt in
    import_lines >>| extract_package_from_import_stmt


  let scrape_packages_in_directory (root_dir : string) : string list =
    let java_files = walk_for_extension root_dir ".java" in
    java_files >>= scrape_packages_in_single_file


  let _ =
    extract_package_from_import_stmt "import org.springframework.context.annotation.Configuration;"


  (* put this in DirectoryManager *)
  let _ = End
end

module Notebook52 = struct
  let sample = "Attributes.<init>()"

  let sample_dir =
    "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan//sagan-site/src/main/java/sagan/site/events/InvalidCalendarException.java"


  let sample_dir2 =
    "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan/sagan-renderer/src/main/java/sagan/renderer/markup/AsciidoctorRenderer.java"


  let sample_dir3 =
    "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan/sagan-renderer/src/main/java/sagan/renderer/guides/content/AsciidoctorGuideContentContributor.java"


  let _ = PackageScraper.scrape_imports_in_single_file sample_dir

  let _ = PackageScraper.scrape_imports_in_single_file sample_dir2

  let _ = PackageScraper.scrape_imports_in_single_file sample_dir3

  let _ = get_package_name sample

  let _ = End
end

module Notebook53 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let all_vertices = G.all_vertices_of_graph df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let renderer_vertices = G.all_vertices_of_graph renderer_graph

  (* ============ site ============ *)
  let site_graph = List.nth_exn splitted 1

  let site_vertices = G.all_vertices_of_graph site_graph

  (* now, get all the ns edges of renderer_vertices. *)

  let renderer_methods = G.all_methods_of_graph renderer_graph

  let _ =
    SimilarVertexPairExtractor.NodewisePairExtractor.init_nodewise_similarity_map "sagan_renderer"
      renderer_methods


  let _ = Method.PackageResolver.resolve_via_package_decls "GuideResource.<init>(Repository)"

  let method_ = "GuideResource.<init>(Repository)"

  let _ = find_unique_identifier method_

  let _ =
    if is_initializer method_ then
      let java_filenames =
        DirectoryManager.walk_for_extension (Deserializer.deserialize_config ()) ".java"
      in
      let classname = get_class_name method_ in
      match
        List.find
          ~f:(fun java_filename ->
            let filename_only = List.last_exn @@ String.split ~on:'/' java_filename in
            String.equal filename_only (classname ^ ".java") )
          java_filenames
      with
      | Some java_filename ->
          List.hd_exn
          @@ DirectoryManager.PackageScraper.scrape_package_decls_in_single_file java_filename
      | None ->
          Core_kernel.failwithf "resolving via package decls failed: %s\n" method_ ()
    else
      Core_kernel.failwithf "non-initializer method resolving is not yet supported: %s\n" method_ ()


  (* Exception: *)
  (* (Failure "could not get package name for GuideResource.<init>(Repository)\n") *)

  (* why..??? *)

  (* don't know... for some reason, it's missing. *)

  let _ = End
end

module Notebook55 = struct
  (* it's obvious that parmap is not a solution, but we bet on making an array. *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let all_vertices = G.all_vertices_of_graph df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let renderer_vertices = G.all_vertices_of_graph renderer_graph

  (* ============ site ============ *)
  let site_graph = List.nth_exn splitted 1

  let site_vertices = G.all_vertices_of_graph site_graph

  (* now, get all the ns edges of renderer_vertices. *)

  let renderer_methods = G.all_methods_of_graph renderer_graph

  let map_array =
    NodeWiseSimilarityMap.make_empty renderer_methods
    |> fun map ->
    NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
    |> List.filter ~f:(fun (m1, m2) ->
           (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
    |> Array.of_list


  let _ = Array.length map_array

  let mapped =
    Array.iteri
      ~f:(fun index pair ->
        if index mod 10 = 0 then print_endline "check" ;
        ignore @@ SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity pair )
      map_array


  let _ = End
end

module Notebook56 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let all_vertices = G.all_vertices_of_graph df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let renderer_vertices = G.all_vertices_of_graph renderer_graph

  let renderer_methods = G.all_methods_of_graph renderer_graph (* Without Test Codes *)

  (* 근데 일단 visualization부터 해 보자. *)

  let _ = Visualizer.visualize_snapshot renderer_graph ~micro:false ~autoopen:false

  let _ = Trunk.identify_longest_trunks renderer_graph

  (* it works *instantaneously* on renderer_graph!!!! *)

  (* ============ site ============ *)

  let site_graph = List.nth_exn splitted 1

  let site_vertices = G.all_vertices_of_graph site_graph

  let site_methods = G.all_methods_of_graph site_graph (* Without Test Codes *)

  let identify_trunks (graph : G.t) : t array =
    let df_only_graph = G.leave_only_df_edges graph in
    let roots = G.collect_df_roots df_only_graph in
    let leaves = G.collect_df_leaves df_only_graph in
    let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
    (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
    let reachable_root_and_leaf_pairs =
      List.filter ~f:(fun (root, leaf) -> PathUtils.is_reachable root leaf df_only_graph) carpro
    in
    (* now, find the path between the root and the leaf. *)
    Array.concat
    @@ List.map
         ~f:(fun (root, leaf) ->
           PathUtils.find_paths_from_source_to_dest df_only_graph (G.LiteralVertex.of_vertex root)
             leaf )
         reachable_root_and_leaf_pairs


  let all_trunks = identify_trunks site_graph

  let _ = Array.length all_trunks (* 1776 *)

  let all_shortest_trunks = Trunk.identify_longest_trunks site_graph

  let _ = Array.length all_shortest_trunks (* 1252 *)

  (* 엄청나게 많이는 아니지만 꽤 줄긴 하네 *)

  let _ = End
end

module Notebook57 = struct
  let list = List.init ~f:(fun i -> i) 90000000

  let array = Array.init ~f:(fun i -> i) 90000000

  let list1 = List.init ~f:(fun i -> i) 90000000

  let list2 = List.init ~f:(fun i -> i) 90000000

  let array1 = Array.init ~f:(fun i -> i) 90000000

  let array2 = Array.init ~f:(fun i -> i) 90000000

  let shortlist1 = List.init ~f:(fun i -> i) 10000

  let shortlist2 = List.init ~f:(fun i -> i) 10000

  let shortarray1 = Array.init ~f:(fun i -> i) 10000

  let shortarray2 = Array.init ~f:(fun i -> i) 10000

  let test_list_fold =
    (* 3 seconds *)
    let start = Unix.time () in
    let _ = List.fold ~f:Int.( + ) ~init:0 list in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_fold =
    (* 3 seconds *)
    let start = Unix.time () in
    let _ = Array.fold ~f:Int.( + ) ~init:0 array in
    let end_ = Unix.time () in
    end_ -. start


  let test_list_map =
    (* 32 seconds *)
    let start = Unix.time () in
    let _ = List.map ~f:(fun x -> x + 1) list in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_map =
    (* 8 seconds *)
    let start = Unix.time () in
    let _ = Array.map ~f:Int.(fun x -> x + 1) array in
    let end_ = Unix.time () in
    end_ -. start


  let test_list_rev =
    (* 14 seconds *)
    let start = Unix.time () in
    let _ = List.rev list in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_rev =
    (* 5 seconds *)
    let start = Unix.time () in
    Array.rev_inplace array ;
    let end_ = Unix.time () in
    end_ -. start


  let test_list_append =
    (* 43 seconds *)
    let start = Unix.time () in
    (* Pervasives.( @ ) is not tail-recursive *)
    let _ = List.append list1 list2 in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_append =
    (* 3~15 seconds depending on cache hit/miss (maybe?) *)
    let start = Unix.time () in
    let _ = Array.append array1 array2 in
    let end_ = Unix.time () in
    end_ -. start


  let test_list_length =
    (* 8-9 seconds *)
    let start = Unix.time () in
    let _ = List.length list in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_length =
    (* 1-2 seconds *)
    let start = Unix.time () in
    let _ = Array.length array in
    let end_ = Unix.time () in
    end_ -. start


  let test_list_carpro =
    (* 54 second *)
    let start = Unix.time () in
    let _ =
      let* elem1 = shortlist1 in
      let* elem2 = shortlist2 in
      return (elem1, elem2)
    in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_carpro =
    (* 18 seckiya *)
    let start = Unix.time () in
    let _ = Array.cartesian_product shortarray1 shortarray2 in
    let end_ = Unix.time () in
    end_ -. start


  (* Array iteration is much faster *)

  let _ = End
end

module Notebook58 = struct
  (* Map.fold vs Array.fold *)

  module IntMap = Caml.Map.Make (Int)

  let shortarray1 = Array.init ~f:(fun i -> i) 30000000

  let map = Array.fold ~f:(fun acc elem -> IntMap.add elem elem acc) ~init:IntMap.empty shortarray1

  let array = Array.init ~f:(fun elem -> (elem, elem)) 30000000

  let test_map_fold =
    (* 2 seconds *)
    let start = Unix.time () in
    let _ = IntMap.fold (fun _ v acc -> v + acc) map 0 in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_fold =
    (* 1 seconds *)
    let start = Unix.time () in
    let _ = Array.fold ~f:(fun acc (_, v) -> v + acc) ~init:0 array in
    let end_ = Unix.time () in
    end_ -. start


  let test_map_filter =
    (* 8 seconds *)
    let start = Unix.time () in
    let _ = IntMap.filter (fun _ v -> v mod 2 = 0) map in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_filter =
    (* 2 seconds *)
    let start = Unix.time () in
    let _ = Array.filter ~f:(fun (_, v) -> v mod 2 = 0) array in
    let end_ = Unix.time () in
    end_ -. start


  let test_map_map =
    (* 14 seconds *)
    let start = Unix.time () in
    let _ = IntMap.map (fun _ v -> v * 2) map in
    let end_ = Unix.time () in
    end_ -. start


  let test_array_filter =
    (* 2 seconds *)
    let start = Unix.time () in
    let _ = Array.map ~f:(fun (_, v) -> v * 2) array in
    let end_ = Unix.time () in
    end_ -. start


  let _ = End
end

module Notebook59 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let methods = Deserializer.deserialize_method_txt ()

  let graph_vertices = VertexMaker.get_all_vertices json

  let graph_methods = G.all_methods_of_graph df_edges_added

  let _ = End
end

module Notebook60 = struct
  (* Debugging Axioms *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  (* mark_well_known_java_methods *)
  (* this_project_main_is_none *)
  (* internal_udf_vertex_is_none *)

  let test_mark_well_known_java_methods =
    (* 1. how many are well known java methods in this project? *)
    let sagan_well_known_java_methods =
      List.filter ~f:Method.is_well_known_java_none_method (G.all_methods_of_graph df_edges_added)
      (* length: 9 *)
    in
    (* 2. are they updated successfully? *)
    let sagan_well_known_java_method_vertices =
      sagan_well_known_java_methods >>= G.this_method_vertices df_edges_added
      (* length: 24 *)
    in
    ()


  (* Lesson (TODO): we have to add more to JavaExpert.none_methods! *)

  let test_this_project_main_is_none =
    (* 1. how many mains are there? *)
    let sagan_well_known_main_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if Method.is_main_method (Vertex.to_string vertex) then vertex :: acc else acc )
        df_edges_added []
    in
    ()


  let test_internal_udf_vertex_is_none =
    (* 1. how many internal udfs are there? *)
    let sagan_udf_vertices =
      (* 1156 *)
      let time1 = Unix.time () in
      let out =
        G.fold_vertex
          (fun vertex acc -> if Method.is_udf (Vertex.get_method vertex) then vertex :: acc else acc)
          df_edges_added []
      in
      let time2 = Unix.time () in
      print_endline @@ F.asprintf "%f" (time2 -. time1) ;
      out
    in
    let sagan_internal_udf_vertices =
      let time1 = Unix.time () in
      let out =
        List.filter
          ~f:(fun vertex -> G.is_df_internal (G.LiteralVertex.of_vertex vertex) df_edges_added)
          sagan_udf_vertices
      in
      let time2 = Unix.time () in
      print_endline @@ F.asprintf "%f" (time2 -. time1) ;
      out
    in
    let sagan_internal_udf_vertices_no_annot =
      let time1 = Unix.time () in
      let out =
        List.filter
          ~f:(fun vertex ->
            let annot = vertex |> Vertex.get_method |> Annotations.get_annots in
            not @@ Annotations.equal annot Annotations.empty )
          sagan_internal_udf_vertices
      in
      let time2 = Unix.time () in
      print_endline @@ F.asprintf "%f" (time2 -. time1) ;
      out
    in
    ()


  let internal_udf_vertices_no_annot =
    let time1 = Unix.time () in
    let out =
      G.fold_vertex
        (fun vertex acc ->
          if
            Method.is_udf (Vertex.get_method vertex)
            && G.is_df_internal (G.LiteralVertex.of_vertex vertex) df_edges_added
            &&
            let annot = vertex |> Vertex.get_method |> Annotations.get_annots in
            Annotations.equal annot Annotations.empty
          then vertex :: acc
          else acc )
        df_edges_added []
    in
    let time2 = Unix.time () in
    print_endline @@ F.asprintf "%f" (time2 -. time1) ;
    out


  let _ = List.length @@ internal_udf_vertices_no_annot

  (* 왜 이거밖에 없지?? *)

  (* 14 seconds *)

  (* 오 쩐다 갑자기 최적화됐네 *)

  (* Lesson (TODO): short-circuiting 을 잘 활용하자. *)

  (* +) 나중에 visualization 해가면서 보자. *)

  let _ = End
end

module Notebook61 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let axiom_applied = Axioms.apply_axioms df_edges_added

  let _ =
    List.length
    @@ G.fold_vertex
         (fun vertex acc ->
           if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
         axiom_applied []


  let _ =
    assert (
      Int.( = )
        (List.length @@ G.all_vertices_of_graph df_edges_added)
        (List.length @@ G.all_vertices_of_graph axiom_applied) )


  (* sanity check pass *)

  let unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      axiom_applied []


  let marked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if not @@ ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc
        else acc )
      axiom_applied []


  let all_methods = G.all_methods_of_graph df_edges_added

  let marked_methods = marked_vertices >>| Vertex.get_method |> List.stable_dedup (* 564 *)

  let unmarked_methods = unmarked_vertices >>| Vertex.get_method |> List.stable_dedup (* 316 *)

  let unmarked_udfs = List.filter ~f:Method.is_udf unmarked_methods

  let marked_udfs = List.filter ~f:Method.is_udf marked_methods

  let _ = List.length unmarked_udfs (* 188 *)

  let _ = List.length marked_udfs (* 334 *)

  let unmarked_udfs_without_annots =
    List.filter
      ~f:(fun meth -> not @@ Annotations.equal Annotations.empty (Annotations.get_annots meth))
      unmarked_udfs


  let _ = List.length unmarked_udfs_without_annots

  (* Curious: how many of unmarked_udfs are not annotated? *)

  let _ = List.length marked_udfs

  let _ = List.length unmarked_methods

  let _ = End
end

module Notebook62 = struct
  (* testing nodewise similarity *)
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let axiom_applied = Axioms.apply_axioms df_edges_added

  let unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      axiom_applied []


  let unmarked_methods = unmarked_vertices >>| Vertex.get_method |> List.stable_dedup (* 312 *)

  let _ = EstablishSimEdges.make_nodewise_sim_edge axiom_applied

  let _ = End
end

module Notebook63 = struct
  (* profiling make_nodewise_sim_edge *)
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let axiom_applied = Axioms.apply_axioms df_edges_added

  let nodewise_sim_map =
    let time1 = Unix.time () in
    let nodewise_sim_map = EstablishSimEdges.make_nodewise_sim_edge axiom_applied in
    let time2 = Unix.time () in
    print_endline @@ F.asprintf "make_nodewise_sim_edge took %f seconds" (time2 -. time1) ;
    nodewise_sim_map


  let unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      axiom_applied []


  let unmarked_methods = unmarked_vertices >>| Vertex.get_method |> List.stable_dedup (* 312 *)

  let unmarked_apis =
    (* 101 *)
    unmarked_vertices >>| Vertex.get_method |> List.stable_dedup |> List.filter ~f:Method.is_api


  let unmarked_udfs =
    (* 186 *)
    unmarked_vertices >>| Vertex.get_method |> List.stable_dedup |> List.filter ~f:Method.is_udf


  let what_the_hell =
    List.filter
      ~f:(fun meth ->
        (not @@ List.mem ~equal:Method.equal unmarked_apis meth)
        && (not @@ List.mem ~equal:Method.equal unmarked_udfs meth) )
      unmarked_methods


  (* 와 뭥미ㅋㅋㅋ *)

  (* 자바는 *메소드 접두사* 가 엄청 중요하긴 하지.ㅋㅋ 그래도 udf/api 차이에 신경쓸 것.
     get는 getter일수도 있지만 POST/GET의 GET일 수도 있거든. *)

  (* 나머지 애들은 어디갔냐;;;;?? *)

  let graph = axiom_applied

  let all_methods = unmarked_methods

  (* uhhh.. making nodewise_sim_map takes quite a lot *)

  let out =
    let map_array =
      NodeWiseSimilarityMap.make_empty all_methods
      |> fun map ->
      NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
      |> List.filter ~f:(fun (m1, m2) ->
             (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
      |> Array.of_list
    in
    let mapped =
      Array.map
        ~f:(fun pair ->
          (pair, SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity pair) )
        map_array
    in
    mapped |> Array.to_list |> NodeWiseSimilarityMap.of_alist


  let _ = End
end

module Notebook64 = struct
  (* 아이디어: 될놈만 짝지어 주기 *)

  (* TODO UDF끼리, API끼리 *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let axiom_applied = Axioms.apply_axioms df_edges_added

  let unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      axiom_applied []


  let unmarked_methods = unmarked_vertices >>| Vertex.get_method |> List.stable_dedup (* 312 *)

  let unmarked_apis =
    (* 126 *)
    unmarked_vertices >>| Vertex.get_method |> List.stable_dedup |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_frontend)


  let _ = List.length unmarked_apis

  let unmarked_udfs =
    (* 178 *)
    unmarked_vertices >>| Vertex.get_method |> List.stable_dedup |> List.filter ~f:Method.is_udf


  let _ = List.length unmarked_udfs

  (* 어케이~~~ *)

  (* making for apis *)

  let api_ns_map =
    (* 6 minutes *)
    let map_array =
      NodeWiseSimilarityMap.make_empty unmarked_apis
      |> fun map ->
      NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
      |> List.filter ~f:(fun (m1, m2) ->
             (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
      |> Array.of_list
    in
    let time1 = Unix.time () in
    let mapped =
      Array.map
        ~f:(fun pair ->
          (pair, SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity pair) )
        map_array
    in
    let time2 = Unix.time () in
    print_endline @@ F.asprintf "%f" (time2 -. time1) ;
    mapped |> Array.to_list |> NodeWiseSimilarityMap.of_alist


  (* 어떻게 최적화하지?? 모르겟다... *)
  (* 일단 m1맥에서 돌려보기~~~ *)
  (* --> m1맥에서 6분 걸림 *)

  let udf_ns_map =
    (* 9 minutes *)
    let map_array =
      NodeWiseSimilarityMap.make_empty unmarked_udfs
      |> fun map ->
      NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
      |> List.filter ~f:(fun (m1, m2) ->
             (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
      |> Array.of_list
    in
    let time1 = Unix.time () in
    let mapped =
      Array.map
        ~f:(fun pair ->
          (pair, SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity pair) )
        map_array
    in
    let time2 = Unix.time () in
    print_endline @@ F.asprintf "%f" (time2 -. time1) ;
    mapped |> Array.to_list |> NodeWiseSimilarityMap.of_alist


  (* *결국 쪼개야 한다는 결론에 도달.* *)
  (* --> 쪼개고, 그 안에서 marked/unmarked로 나누기. *)

  let _ = End
end

(* 그럼 일단 쪼개고, 각각의 쪼개진 그래프들에 대해서 위처럼 marked/unmarked를 나눠 주면 되는 것이다!!! *)

module Notebook65 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let axiom_applied = Axioms.apply_axioms df_edges_added

  let splitted = split_graph_by_comp_unit axiom_applied

  let renderer_graph = List.nth_exn splitted 0

  let site_graph = List.nth_exn splitted 1

  let renderer_graph_vertices = G.all_vertices_of_graph renderer_graph

  let renderer_unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      renderer_graph []


  let renderer_unmarked_methods =
    renderer_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup


  let renderer_unmarked_apis =
    renderer_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_frontend)


  let renderer_unmarked_udfs =
    renderer_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_udf


  let _ = List.length renderer_graph_vertices (* 311 *)

  let _ = List.length renderer_unmarked_vertices (* 114 *)

  let _ = List.length renderer_unmarked_methods (* 51 *)

  (* finally, a managable size!! *)

  let site_graph_vertices = G.all_vertices_of_graph site_graph

  let site_unmarked_vertices =
    G.fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      site_graph []


  let site_unmarked_methods = site_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup

  let _ = List.length site_graph_vertices (* 1645 *)

  let _ = List.length site_unmarked_vertices (* 684 *)

  let _ = List.length site_unmarked_methods (* 280 *)

  (* break it down a bit more *)

  let site_unmarked_apis =
    site_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_frontend)


  let site_unmarked_udfs =
    site_unmarked_vertices >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_udf


  let _ = List.length site_unmarked_apis (* 97 *)

  let _ = List.length site_unmarked_udfs (* 162 *)

  (* OK, much more managable. *)

  (* Let's batch-run everything! *)

  let make_ns_map all_methods =
    let map_array =
      NodeWiseSimilarityMap.make_empty all_methods
      |> fun map ->
      NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
      |> List.filter ~f:(fun (m1, m2) ->
             (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
      |> Array.of_list
    in
    Out_channel.print_endline
    @@ Format.asprintf "domain size of nodewise sim map is %d\n" (Array.length map_array) ;
    let mapped =
      Array.map
        ~f:(fun pair ->
          (pair, SimilarVertexPairExtractor.NodewisePairExtractor.get_nodewise_similarity pair) )
        map_array
    in
    mapped |> Array.to_list |> NodeWiseSimilarityMap.of_alist


  let _ =
    let time1 = Unix.time () in
    let renderer_udf_ns_map = make_ns_map renderer_unmarked_udfs
    and renderer_api_ns_map = make_ns_map renderer_unmarked_apis
    and site_udf_ns_map = make_ns_map site_unmarked_udfs
    and site_api_ns_map = make_ns_map site_unmarked_apis in
    let time2 = Unix.time () in
    time2 -. time1


  (* 아 ㅋㅋㅋㅋㅋ 시발 개빡치게하네 ^^ *)

  let _ = End
end

module Notebook66 = struct
  let method_ = "Post PostRepository.findByPublicSlugAndDraftFalseAndPublishAtBefore(String,Date)"

  let other_method_with_same_classname =
    List.find_exn
      ~f:(fun other_method ->
        String.equal (Method.get_class_name method_) (Method.UniqueID.get_class_name other_method)
        )
      (Deserializer.deserialize_skip_func () @ Deserializer.deserialize_method_txt ())


  let _ = Method.get_package_name method_

  let get_package_name (method_ : Method.t) : string =
    try
      let unique_id = find_unique_identifier method_ in
      UniqueID.get_package_name unique_id
    with Core_kernel.Not_found_s _ ->
      let other_method_with_same_classname =
        List.find_exn
          ~f:(fun other_method ->
            String.equal (get_class_name method_) (UniqueID.get_class_name other_method) )
          (Deserializer.deserialize_skip_func () @ Deserializer.deserialize_method_txt ())
      in
      UniqueID.get_package_name other_method_with_same_classname


  let _ = get_package_name method_

  let _ = End
end
