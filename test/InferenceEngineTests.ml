open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open SimilarityHandler
open Loop
open AskingRules
open PropagationRules
open MetaRules
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
open SpanningTree
open WeaklyConnectedComponents
open TaintLabel
open EdgeLabel
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

module Notebook100 = struct
  let _ = Start

  (* TODO Debugging SpanningTree *)

  let graph = renderer_graph

  open EdgeLabel
  open TaintLabel

  let sim_edges =
    let api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit in
    let udf_csv_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.csv" graph.comp_unit in
    if not @@ Sys.file_exists_exn api_csv_filename then (
      Out_channel.print_string "spawning python process compute_nodewise_similarity.py..." ;
      SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py"
        ~args:[graph.comp_unit] ) ;
    Out_channel.print_endline "done" ;
    let api_in_chan = In_channel.create api_csv_filename in
    let api_array = Csv.to_array @@ Csv.load_in api_in_chan in
    let udf_in_chan = In_channel.create udf_csv_filename in
    let udf_array = Csv.to_array @@ Csv.load_in udf_in_chan in
    In_channel.close api_in_chan ;
    In_channel.close udf_in_chan ;
    Out_channel.print_string "Now adding NS edges..." ;
    Out_channel.flush stdout ;
    (* make a temporary empty graph *)
    let acc = ref [] in
    Array.iter api_array ~f:(fun array ->
        let method1 = array.(0) and method2 = array.(1) in
        let m1_vertices = G.this_method_vertices graph method1
        and m2_vertices = G.this_method_vertices graph method2 in
        List.iter
          ~f:(fun m1_vertex ->
            List.iter
              ~f:(fun m2_vertex ->
                let m1_vertex_method = Vertex.get_method m1_vertex
                and m2_vertex_method = Vertex.get_method m2_vertex in
                let there_is_no_cs =
                  not
                  @@ List.exists
                       ~f:(fun (v1, _, v2) ->
                         let v1_method = Vertex.get_method v1
                         and v2_method = Vertex.get_method v2 in
                         Method.equal m1_vertex_method v1_method
                         && Method.equal m2_vertex_method v2_method )
                       (G.get_edges graph ~label:ContextualSimilarity)
                in
                if there_is_no_cs then
                  let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
                  and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
                  acc := edge1 :: edge2 :: !acc )
              m2_vertices )
          m1_vertices ) ;
    Array.iter udf_array ~f:(fun array ->
        let method1 = array.(0) and method2 = array.(1) in
        let m1_vertices = G.this_method_vertices graph method1
        and m2_vertices = G.this_method_vertices graph method2 in
        List.iter
          ~f:(fun m1_vertex ->
            List.iter
              ~f:(fun m2_vertex ->
                let m1_vertex_method = Vertex.get_method m1_vertex
                and m2_vertex_method = Vertex.get_method m2_vertex in
                let there_is_no_cs =
                  not
                  @@ List.exists
                       ~f:(fun (v1, _, v2) ->
                         let v1_method = Vertex.get_method v1
                         and v2_method = Vertex.get_method v2 in
                         Method.equal m1_vertex_method v1_method
                         && Method.equal m2_vertex_method v2_method )
                       (G.get_edges graph ~label:ContextualSimilarity)
                in
                if there_is_no_cs then
                  let edge1 = (m1_vertex, NodeWiseSimilarity, m2_vertex)
                  and edge2 = (m2_vertex, NodeWiseSimilarity, m1_vertex) in
                  acc := edge1 :: edge2 :: !acc )
              m2_vertices )
          m1_vertices ) ;
    !acc


  (* testing diet_edge_list *)

  let edgelist = sim_edges

  (* graph containing only the sim_edges *)
  let subgraph = List.fold ~f:G.add_edge_e ~init:G.empty edgelist

  let digraph = subgraph

  let undirected_of_digraph = to_undirected digraph

  (* ============ Validation start ============ *)

  let is_isolated_g (graph : G.t) (vertex : G.V.t) =
    G.in_degree graph vertex = 0 && G.out_degree graph vertex = 0


  let is_isolated_u (graph : U.t) (vertex : U.V.t) =
    U.in_degree graph vertex = 0 && U.out_degree graph vertex = 0


  let undirected_does_not_lose_vertices =
    let digraph_vertices = G.all_vertices_of_graph digraph
    and undigraph_vertices = U.fold_vertex List.cons undirected_of_digraph [] in
    List.length digraph_vertices = List.length undigraph_vertices


  let undirected_has_half_of_edges =
    let digraph_vertices = G.all_vertices_of_graph digraph
    and undigraph_vertices = U.fold_vertex List.cons undirected_of_digraph [] in
    List.length digraph_vertices = List.length undigraph_vertices


  let digraph_has_no_isolated_vertices =
    List.for_all (G.all_vertices_of_graph digraph) ~f:(not << is_isolated_g digraph)


  let undigraph_has_no_isolated_vertices =
    List.for_all
      (U.fold_vertex List.cons undirected_of_digraph [])
      ~f:(not << is_isolated_u undirected_of_digraph)


  let _ =
    List.exists (G.fold_edges_e List.cons digraph []) ~f:(fun (v1, _, v2) ->
        Method.equal (Vertex.get_method v1)
          "ConfigurableApplicationContext SpringApplication.run(Class,String[])"
        || Method.equal (Vertex.get_method v2)
             "ConfigurableApplicationContext SpringApplication.run(Class,String[])" )


  let _ =
    List.exists (U.fold_edges_e List.cons undirected_of_digraph []) ~f:(fun (v1, _, v2) ->
        Method.equal (Vertex.get_method v1)
          "ConfigurableApplicationContext SpringApplication.run(Class,String[])"
        || Method.equal (Vertex.get_method v2)
             "ConfigurableApplicationContext SpringApplication.run(Class,String[])" )


  let _ =
    List.exists sim_edges ~f:(fun (v1, _, v2) ->
        Method.equal (Vertex.get_method v1)
          "ConfigurableApplicationContext SpringApplication.run(Class,String[])"
        || Method.equal (Vertex.get_method v2)
             "ConfigurableApplicationContext SpringApplication.run(Class,String[])" )


  (* all looking good. *)

  (* ============ Validation end ============ *)

  let mst = UKruskal.spanningtree undirected_of_digraph

  let spring_run_exists =
    List.exists
      ~f:(fun (v1, _, v2) ->
        Method.equal (Vertex.get_method v1)
          "ConfigurableApplicationContext SpringApplication.run(Class,String[])"
        || Method.equal (Vertex.get_method v2)
             "ConfigurableApplicationContext SpringApplication.run(Class,String[])" )
      mst


  (* meh. Prim is borken. *)

  let subgraph_pruned =
    G.empty
    |> fun graph ->
    List.fold ~f:G.add_vertex ~init:graph (G.all_vertices_of_graph digraph)
    |> fun graph ->
    List.fold
      ~f:(fun acc (v1, label, v2) ->
        G.add_edge_e acc (v1, label, v2) |> fun g -> G.add_edge_e g (v2, label, v1) )
      ~init:graph mst


  let _ = List.exists ~f:(is_isolated_g subgraph_pruned) (G.all_vertices_of_graph subgraph_pruned)

  let _ = G.all_edges_of_graph subgraph_pruned

  (* DONE: replaced Prim in favor of Kruskal. *)

  let _ = End
end

module Notebook101 = struct
  let _ = Start

  (* TODO debugging building nodewise maps for sagan-site. *)

  let _ = NodeWiseFeatureMap.init (G.all_methods_of_graph site_graph)

  let getMask = "BadgeSvg$Mask BadgeSvg.getMask()"

  let _ = run_all_single_features getMask

  let method_ = getMask

  let get_package_name (method_ : Method.t) : string =
    try
      let unique_id = find_unique_identifier method_ in
      UniqueID.get_package_name unique_id
    with _ -> (
      (* we couldn't find an unique_id for this method. *)
      (* is it an interface method? *)
      let class_name = get_class_name method_ in
      let class_name_file_opt =
        let all_java_files = walk_for_extension Deserializer.project_root ".java" in
        List.find all_java_files ~f:(fun java_file ->
            String.is_substring java_file ~substring:(class_name ^ ".java") )
      in
      match class_name_file_opt with
      | None ->
          let other_method_with_same_classname =
            List.find_exn
              ~f:(fun other_method ->
                String.equal (get_class_name method_) (UniqueID.get_class_name other_method) )
              (Deserializer.deserialize_skip_func () @ Deserializer.deserialize_method_txt ())
          in
          UniqueID.get_package_name other_method_with_same_classname
      | Some class_name_file ->
          List.hd_exn @@ PackageScraper.scrape_package_decls_in_single_file class_name_file )


  let _ = get_package_name getMask

  (* ~/Taint-Analysis/Code/benchmarks/realworld/sagan/sagan-site/src/main/java/sagan/projects/support/BadgeSvg.java *)

  let _ = End
end

module Notebook102 = struct
  let _ = Start

  (* DONE Debugging GetterSetter.ml *)

  let _ = GetterSetter.is_setter "void AtomFeedView.setRenderedContent(Post,Entry)"

  let gettersetter_none_marked =
    Axioms.Distribution.getters_setters_and_predicates_are_none renderer_graph


  let gettersetter_none_marked =
    Axioms.Distribution.getters_setters_and_predicates_are_none site_graph


  let _ = Visualizer.visualize_and_open gettersetter_none_marked

  let unmarked_udfs = G.get_unmarked_udfs gettersetter_none_marked

  let unmarked_apis = G.get_unmarked_apis gettersetter_none_marked

  let _ = List.length unmarked_udfs

  let _ = List.length unmarked_apis

  let _ = G.nb_vertex gettersetter_none_marked

  let _ = End
end

module Notebook103 = struct
  let _ = Start

  (* debugging find_distinct_subgraphs *)

  open EdgeLabel

  let make str : Vertex.t = (str, "", ProbQuadruple.initial)

  let test_graph =
    G.empty
    |> fun g ->
    List.fold
      [ (make "a", DataFlow, make "b")
      ; (make "a", DataFlow, make "c")
      ; (make "b", DataFlow, make "d") ]
      ~f:G.add_edge_e ~init:g
    |> fun g ->
    List.fold
      [ (make "e", DataFlow, make "f")
      ; (make "f", NodeWiseSimilarity, make "g")
      ; (make "g", NodeWiseSimilarity, make "h") ]
      ~f:G.add_edge_e ~init:g


  let _ = Visualizer.visualize_and_open test_graph

  let find_distinct_subgraphs (g : G.t) : G.t array =
    let undirected = to_undirected g in
    let vertices_array = Components.scc_array undirected in
    Array.map vertices_array ~f:(fun vertex_list ->
        G.empty
        |> fun subgraph ->
        List.fold vertex_list ~f:G.add_vertex ~init:subgraph
        |> fun subgraph ->
        List.fold (G.all_edges_of_graph g)
          ~f:(fun acc ((v1, _, v2) as edge) ->
            if List.mem vertex_list v1 ~equal:G.V.equal && List.mem vertex_list v2 ~equal:G.V.equal
            then G.add_edge_e acc edge
            else acc )
          ~init:subgraph )


  let subgraphs = find_distinct_subgraphs test_graph

  let subgraph1 = subgraphs.(0)

  let subgraph2 = subgraphs.(1)

  let _ = Visualizer.visualize_and_open subgraph1

  let _ = Visualizer.visualize_and_open subgraph2

  (* working nicely! *)

  let _ = End
end

module Notebook104 = struct
  let _ = Start

  (* TODO: developing a graph splitter. *)

  let is_isolated graph vertex =
    (Int.equal 0 @@ G.in_degree graph vertex) && (Int.equal 0 @@ G.out_degree graph vertex)


  let subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs renderer_graph

  let x = [1; 2; 3; 4; 5; 6]

  let evens =
    List.rev
    @@ List.foldi ~f:(fun index acc elem -> if index mod 2 = 1 then elem :: acc else acc) ~init:[] x


  let odds =
    List.rev
    @@ List.foldi ~f:(fun index acc elem -> if index mod 2 = 0 then elem :: acc else acc) ~init:[] x


  let subgraphs_sorted =
    Array.sorted_copy subgraphs ~compare:(fun g1 g2 ->
        Int.compare (G.nb_vertex g1) (G.nb_vertex g2) )


  let even_indices =
    List.rev
    @@ Array.foldi
         ~f:(fun index acc elem -> if index mod 2 = 0 then elem :: acc else acc)
         ~init:[] subgraphs_sorted


  let odd_indices =
    List.rev
    @@ Array.foldi
         ~f:(fun index acc elem -> if index mod 2 = 1 then elem :: acc else acc)
         ~init:[] subgraphs_sorted


  let _ = List.map ~f:G.nb_vertex even_indices

  let _ = List.map ~f:G.nb_vertex odd_indices

  let _ = Array.map ~f:G.nb_vertex subgraphs_sorted

  let subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs site_graph

  let subgraphs_sorted =
    Array.sorted_copy subgraphs ~compare:(fun g1 g2 ->
        Int.compare (G.nb_vertex g1) (G.nb_vertex g2) )


  let even_indices =
    List.rev
    @@ Array.foldi
         ~f:(fun index acc elem -> if index mod 2 = 0 then elem :: acc else acc)
         ~init:[] subgraphs_sorted


  let odd_indices =
    List.rev
    @@ Array.foldi
         ~f:(fun index acc elem -> if index mod 2 = 1 then elem :: acc else acc)
         ~init:[] subgraphs_sorted


  let _ = List.map ~f:G.nb_vertex even_indices

  let _ = List.map ~f:G.nb_vertex odd_indices

  let _ = Array.map ~f:G.nb_vertex subgraphs_sorted

  let _ = List.length @@ G.get_unmarked_vertices @@ Axioms.apply_axioms site_graph

  let _ = "아... 약간 빡치는데... 이것도 돌려야 해??ㅡㅡ 몰겠당"

  let _ = End
end

module Notebook105 = struct
  let _ = Start

  (* DONE debugging init_for_graph_apis for sagan_site *)

  let _ = NodeWiseFeatures.NodeWiseFeatureMap.init_for_graph_apis site_graph

  let setDraw = "void BadgeSvg$Path.setDraw(String)"

  let method_ = setDraw

  let normalstring = method_

  let get_package_name (method_ : Method.t) : string =
    try
      let unique_id = find_unique_identifier method_ in
      UniqueID.get_package_name unique_id
    with _ -> (
      (* we couldn't find an unique_id for this method. *)
      (* is it an interface method? *)
      let class_name = get_parent_class_name method_ in
      let class_name_file_opt =
        let all_java_files = walk_for_extension Deserializer.project_root ".java" in
        List.find all_java_files ~f:(fun java_file ->
            String.is_substring java_file ~substring:(class_name ^ ".java") )
      in
      match class_name_file_opt with
      | None ->
          let other_method_with_same_classname =
            List.find_exn
              ~f:(fun other_method ->
                String.equal (get_class_name method_) (UniqueID.get_class_name other_method) )
              (Deserializer.deserialize_skip_func () @ Deserializer.deserialize_method_txt ())
          in
          UniqueID.get_package_name other_method_with_same_classname
      | Some class_name_file ->
          List.hd_exn @@ PackageScraper.scrape_package_decls_in_single_file class_name_file )


  let _ = Method.get_package_name setDraw

  let _ = End
end

module Notebook106 = struct
  let _ = Start

  let _ =
    let api_map = NodeWiseFeatureMap.init_for_graph_apis site_graph
    and udf_map = NodeWiseFeatureMap.init_for_graph_udfs site_graph in
    NodeWiseFeatureMap.CSVSerializer.serialize api_map
      ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" site_graph.comp_unit) ;
    NodeWiseFeatureMap.CSVSerializer.serialize udf_map
      ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" site_graph.comp_unit)


  let _ = End
end

module Notebook107 = struct
  let _ = Start

  (* DONE Memoization test *)

  let _ = G.all_methods_of_graph site_graph

  let _ = G.all_vertices_of_graph site_graph

  let _ = G.all_edges_of_graph site_graph

  let _ = End
end

module Notebook109 = struct
  let _ = Start

  (* TODO debugging @GetMapping reachability *)

  let renderer_finished = build_graph renderer_graph

  let _ = G.get_edges renderer_finished ~label:NodeWiseSimilarity

  let ns_view = G.leave_only_ns_edges renderer_finished

  let ns_subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs ns_view

  let ns_largest_subgraph =
    (Array.sorted_copy ns_subgraphs ~compare:(fun g1 g2 ->
         -Int.compare (G.nb_vertex g1) (G.nb_vertex g2) ) ).(2)


  let _ = Visualizer.visualize_and_open ns_largest_subgraph

  (* Seems like not all @GetMappings are connected to one another. *)

  (* equality on annotations is kinda doubtful *)

  let _ = End
end

module Notebook110 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let ns_view = G.leave_only_ns_edges renderer_finished

  let _ = Visualizer.visualize_and_open ns_view

  (* DONE Working on milestone 1. *)

  (* Testing if File vertices are reachable from one another. *)

  let file_vertices =
    List.filter
      ~f:(fun vertex -> String.equal "File" (vertex |> Vertex.get_method |> Method.get_class_name))
      (G.all_vertices_of_graph renderer_finished)
    |> Array.of_list


  let file_methods =
    List.filter
      ~f:(fun method_ -> String.equal "File" (Method.get_class_name method_))
      (G.all_methods_of_graph renderer_finished)
    |> Array.of_list


  module LV = G.LiteralVertex

  let file_vertices_are_reachable_with_one_another =
    let cartesian_product = Array.cartesian_product file_vertices file_vertices in
    Array.iter cartesian_product ~f:(fun (v1, v2) ->
        let lv1 = LV.of_vertex v1 and lv2 = LV.of_vertex v2 in
        if PathUtils.is_reachable lv1 lv2 ns_view then
          print_endline
          @@ F.asprintf "%s and %s are connected" (LV.to_string lv1) (LV.to_string lv2) )


  let _ =
    let ns_succs_of_file_vertices =
      Array.to_list file_vertices
      >>= fun vertex -> G.get_succs ns_view (LV.of_vertex vertex) ~label:NodeWiseSimilarity
    in
    List.filter
      ~f:(fun succ ->
        not @@ String.equal (succ |> Vertex.get_method |> Method.get_class_name) "File" )
      ns_succs_of_file_vertices


  let replace_succs =
    List.filter ~f:(fun succ ->
        String.equal (succ |> Vertex.get_method |> Method.get_class_name) "File" )
    @@ G.get_succs ns_view
         ("String String.replace(CharSequence,CharSequence)", "{ line 37 }")
         ~label:NodeWiseSimilarity


  let _ = End
end

module Notebook111 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let axiom_applied = Axioms.apply_axioms renderer_graph

  let _ = Visualizer.visualize_and_open renderer_graph

  let _ = Visualizer.visualize_and_open renderer_finished

  let _ = Visualizer.visualize_and_open axiom_applied

  let ns_view = G.leave_only_ns_edges renderer_finished

  let getMappings =
    [ "ResourceSupport IndexController.index()"
    ; "Resources GuidesController.listGuides()"
    ; "ResponseEntity GuidesController.renderGuide(String,String)"
    ; "ResponseEntity GuidesController.showGuide(String,String)"
    ; "ResponseEntity MarkupController.renderMarkup(MediaType,String)" ]


  let getMapping_vertices = getMappings >>= G.this_method_vertices ns_view

  let _ =
    let ns_succs_of_getMapping_vertices =
      getMapping_vertices
      >>= fun vertex -> G.get_succs ns_view (LV.of_vertex vertex) ~label:NodeWiseSimilarity
    in
    List.filter
      ~f:(fun succ -> not @@ List.mem getMappings (Vertex.get_method succ) ~equal:Method.equal)
      ns_succs_of_getMapping_vertices
    |> List.map ~f:Vertex.get_method |> List.stable_dedup


  let _ = Visualizer.visualize_and_open ns_view

  (* GuideContentResource GuideRenderer.render(GuideType,String) is the problem. *)

  (* This can be mitigated by making a separate compute_nodewise_similarity for udfs. *)

  let unmarked_udfs_of_graph = G.get_unmarked_udfs

  let _ = End
end

module Notebook112 = struct
  let _ = Start

  let _ = Visualizer.visualize_and_open renderer_graph

  let file_vertices =
    List.filter
      ~f:(fun vertex -> String.equal "File" (vertex |> Vertex.get_method |> Method.get_class_name))
      (G.all_vertices_of_graph renderer_graph)
    |> Array.of_list


  let getAbsolutePath = ("String File.getAbsolutePath()", "{ line 30 }")

  let getName = ("String File.getName()", "{ line 43 }")

  (* let _ = *)
  (*   Trunk.find_position_of_vertex renderer_graph (G.LiteralVertex.to_vertex_cheap getAbsolutePath) *)

  let longest_trunk_containing_vertex =
    Array.filter (identify_longest_trunks renderer_graph) ~f:(fun trunk ->
        Array.mem trunk (G.LiteralVertex.to_vertex_cheap getName) ~equal:Vertex.equal )


  (* let _ = Trunk.find_position_of_vertex renderer_graph (G.LiteralVertex.to_vertex_cheap getName) *)

  let contribute =
    ("void ImagesGuideContentContributor.contribute(GuideContentResource,File)", "{ line 29 }")


  let contribute_trunks =
    PathUtils.enumerate_df_paths_from_source_to_leaves renderer_graph contribute


  let contribute_trunks_ending_with_itself =
    Array.filter
      ~f:(fun trunk ->
        Array.mem trunk (G.LiteralVertex.to_vertex_cheap contribute) ~equal:Vertex.equal )
      contribute_trunks


  let _ =
    (Array.sorted_copy contribute_trunks_ending_with_itself ~compare:(fun t1 t2 ->
         -Int.compare (Array.length t1) (Array.length t2) ) ).(0)


  (* DONE: "내가 속한 것 중에 제일 긴 거" *)

  let find_my_roots graph vertex =
    let ancestors = get_recursive_preds graph vertex ~label:DataFlow >>| fst in
    List.filter ~f:(fun vertex -> G.is_df_root graph (G.LiteralVertex.of_vertex vertex)) ancestors


  let getName = ("String File.getName()", "{ line 43 }")

  let longest_trunks_where_i_belong graph vertex =
    let my_roots = Array.of_list @@ find_my_roots graph vertex in
    let all_trunks =
      Array.fold
        ~f:(fun acc root ->
          Array.append acc
            (Trunk.PathUtils.enumerate_df_paths_from_source_to_leaves graph
               (G.LiteralVertex.of_vertex root) ) )
        ~init:[||] my_roots
    in
    let my_trunks =
      Array.filter all_trunks ~f:(fun trunk ->
          Array.mem trunk (G.LiteralVertex.to_vertex_cheap vertex) ~equal:Vertex.equal )
    in
    Array.sort my_trunks ~compare:(fun t1 t2 -> -Int.compare (Array.length t1) (Array.length t2)) ;
    my_trunks.(0)


  (* 개발 완료!!! *)

  let _ = longest_trunks_where_i_belong renderer_graph getName

  let _ = Array.of_list @@ find_my_roots renderer_graph getName

  let _ = End
end

module Notebook113 = struct
  let _ = Start

  (* ABORTED: debugging current implementation of find_position_of_vertex *)

  let add = ("void ResourceSupport.add(Link)", "{ line 90 }")

  let _ = Visualizer.visualize_and_open renderer_graph

  let graph = renderer_graph

  let _ = build_graph renderer_graph

  let vertex = add

  let _ = Visualizer.visualize_and_open graph

  let _ =
    let my_roots = Array.of_list @@ find_my_roots graph vertex in
    let all_trunks =
      Array.fold
        ~f:(fun acc root ->
          Array.append acc
            (PathUtils.enumerate_df_paths_from_source_to_leaves graph (LV.of_vertex root)) )
        ~init:[||] my_roots
    in
    let my_trunks =
      Array.filter all_trunks ~f:(fun trunk ->
          Array.mem trunk (LV.to_vertex_cheap vertex) ~equal:Vertex.equal )
    in
    let out =
      try
        Array.sort my_trunks ~compare:(fun t1 t2 ->
            -Int.compare (Array.length t1) (Array.length t2) ) ;
        my_trunks.(0)
      with _ -> failwith (LV.to_string vertex)
    in
    out


  (* let _ = find_position_of_vertex graph (LV.to_vertex_cheap vertex) *)

  let (graph : G.t) =
    Deserializer.deserialize_graph "2022-3-18_21:55:40_sagan-renderer_ahahahhah.bin"


  let withRel = ("Link LinkBuilderSupport.withRel(String)", "{ line 90 }")

  let vertex = withRel

  let linkTo = ("ControllerLinkBuilder ControllerLinkBuilder.linkTo(Object)", "{ line 90 }")

  let vertex = linkTo

  let _ = End
end

module Notebook114 = struct
  let _ = Start

  (* DONE: alternative implementation of find_position_of_vertex *)

  let get_recursive_preds (g : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) :
      (G.V.t * int) list =
    let rec inner (current_vertex : G.V.t) (big_acc : (G.V.t * int) list) (count : int) =
      let current_vertex_df_preds =
        G.get_preds g (G.LiteralVertex.of_vertex current_vertex) ~label
      in
      let to_explore =
        List.filter current_vertex_df_preds ~f:(fun pred ->
            not @@ List.mem (big_acc >>| fst) pred ~equal:Vertex.equal )
      in
      if List.is_empty current_vertex_df_preds || List.is_empty to_explore then big_acc
      else
        List.fold
          ~f:(fun smol_acc vertex -> inner vertex ((vertex, count) :: smol_acc) (count + 1))
          ~init:big_acc to_explore
    in
    inner (G.LiteralVertex.to_vertex vertex g.graph) [] 1


  let linkTo = ("ControllerLinkBuilder ControllerLinkBuilder.linkTo(Object)", "{ line 90 }")

  let _ = get_recursive_preds renderer_graph linkTo ~label:DataFlow

  let find_position_of_vertex (graph : G.t) (vertex : G.LiteralVertex.t) : Trunk.VertexPosition.t =
    let recursive_preds_and_distances =
      Array.of_list @@ get_recursive_preds graph vertex ~label:DataFlow
    and recursive_succs_and_distances =
      Array.of_list @@ get_recursive_succs graph vertex ~label:DataFlow
    in
    Array.sort recursive_preds_and_distances ~compare:(fun (pred1, distance1) (pred2, distance2) ->
        -Int.compare distance1 distance2 ) ;
    Array.sort recursive_succs_and_distances ~compare:(fun (pred1, distance1) (pred2, distance2) ->
        -Int.compare distance1 distance2 ) ;
    let farthest_pred_distance = snd recursive_preds_and_distances.(0)
    and farthest_succ_distance = snd recursive_succs_and_distances.(0) in
    match Int.compare farthest_pred_distance farthest_succ_distance with
    | -1 ->
        Close_to_Root
    | 0 ->
        Right_at_Middle
    | 1 ->
        Close_to_Leaf
    | _ ->
        failwith "WTF"


  let _ = End
end

module Notebook115 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let ns_graph = G.leave_only_ns_edges renderer_finished

  let subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs_with_edges ns_graph

  let subgraphs_api_only =
    Array.filter subgraphs ~f:(fun subgraph ->
        List.for_all (G.all_methods_of_graph subgraph) ~f:Method.is_api )


  let _ = Array.length subgraphs_api_only (* 19 *)

  let subgraphs_integrated_apis_only =
    G.empty
    |> fun g ->
    Array.fold
      ~f:(fun big_acc subgraph ->
        G.fold_vertex
          (fun vertex smol_acc ->
            if Method.is_api (Vertex.get_method vertex) then G.add_vertex smol_acc vertex
            else smol_acc )
          subgraph big_acc )
      ~init:g subgraphs
    |> fun g ->
    Array.fold
      ~f:(fun big_acc subgraph ->
        G.fold_edges_e
          (fun edge smol_acc ->
            if
              Method.is_api (Vertex.get_method (fst3 edge))
              && Method.is_api (Vertex.get_method (trd3 edge))
            then G.add_edge_e smol_acc edge
            else smol_acc )
          subgraph big_acc )
      ~init:g subgraphs


  let _ = Visualizer.visualize_and_open subgraphs_integrated_apis_only

  let subgraphs_integrated =
    G.empty
    |> fun g ->
    Array.fold
      ~f:(fun big_acc subgraph ->
        G.fold_vertex (fun vertex smol_acc -> G.add_vertex smol_acc vertex) subgraph big_acc )
      ~init:g subgraphs
    |> fun g ->
    Array.fold
      ~f:(fun big_acc subgraph ->
        G.fold_edges_e (fun edge smol_acc -> G.add_edge_e smol_acc edge) subgraph big_acc )
      ~init:g subgraphs


  let _ = Visualizer.visualize_and_open subgraphs_integrated

  let _ = End
end

module Notebook116 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let cs_graph = G.leave_only_cs_edges renderer_finished

  let _ = Visualizer.visualize_and_open cs_graph

  let _ = End
end

module Notebook117 = struct
  let _ = Start

  let 

  let _ = End
end
