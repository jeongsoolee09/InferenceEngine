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
open Transfer
open Utils

type delimiter = Start | End

type json = JSON.t

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

  let gettersetter_none_marked = Axioms.Distribution.getters_setters_predicates_equals_are_none

  let gettersetter_none_marked = Axioms.Distribution.getters_setters_predicates_equals_are_none

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
    Array.sort recursive_preds_and_distances ~compare:(fun (_, distance1) (_, distance2) ->
        -Int.compare distance1 distance2 ) ;
    Array.sort recursive_succs_and_distances ~compare:(fun (_, distance1) (_, distance2) ->
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

  let renderer_labelresultmap =
    InferenceResult.deserialize_label_result_map "sagan-renderer_inference_results.json"
      "sagan-renderer"


  let site_transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_graph


  (* Transfers in a flash!!! That's a good sign. *)

  (* Uh... wait. We're not using the calculation results!! *)

  let _ = End
end

module Notebook118 = struct
  let _ = Start

  (* test the transfer from renderer to site. *)

  let site_axiom_applied = Axioms.apply_axioms site_graph

  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_axiom_applied


  let indeterminates =
    List.filter (G.all_vertices_of_graph transferred) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let _ = List.length indeterminates

  let _ = Visualizer.visualize_and_open transferred

  let _ = End
end

module Notebook119 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_finished


  let indeterminates =
    List.filter (G.all_vertices_of_graph transferred) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  (* DONE developing a function that initiates NS propagation on all NS clusters. *)

  let initiate_cluster (supergraph : G.t) (cluster : G.t) : G.t =
    let there_is_no_indeterminate =
      List.for_all (G.all_vertices_of_graph cluster)
        ~f:(Vertex.get_dist >> ProbQuadruple.is_determined)
    and all_vertices_are_indeterminate =
      List.for_all (G.all_vertices_of_graph cluster)
        ~f:(Vertex.get_dist >> ProbQuadruple.is_indeterminate)
    in
    if there_is_no_indeterminate || all_vertices_are_indeterminate then supergraph
    else
      let random_elem =
        Utils.random_elem
          (List.filter (G.all_vertices_of_graph cluster)
             ~f:(Vertex.get_dist >> ProbQuadruple.is_determined) )
      in
      let this_elem_response =
        let method_ = Vertex.get_method random_elem and dist = Vertex.get_dist random_elem in
        Response.ForLabel (method_, ProbQuadruple.determine_label dist)
      in
      let ns_propagated, _ =
        Propagator.propagator this_elem_response cluster
          [| { rule= PropagationRules.nodewise_similarity_propagation_rule
             ; label= "nodewise_similarity_propagation_rule" } |]
          [] [||]
          [| { rule= PropagationRules.nodewise_similarity_propagation_rule
             ; label= "nodewise_similarity_propagation_rule" } |]
      in
      G.fold_vertex
        (fun vertex current_supergraph ->
          G.strong_update_dist vertex (Vertex.get_dist vertex) current_supergraph )
        ns_propagated supergraph


  let transferred_graph = transferred

  let initiate_NS_propagation (transferred_graph : G.t) : G.t =
    let ns_clusters = all_ns_clusters transferred_graph in
    Array.fold ns_clusters ~f:initiate_cluster ~init:transferred_graph


  let internal_ns_propagated = initiate_NS_propagation transferred

  let indeterminates =
    List.filter (G.all_vertices_of_graph internal_ns_propagated) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let _ = List.length indeterminates

  (* what are those methods? *)

  let indeterminate_methods = List.map indeterminates ~f:Vertex.get_method |> List.stable_dedup

  let _ = List.length indeterminate_methods (* 232 *)

  let ns_clusters = all_ns_clusters site_finished

  let ns_clusters_enumeration = Array.mapi ns_clusters ~f:(fun index cluster -> (cluster, index))

  let find_containing_cluster_number (clusters : (G.t * int) array) (method_ : Method.t) :
      int option =
    Array.fold clusters
      ~f:(fun acc (cluster, index) ->
        if List.mem (G.all_methods_of_graph cluster) method_ ~equal:Method.equal then Some index
        else acc )
      ~init:(None : int option)


  let indeterminate_methods_membership =
    List.map indeterminate_methods ~f:(fun method_ ->
        (method_, find_containing_cluster_number ns_clusters_enumeration method_) )


  let not_connected_methods =
    List.map ~f:fst
    @@ List.filter indeterminate_methods_membership ~f:(fun (_, membership) ->
           Option.is_none membership )


  let _ = List.length not_connected_methods

  let _ =
    (* let's debug the NS_connection: the `fetch` families should be connected!! *)
    (* but first, we sort the methods and output them. *)
    let sorted_methods = List.sort ~compare:String.compare not_connected_methods in
    Out_channel.with_file "not_connected_methods.txt" ~f:(fun out_chan ->
        Out_channel.output_lines out_chan sorted_methods )


  let _ = End
end

module Notebook120 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_finished


  let internal_ns_propagated = Transfer.initiate_NS_propagation transferred

  let indeterminates =
    List.filter (G.all_vertices_of_graph internal_ns_propagated) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let indeterminate_methods = List.map indeterminates ~f:Vertex.get_method |> List.stable_dedup

  let ns_clusters = all_ns_clusters site_finished

  let ns_clusters_enumeration = Array.mapi ns_clusters ~f:(fun index cluster -> (cluster, index))

  let find_containing_cluster_number (clusters : (G.t * int) array) (method_ : Method.t) :
      int option =
    Array.fold clusters
      ~f:(fun acc (cluster, index) ->
        if List.mem (G.all_methods_of_graph cluster) method_ ~equal:Method.equal then Some index
        else acc )
      ~init:(None : int option)


  let indeterminate_methods_membership =
    List.map indeterminate_methods ~f:(fun method_ ->
        (method_, find_containing_cluster_number ns_clusters_enumeration method_) )


  let not_connected_methods =
    List.map ~f:fst
    @@ List.filter indeterminate_methods_membership ~f:(fun (_, membership) ->
           Option.is_none membership )


  let _ = List.length not_connected_methods (* 101 *)

  let _ =
    (* let's debug the NS_connection: the `fetch` families should be connected!! *)
    (* but first, we sort the methods and output them. *)
    let sorted_methods = List.sort ~compare:String.compare not_connected_methods in
    Out_channel.with_file "not_connected_methods.txt" ~f:(fun out_chan ->
        Out_channel.output_lines out_chan sorted_methods )


  let _ = List.length indeterminates

  let _ = List.length indeterminate_methods (* 120 *)

  let _ = End
end

module Notebook121 = struct
  let _ = Start

  (* WORKING Visualizing NS clusters *)

  let site_finished = build_graph site_graph

  let ns_graph = G.leave_only_ns_edges site_finished

  let subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs_with_edges ns_graph

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

module Notebook122 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  let ns_graph = G.leave_only_ns_edges site_finished

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

  let _ = End
end

module Notebook123 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  let ns_graph = G.leave_only_ns_edges site_finished

  let _ = G.all_methods_of_graph ns_graph

  let _ = 1

  (* *** DONE API: .get()은 non이야 *)

  let is_api_with_get_and_no_intypes (method_ : Method.t) : bool =
    Method.is_api method_
    && String.is_prefix (Method.get_method_name method_) ~prefix:"get"
    && String.is_substring method_ ~substring:"()"


  let api_that_has_prefix_get_and_intype_is_void_is_none (graph : G.t) =
    let all_apis_with_get_and_no_intypes =
      List.filter (G.all_methods_of_graph graph) ~f:is_api_with_get_and_no_intypes
    in
    all_apis_with_get_and_no_intypes


  (* *** DONE API: void .set()은 non이야 *)

  let _ = api_that_has_prefix_get_and_intype_is_void_is_none ns_graph

  let is_api_with_set_prefix_set_and_void_rtntype (method_ : Method.t) : bool =
    Method.is_api method_
    && String.is_prefix (Method.get_method_name method_) ~prefix:"set"
    && String.equal (Method.get_return_type method_) "void"


  let api_that_has_prefix_set_and_rtntype_is_void_is_none (graph : G.t) =
    let all_apis_with_set_and_void_rtntypes =
      List.filter (G.all_methods_of_graph graph) ~f:is_api_with_set_prefix_set_and_void_rtntype
    in
    all_apis_with_set_and_void_rtntypes


  let _ = api_that_has_prefix_set_and_rtntype_is_void_is_none ns_graph

  (* *** DONE API: java.util 끼리 모으기 *)

  let is_java_builtin (method_ : Method.t) : bool =
    String.is_prefix (Method.get_package_name method_) ~prefix:"java."


  let all_java_builtin_methods_and_their_packages (graph : G.t) =
    let all_java_builtin_methods = List.filter (G.all_methods_of_graph graph) ~f:is_java_builtin in
    List.map ~f:(fun method_ -> (method_, Method.get_package_name method_)) all_java_builtin_methods


  let java_builtins_and_packages = all_java_builtin_methods_and_their_packages ns_graph

  (* 108!! *)

  (* now i want to partition them by package names. *)

  let partitioned_by_packages =
    let all_packages = List.stable_dedup @@ List.map ~f:snd java_builtins_and_packages in
    List.map all_packages ~f:(fun package ->
        List.filter ~f:(String.equal package << snd) java_builtins_and_packages )


  let format_for_single_package tuples =
    let stringlist =
      List.fold tuples
        ~f:(fun acc (method_, package) -> F.asprintf " ; (%s, %s)\n" method_ package :: acc)
        ~init:[]
    in
    F.asprintf "[%s]" (String.concat stringlist)


  let _ =
    let stringlist = List.map partitioned_by_packages ~f:format_for_single_package in
    Out_channel.with_file "java_builtin_methods.txt" ~f:(fun out_chan ->
        Out_channel.output_lines out_chan stringlist )


  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_finished


  let internal_ns_propagated = Transfer.initiate_NS_propagation transferred

  let indeterminates =
    List.filter (G.all_vertices_of_graph internal_ns_propagated) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let indeterminate_methods = List.map indeterminates ~f:Vertex.get_method |> List.stable_dedup
  (* 120 *)

  let indeterminate_apis = List.filter ~f:Method.is_api indeterminate_methods (* 63 *)

  let indeterminate_udfs = List.filter ~f:Method.is_udf indeterminate_methods (* 57 *)

  (* TODO How many of them are indeterminates? *)

  let interesting =
    let m1 = api_that_has_prefix_get_and_intype_is_void_is_none ns_graph
    and m2 = api_that_has_prefix_set_and_rtntype_is_void_is_none ns_graph
    and m3 = all_java_builtin_methods_and_their_packages ns_graph in
    m1 @ m2 @ (m3 >>| fst)


  let _ =
    List.length @@ List.stable_dedup @@ List.map ~f:Vertex.get_method
    @@ List.filter
         (interesting >>= G.this_method_vertices internal_ns_propagated)
         ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist)


  let ns_clusters = all_ns_clusters site_finished

  let subgraphs = WeaklyConnectedComponents.find_distinct_subgraphs_with_edges ns_graph

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


  let not_connected = In_channel.read_lines "not_connected_methods.txt"

  let not_connected_interesting =
    (* 22 *)
    List.length
    @@ List.filter interesting ~f:(fun method_ ->
           List.mem not_connected method_ ~equal:Method.equal )


  (* nice! *)

  let _ = End
end

module Notebook124 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  (* I want to know the constitution of not_connected_methods. *)

  let not_connected : Method.t list = In_channel.read_lines "not_connected_methods.txt"

  let not_connected_apis = List.filter ~f:Method.is_api not_connected (* 47 *)

  let not_connected_udfs = List.filter ~f:Method.is_udf not_connected (* 54 *)

  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_finished


  let internal_ns_propagated = Transfer.initiate_NS_propagation transferred

  let indeterminates =
    List.filter (G.all_vertices_of_graph internal_ns_propagated) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let indeterminate_methods = List.map indeterminates ~f:Vertex.get_method |> List.stable_dedup
  (* 120 *)

  let determinate_apis =
    List.stable_dedup @@ List.map ~f:Vertex.get_method
    @@ List.filter
         ~f:(ProbQuadruple.is_determined << Vertex.get_dist)
         (G.all_vertices_of_graph internal_ns_propagated)


  let determinate_api_vertices =
    List.filter
      ~f:(ProbQuadruple.is_determined << Vertex.get_dist)
      (G.all_vertices_of_graph internal_ns_propagated)


  let _ =
    let repository_methods =
      List.filter determinate_apis ~f:(fun meth ->
          String.is_substring (Method.get_class_name meth) ~substring:"Repository" )
    in
    let repository_vertices =
      repository_methods >>= G.this_method_vertices internal_ns_propagated
    in
    Out_channel.write_lines "repository_methods.txt"
      ( repository_vertices
      >>| fun vertex ->
      F.asprintf "(%s, %s)" (Vertex.to_string vertex)
        (ProbQuadruple.to_string @@ Vertex.get_dist vertex) )


  let _ =
    let repository_methods =
      List.filter determinate_apis ~f:(fun meth ->
          String.is_substring (Method.get_class_name meth) ~substring:"Repository" )
    in
    let repository_vertices = repository_methods >>= G.this_method_vertices transferred in
    Out_channel.write_lines "repository_methods.txt"
      ( repository_vertices
      >>| fun vertex ->
      F.asprintf "(%s, %s)" (Vertex.to_string vertex)
        (ProbQuadruple.to_string @@ Vertex.get_dist vertex) )


  let indeterminate_apis = List.filter ~f:Method.is_api indeterminate_methods (* 63 *)

  let indeterminate_udfs = List.filter ~f:Method.is_udf indeterminate_methods (* 57 *)

  let indeterminate_udfs_and_their_annots =
    indeterminate_udfs >>| fun udf -> (udf, Annotations.get_annots udf)


  let indeterminate_udfs_without_annots =
    (* 39 *)
    List.filter indeterminate_udfs ~f:(List.is_empty << Annotations.get_annots)


  let not_connected_udfs_without_annots =
    (* 39 *)
    List.filter not_connected_udfs ~f:(List.is_empty << Annotations.get_annots)


  (* oh, nice. 120 - 39 (indeterminate_udfs_without_annots) - 22 (interesting) = 59. *)

  let _ = End
end

module Notebook125 = struct
  let _ = Start

  let site_finished = build_graph site_graph

  let transferred =
    Transfer.transfer_from_json ~filename:"sagan-renderer_inference_results.json"
      ~prev_comp_unit:"sagan-renderer" site_finished


  let internal_ns_propagated = Transfer.initiate_NS_propagation transferred

  let ns_clusters = all_ns_clusters transferred

  let indeterminate_ns_clusters =
    (* 22 *)
    Array.filter ns_clusters ~f:(fun ns_cluster ->
        List.for_all
          (G.all_vertices_of_graph ns_cluster)
          ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )


  let indeterminates =
    List.filter (G.all_vertices_of_graph internal_ns_propagated) ~f:(fun vertex ->
        ProbQuadruple.is_indeterminate @@ Vertex.get_dist vertex )


  let indeterminate_methods = List.map indeterminates ~f:Vertex.get_method |> List.stable_dedup
  (* 120 *)

  let _ = End
end

module Notebook126 = struct
  let _ = Start

  (* DONE *)

  let is_interface_method (method_ : Method.t) : bool =
    let all_interfaces =
      InterfaceScraper.scrape_interfaces_from_directory Deserializer.project_root
    in
    List.mem all_interfaces (Method.get_class_name method_) ~equal:String.equal


  let apis = In_channel.read_lines "APIs.txt"

  let fake_apis = List.filter apis ~f:is_interface_method

  let _ = FileAmender.amend_interface_method ()

  let _ = End
end

module Notebook127 = struct
  let _ = Start

  (* TODO Testing the indeterminate udfs which slipped through the axioms. *)

  let site_finished = build_graph site_graph

  let indeterminate_methods =
    List.map (G.all_vertices_of_graph site_finished) ~f:Vertex.get_method
    |> List.stable_dedup |> List.filter ~f:Method.is_udf


  let _ = End
end

module Notebook128 = struct
  let _ = Start

  (* DONE writing a function to measure snapshot-wise accuracy of SRMs. *)

  type solution = Sagan_solution.solution

  let is_srm (method_ : Method.t) (solutions : solution array) : bool =
    let label_strs = snd @@ Array.find_exn solutions ~f:(Method.equal method_ << fst) in
    List.mem label_strs "src" ~equal:String.equal
    || List.mem label_strs "sin" ~equal:String.equal
    || List.mem label_strs "san" ~equal:String.equal


  let srm_accuracy_of_snapshot (snapshot : G.t) (solutions : solution array) : float =
    let all_srm_vertices_count =
      G.fold_vertex
        (fun vertex acc -> if is_srm (Vertex.get_method vertex) solutions then acc + 1 else acc)
        snapshot 0
    in
    let correct_srm_vertex_count =
      G.fold_vertex
        (fun vertex acc ->
          if is_srm (Vertex.get_method vertex) solutions then
            let estimation_is_correct =
              AutoTest.Scoring.vertex_inference_result_is_correct vertex
            in
            if estimation_is_correct then acc + 1 else acc
          else acc )
        snapshot 0
    in
    Float.( / ) (Float.of_int correct_srm_vertex_count) (Float.of_int all_srm_vertices_count)


  let _ = End
end

module Notebook129 = struct
  let _ = Start

  (* DONE Debugging AutoTest's SRM accuracy measurement function *)

  let _ = AutoTest.auto_test renderer_graph

  let method_ = "ResponseEntity GuidesController.renderGuide(String,String)"

  let _ = AutoTest.Scoring.is_srm method_

  let _ =
    Hashtbl.find AutoTest.solution_table "ResponseEntity GuidesController.showGuide(String,String)"


  let _ = End
end

module Notebook130 = struct
  let _ = Start

  let srm_map_of_snapshot (snapshot : G.t) : string =
    let label_result_map = InferenceResult.make_label_result_map snapshot in
    let only_srm_map =
      InferenceResult.LabelResultMap.filter
        (fun meth _ -> AutoTest.Scoring.is_srm meth)
        label_result_map
    in
    let json_repr = InferenceResult.Serializer.to_json_repr snapshot only_srm_map in
    Json.pretty_to_string json_repr


  let _ = End
end

module Notebook131 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  (* DONE why isn't bump working? *)

  (* is bump working in the first place? *)

  let initial = ProbQuadruple.initial

  let _ = DistManipulator.bump initial [Source] ~inc_delta:10. ~dec_delta:3.

  (* 아 아니구나. 잘못 봄. ㅋㅋㅋ DistManipulator.bump는 잘 작동하고 있다. *)

  let one_step : G.t =
    Deserializer.deserialize_graph "2022-3-29_12:57:26_sagan-renderer_ahahaha.bin"


  (* now let's propagate Optional Optional.of(Object) ... *)

  let optional_of = "Optional Optional.of(Object)"

  let optional_of_cluster =
    let all_ns_clusters = all_ns_clusters one_step in
    Array.find all_ns_clusters ~f:(fun cluster ->
        List.mem (G.all_methods_of_graph cluster) optional_of ~equal:Method.equal )


  let _ = Visualizer.visualize_and_open @@ G.leave_only_ns_edges renderer_finished

  let response = Response.ForLabel (optional_of, None)

  let propagated = Propagator.propagator

  let ended : G.t = Deserializer.deserialize_graph "2022-3-29_13:36:50_sagan-renderer_ahahaha.bin"

  let sb = "StringBuilder StringBuilder.append(String)"

  let _ = G.snapshot_to_json ended

  let _ = End
end

module Notebook132 = struct
  let _ = Start

  let ask_annotated_done : G.t = Deserializer.deserialize_graph "ask_annotated_finished.bin"

  let _ = all_ns_clusters_api_only ask_annotated_done

  let sb = "StringBuilder StringBuilder.append(String)"

  let sb_cluster =
    let all_ns_clusters = all_ns_clusters ask_annotated_done in
    Array.find_exn all_ns_clusters ~f:(fun cluster ->
        List.mem (G.all_methods_of_graph cluster) sb ~equal:Method.equal )


  let _ = Visualizer.visualize_and_open sb_cluster

  let response = Response.ForLabel (sb, None)

  let propagated, _ = AutoTest.auto_test_spechunter_for_snapshot_once sb_cluster [response]

  let _ = Visualizer.visualize_and_open propagated

  (* okay. no change. let's inline auto_test..once here and do it line-by-line. *)

  (* observation 1 : if the dist is saturated, then no job is done (termination condition). *)

  let _ = raise TODO

  let current_snapshot = ask_annotated_done

  and received_responses = []

  open InfixOperators
  open ListMonad
  open GraphRepr
  open TaintLabel
  open Propagator
  open Utils
  open AutoTest

  let graph, responses =
    (* find the most appropriate Asking Rule. *)
    if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
      (current_snapshot, received_responses)
    else
      let question_maker =
        MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
      in
      let question = question_maker.rule current_snapshot received_responses ~dry_run:false in
      print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
      let response = AutoTest.responder question in
      (* sort applicable Propagation Rules by adequacy. *)
      let propagation_rules_to_apply =
        MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
      in
      let propagated =
        fst
        @@ propagator response current_snapshot propagation_rules_to_apply received_responses [||]
             PropagationRules.all_rules
      in
      let propagated' = Axioms.apply_axioms propagated in
      let stats =
        let correct_vertices_count =
          G.fold_vertex
            (fun vertex acc ->
              if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
            propagated' 0
        in
        F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
          correct_vertices_count (G.nb_vertex propagated')
          (Scoring.get_vertexwise_precision_of_snapshot propagated')
          ( List.length
          @@ List.filter
               (G.all_vertices_of_graph propagated')
               ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
          (G.nb_vertex propagated')
      in
      print_endline stats ;
      let srm_stats =
        let correct_srms_count, all_srms_count, accuracy =
          Scoring.srm_report_of_snapshot propagated'
        in
        F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
      in
      print_endline srm_stats ;
      print_endline @@ srm_map_of_snapshot propagated' ;
      (propagated', response :: received_responses)


  let _ = Visualizer.visualize_and_open graph

  let current_snapshot = propagated

  and received_responses = responses

  let graph', responses' =
    (* find the most appropriate Asking Rule. *)
    if G.Saturation.all_dists_in_graph_are_saturated current_snapshot then
      (current_snapshot, received_responses)
    else
      (* let question_maker = *)
      (*   MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses *)
      (* in *)
      let question =
        (* question_maker.rule current_snapshot received_responses ~dry_run:false *)
        Question.AskingForLabel "boolean Optional.isPresent()"
      in
      print_endline @@ F.asprintf "Question: %s" (Question.to_string question) ;
      let response = AutoTest.responder question in
      (* sort applicable Propagation Rules by adequacy. *)
      let propagation_rules_to_apply =
        MetaRules.ForPropagation.sort_propagation_rules_by_priority current_snapshot response
      in
      let propagated =
        fst
        @@ propagator response current_snapshot propagation_rules_to_apply received_responses [||]
             PropagationRules.all_rules
      in
      let propagated' = Axioms.apply_axioms propagated in
      let stats =
        let correct_vertices_count =
          G.fold_vertex
            (fun vertex acc ->
              if Scoring.vertex_inference_result_is_correct vertex then acc + 1 else acc )
            propagated' 0
        in
        F.asprintf "overall: [%d / %d] (%f) <vertex>, [%d / %d] <indeterminate>"
          correct_vertices_count (G.nb_vertex propagated')
          (Scoring.get_vertexwise_precision_of_snapshot propagated')
          ( List.length
          @@ List.filter
               (G.all_vertices_of_graph propagated')
               ~f:(ProbQuadruple.is_indeterminate << Vertex.get_dist) )
          (G.nb_vertex propagated')
      in
      print_endline stats ;
      let srm_stats =
        let correct_srms_count, all_srms_count, accuracy =
          Scoring.srm_report_of_snapshot propagated'
        in
        F.asprintf "srm: [%d / %d] (%f) <vertex>" correct_srms_count all_srms_count accuracy
      in
      print_endline srm_stats ;
      print_endline @@ srm_map_of_snapshot propagated' ;
      (propagated', response :: received_responses)


  let _ = Visualizer.visualize_and_open graph'

  let _ = End
end

module Notebook133 = struct
  let _ = Start

  open SingleFeature

  let getforobject = "Object RestTemplate.getForObject(String,Class,Object[])"

  let getforentity = "ResponseEntity RestTemplate.getForEntity(String,Class,Object[])"

  let mkdir = "boolean File.mkdir()"

  let deleteonexit = "void File.deleteOnExit()"

  let isfile = "boolean File.isFile()"

  let write = "void FileOutputStream.write(byte[])"

  let _ = SingleFeature.returnval_not_used_in_caller getforobject

  let _ = SingleFeature.returnval_not_used_in_caller getforentity

  let _ = SingleFeature.returnval_not_used_in_caller mkdir

  let _ = SingleFeature.returnval_not_used_in_caller deleteonexit

  let _ = SingleFeature.returnval_not_used_in_caller isfile

  let _ = SingleFeature.returnval_not_used_in_caller write

  (* ======================================= *)

  let getforobject_vertices = G.this_method_vertices renderer_graph getforobject

  let _ =
    List.map getforobject_vertices ~f:(LV.of_vertex >> Trunk.vertex_is_close_to_root renderer_graph)


  let getforentity_vertices = G.this_method_vertices renderer_graph getforentity

  let _ =
    List.map getforentity_vertices ~f:(LV.of_vertex >> Trunk.vertex_is_close_to_root renderer_graph)


  let mkdir_vertices = G.this_method_vertices renderer_graph mkdir

  let _ = List.map mkdir_vertices ~f:(LV.of_vertex >> Trunk.vertex_is_close_to_root renderer_graph)

  let deleteonexit_vertices = G.this_method_vertices renderer_graph getforentity

  let _ =
    List.map deleteonexit_vertices ~f:(LV.of_vertex >> Trunk.vertex_is_close_to_root renderer_graph)


  let isfile_vertices = G.this_method_vertices renderer_graph isfile

  let _ = List.map isfile_vertices ~f:(LV.of_vertex >> Trunk.vertex_is_close_to_root renderer_graph)

  (* DONE the relative positions were the culprit!! *)

  let _ = End
end

module Notebook134 = struct
  let _ = Start

  (* TODO testing returnval_not_used_in_caller *)

  let mkdir = "boolean File.mkdir()"

  let deleterecursively = "boolean FileSystemUtils.deleteRecursively(File)"

  let _ = SingleFeature.returnval_not_used_in_caller mkdir

  let _ = SingleFeature.returnval_not_used_in_caller deleterecursively

  (* mkdir and deleterecursively is not used anywhere! *)

  (* --> can be detected with a simple trick. *)

  let rec get_next_elem (lst : 'a list) ~(next_to : 'a) ~(equal : 'a -> 'a -> bool) =
    match lst with
    | [] | [_] ->
        failwith "get_next_elem failed"
    | x1 :: (x2 :: _ as rest) ->
        if equal x1 next_to then x2 else get_next_elem rest ~next_to ~equal


  let sample_json =
    "{\n\
    \    \"defining_method\":\n\
    \      \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\",\n\
    \    \"access_path\": \"(image, [])\",\n\
    \    \"location\": \"{ line 38 }\",\n\
    \    \"chain\": [\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"(image, [])\",\n\
    \        \"location\": \"{ line 38 }\",\n\
    \        \"using\":\n\
    \          \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\",\n\
    \        \"status\": \"Call\",\n\
    \        \"callee\": \"boolean File.isFile()\",\n\
    \        \"location\": \"{ line 39 }\",\n\
    \        \"with\": \"(param_isFile_39_0, [])\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\",\n\
    \        \"status\": \"Define\",\n\
    \        \"access_path\": \"($irvar7, [])\",\n\
    \        \"location\": \"{ line 39 }\",\n\
    \        \"using\": \"boolean File.isFile()\"\n\
    \      },\n\
    \      {\n\
    \        \"current_method\":\n\
    \          \"void ImagesGuideContentContributor.contribute(GuideContentResource,File)\",\n\
    \        \"status\": \"Dead\"\n\
    \      }\n\
    \    ]\n\
    \  }"


  let chain = chain_slice_list_of_wrapped_chain (JSON.from_string sample_json)

  let chain_slice =
    ChainSlice.CallSlice
      ( "void ImagesGuideContentContributor.contribute(GuideContentResource,File)"
      , "boolean File.isFile()"
      , "{ line 39 }"
      , "(param_isFile_39_0, [])" )


  let _ = ReturnValUsedInCaller.returnval_not_used_really chain_slice

  let _ = End
end
