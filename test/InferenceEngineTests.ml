open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open SimilarityHandler
open Loop
open RulesOfInference
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

module Notebook99 = struct
  let _ = Start

  let renderer_finished = build_graph renderer_graph

  let _ = Loop.loop renderer_finished NodeWiseFeatureMap.empty ~auto_test:true

  let round1, round1_responses =
    AutoTest.auto_test_spechunter_for_snapshot_once renderer_finished []


  let round2, round2_responses =
    AutoTest.auto_test_spechunter_for_snapshot_once round1 round1_responses


  let _ = AutoTest.get_vertexwise_precision_of_snapshot round2

  let _ = Visualizer.visualize_snapshot round2 ~autoopen:true ~micro:false

  let snapshot = round1

  let received_responses = round1_responses

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

  (* TODO Debugging GetterSetter.ml *)

  let _ = GetterSetter.is_setter "void AtomFeedView.setRenderedContent(Post,Entry)"

  let gettersetter_none_marked =
    Axioms.Distribution.getters_setters_and_predicates_are_none site_graph


  let _ = Visualizer.visualize_snapshot gettersetter_none_marked ~autoopen:true ~micro:false

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


  let _ = Visualizer.visualize_snapshot test_graph ~autoopen:true ~micro:false

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

  let _ = Visualizer.visualize_snapshot subgraph1 ~autoopen:true ~micro:false

  let _ = Visualizer.visualize_snapshot subgraph2 ~autoopen:true ~micro:false

  (* working nicely! *)

  let _ = End
end
