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
open SpawnPython
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

module Notebook67 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let all_udfs = df_edges_added |> G.all_methods_of_graph |> List.filter ~f:Method.is_udf

  let all_apis =
    df_edges_added |> G.all_methods_of_graph |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_dunder)


  let _ = List.length all_udfs

  let _ = List.length all_apis

  let all_methods = G.all_methods_of_graph df_edges_added

  let _ = List.length all_methods

  let splitted = split_graph_by_comp_unit df_edges_added

  let renderer_graph = List.nth_exn splitted 0

  let renderer_udfs = renderer_graph |> G.all_methods_of_graph |> List.filter ~f:Method.is_udf

  let renderer_apis =
    renderer_graph |> G.all_methods_of_graph |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_dunder)


  let site_graph = List.nth_exn splitted 1

  let site_udfs = site_graph |> G.all_methods_of_graph |> List.filter ~f:Method.is_udf

  let site_apis =
    site_graph |> G.all_methods_of_graph |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_dunder)


  let _ = End
end

module Notebook68 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let renderer_graph = List.nth_exn splitted 0

  let site_graph = List.nth_exn splitted 1

  let renderer_trunks = identify_longest_trunks renderer_graph

  let site_trunks = identify_longest_trunks site_graph

  let _ = Trunk.Serializer.serialize_graph_trunks_to_json renderer_graph

  let _ = Trunk.Serializer.serialize_graph_trunks_to_json site_graph

  let _ = End
end

module Notebook69 = struct
  let _ = spawn_python ~pyfile:"./python/compute_nodewise_similarity.py" ~args:[]

  let csv_filename = "NodeWiseFeatures_sagan-renderer_udfs.csv_filtered.csv"

  let ns_table_from_csv csv_filename =
    let acc = Hashtbl.create 777 in
    let in_chan = In_channel.create csv_filename in
    let csv_array = Csv.to_array @@ Csv.load_in in_chan in
    In_channel.close in_chan ;
    for i = 1 to Array.length csv_array - 1 do
      let method1 = csv_array.(i).(1)
      and method2 = csv_array.(i).(12)
      and ns_score = int_of_string @@ csv_array.(i).(23) in
      Hashtbl.add acc (method1, method2) ns_score
    done ;
    acc


  let read =
    let in_chan = In_channel.create csv_filename in
    let out = Csv.to_array @@ Csv.load_in in_chan in
    In_channel.close in_chan ;
    out


  let ns_table = ns_table_from_csv "NodeWiseFeatures_sagan-renderer_udfs.csv_filtered.csv"

  let _ = End
end

module Notebook70 = struct
  exception End

  let csv_filename =
    "/Users/jslee/Dropbox/InferenceEngine/lib/sagan-renderer_all_longest_trunks.json_filtered.csv"


  let cs_table_from_csv csv_filename =
    let acc = Hashtbl.create 777 in
    let in_chan = In_channel.create csv_filename in
    List.iter ~f:(fun array -> raise TODO) (Csv.load_in in_chan) ;
    In_channel.close in_chan ;
    acc


  let read =
    let in_chan = In_channel.create csv_filename in
    let out = Csv.to_array @@ Csv.load_in in_chan in
    In_channel.close in_chan ;
    out


  let _ = End
end

module Notebook71 = struct
  let _ =
    DirectoryManager.get_compilation_unit_subdirs
      "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access"


  (* this is not the problem *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let _ = G.all_methods_of_graph df_edges_added

  let _ = split_graph_by_comp_unit df_edges_added

  let _ = get_comp_unit "int[] JdbcTemplate.batchUpdate(String,List)"

  let _ = Trunk.identify_longest_trunks df_edges_added

  let _ = Trunk.Serializer.all_longest_trunks_to_json df_edges_added

  let _ = Trunk.Serializer.serialize_graph_trunks_to_json df_edges_added

  let _ = End
end

module Notebook72 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let _ = G.all_methods_of_graph df_edges_added

  let graph = List.hd_exn @@ split_graph_by_comp_unit df_edges_added

  let unmarked_apis = G.get_unmarked_apis graph

  let unmarked_udfs = G.get_unmarked_udfs graph

  open NodeWiseFeatures.NodeWiseFeatureMap

  let api_map, udf_map = init_for_graph graph

  let _ =
    CSVSerializer.serialize api_map
      ~filename:(F.asprintf "NodeWiseFeatures_%s_apis.csv" graph.comp_unit)


  let _ =
    CSVSerializer.serialize udf_map
      ~filename:(F.asprintf "NodeWiseFeatures_%s_udfs.csv" graph.comp_unit)


  let _ = G.get_unmarked_udfs graph

  let _ = End
end

module Notebook73 = struct
  (* debugging Propagation *)

  (* first and foremost, we initialize the graph *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let relational =
    {df_edges_added with comp_unit= "src"}
    |> SimilarityHandler.make_nodewise_sim_edge |> SimilarityHandler.make_contextual_sim_edge


  let _ = Visualizer.visualize_snapshot relational ~micro:false ~autoopen:true

  let _ = Loop.loop relational [] NodeWiseFeatureMap.empty

  let _ = End
end

module Notebook74 = struct
  (* debugging SimilarityHandler.make_nodewise_sim_edge *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let df_edges_added = List.hd_exn splitted

  let ns_edges_added = SimilarityHandler.make_nodewise_sim_edge df_edges_added

  let _ = Visualizer.visualize_snapshot ns_edges_added ~autoopen:true ~micro:false

  (* ============ debugging ============ *)

  let graph = ns_edges_added

  let udf_csv_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.csv" graph.comp_unit

  and api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit

  let udf_in_chan = In_channel.create udf_csv_filename

  and api_in_chan = In_channel.create api_csv_filename

  let csv_array =
    let udf_array = Array.slice (Csv.to_array @@ Csv.load_in udf_in_chan) 1 0
    and api_array = Array.slice (Csv.to_array @@ Csv.load_in api_in_chan) 1 0 in
    Array.append udf_array api_array


  let _ =
    In_channel.close udf_in_chan ;
    In_channel.close api_in_chan


  let acc = ref graph

  let _ =
    for i = 0 to Array.length csv_array - 1 do
      let method1 = csv_array.(i).(1) and method2 = csv_array.(i).(12) in
      print_endline method1 ;
      print_endline method2 ;
      let m1_vertices = G.this_method_vertices df_edges_added method1
      and m2_vertices = G.this_method_vertices df_edges_added method2 in
      List.iter
        ~f:(fun m1_vertex ->
          List.iter
            ~f:(fun m2_vertex ->
              acc := G.add_edge_e !acc (m1_vertex, EdgeLabel.NodeWiseSimilarity, m2_vertex) )
            m2_vertices )
        m1_vertices
    done


  let _ = !acc

  let _ = Visualizer.visualize_snapshot !acc ~autoopen:true ~micro:false

  let _ = End
end

module Notebook75 = struct
  (* debugging SimilarityHandler.make_contextual_sim_edge *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let df_edges_added = List.hd_exn splitted

  let ns_edges_added = SimilarityHandler.make_nodewise_sim_edge df_edges_added

  (* ============ debugging ============ *)

  let graph = ns_edges_added

  let csv_filename = F.asprintf "%s_all_longest_trunks.json_filtered.csv" graph.comp_unit

  let in_chan = In_channel.create csv_filename

  let csv_array = Csv.to_array @@ Csv.load_in in_chan

  let _ = In_channel.close in_chan

  let acc = ref graph

  let _ =
    for i = 1 to Array.length csv_array - 1 do
      let vertex1 = csv_array.(i).(1) and vertex2 = csv_array.(i).(2) in
      let vertex1 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex1) !acc.graph
      and vertex2 = G.LiteralVertex.to_vertex (G.LiteralVertex.of_string vertex2) !acc.graph in
      acc := G.add_edge_e !acc (vertex1, EdgeLabel.ContextualSimilarity, vertex2)
    done


  let _ = !acc

  (* ============ debugging end ============ *)

  let _ = Visualizer.visualize_snapshot !acc ~autoopen:true ~micro:false

  let _ = End
end

module Notebook76 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let axiom_applied = Axioms.apply_axioms renderer_graph

  let _ = Trunk.Serializer.serialize_graph_trunks_to_json axiom_applied

  let _ = End
end

module Notebook77 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  (* ============ renderer ============ *)
  let renderer_graph = List.nth_exn splitted 0

  let renderer_finished = Main.build_graph renderer_graph

  let ns_clusters = all_ns_clusters renderer_finished

  let _ = End
end

module Notebook78 = struct
  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let renderer_graph = List.nth_exn splitted 0

  let renderer_finished = SimilarityHandler.make_nodewise_sim_edge renderer_graph

  let graph = renderer_graph

  let udf_csv_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.csv" graph.comp_unit

  and api_csv_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.csv" graph.comp_unit

  let udf_in_chan = In_channel.create udf_csv_filename

  and api_in_chan = In_channel.create api_csv_filename

  let csv_array =
    let udf_array = Array.slice (Csv.to_array @@ Csv.load_in udf_in_chan) 1 0
    and api_array = Array.slice (Csv.to_array @@ Csv.load_in api_in_chan) 1 0 in
    Array.append udf_array api_array


  let _ = In_channel.close udf_in_chan

  let _ = In_channel.close api_in_chan

  let _ = Out_channel.print_string "Now adding NS edges..."

  let _ = Out_channel.flush stdout

  let acc = ref graph

  let _ =
    for i = 0 to Array.length csv_array - 1 do
      (* let method1 = csv_array.(i).(1) and method2 = csv_array.(i).(12) in *)
      let method1 = csv_array.(i).(1) and method2 = csv_array.(i).(2) in
      let m1_vertices = G.this_method_vertices graph method1
      and m2_vertices = G.this_method_vertices graph method2 in
      List.iter
        ~f:(fun m1_vertex ->
          List.iter
            ~f:(fun m2_vertex ->
              acc := G.add_edge_e !acc (m1_vertex, EdgeLabel.NodeWiseSimilarity, m2_vertex) )
            m2_vertices )
        m1_vertices
    done


  let _ = Out_channel.print_endline "done"

  let _ = !acc

  let _ = End
end

module Notebook79 = struct
  (* profiling Loop.loop *)

  let df_edges_added =
    match graph_already_serialized ~comp_unit:"" ~suffix:"df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename


  let splitted = split_graph_by_comp_unit df_edges_added

  let renderer_graph = List.nth_exn splitted 0

  let renderer_finished = Main.build_graph renderer_graph

  let current_snapshot = renderer_finished

  let nodewise_featuremap = NodeWiseFeatureMap.empty

  let received_responses = []

  let question_maker =
    MetaRules.ForAsking.asking_rules_selector current_snapshot received_responses
      nodewise_featuremap


  open MetaRules.ForAsking

  let graph = current_snapshot

  let responses = received_responses

  let nfeaturemap = nodewise_featuremap

  let priority_assigned =
    assign_priority_on_asking_rules
      (take_subset_of_applicable_asking_rules graph responses nfeaturemap AskingRules.all_rules)
      graph


  let _ = take_subset_of_applicable_asking_rules graph responses nfeaturemap AskingRules.all_rules

  open AskingRules

  let all_rules : t array =
    [| {label= "ask_if_leaf_is_sink"; rule= ask_if_leaf_is_sink}
     ; {label= "ask_if_root_is_source"; rule= ask_if_root_is_source}
     ; {label= "ask_foreign_package_label"; rule= ask_foreign_package_label}
     ; {label= "ask_indeterminate"; rule= ask_indeterminate}
     ; { label= "ask_from_ns_cluster_if_it_contains_internal_src_or_sink"
       ; rule= ask_from_ns_cluster_if_it_contains_internal_src_or_sink } |]


  let snapshot = current_snapshot

  let _ = ask_if_leaf_is_sink snapshot received_responses nfeaturemap

  let _ = End
end

(* all_ns_clusters was the culprit!!!
   also, it's being used at lots of places in
   RulesOfInference.ml. *)

module Notebook80 = struct
  exception End

  let graph_comp_unit = "sagan-renderer"

  type json = Yojson.Basic.t

  module Json = Yojson.Basic

  (* ============ *)

  let udf_json_filename = F.asprintf "NodeWiseFeatures_%s_udfs.csv_filtered.json" graph_comp_unit

  and api_json_filename = F.asprintf "NodeWiseFeatures_%s_apis.csv_filtered.json" graph_comp_unit

  let parse_mst_json filename =
    let udf_json =
      let in_chan = In_channel.create udf_json_filename in
      let json = Json.from_channel in_chan in
      In_channel.close in_chan ;
      json
    in
    Util.to_list udf_json
    >>| fun lst -> Util.to_list lst >>| fun l -> Util.to_list l |> Util.filter_string


  let udf_parsed = parse_mst_json udf_json_filename

  let api_parsed = parse_mst_json api_json_filename

  let _ = udf_parsed >>| fun tuplist -> tuplist >>= ident |> List.stable_dedup

  (* ============ *)

  let _ = End
end
