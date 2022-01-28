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
  let make_nodewise_sim_edge (graph : G.t) : G.t =
    Out_channel.print_string "spawning python process compute_nodewise_similarity.py..." ;
    SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py" ~args:[] ;
    Out_channel.print_endline "done" ;
    let regex = Re2.create_exn "NodeWiseFeatures_.*\.csv" in
    let sim_csvs =
      Array.filter ~f:(fun directory -> Re2.matches regex directory) @@ Sys.readdir "."
    in
    Array.fold ~f:(fun acc csvfile ->
        raise TODO
      ) ~init:graph sim_csvs


  let _ = End
end
