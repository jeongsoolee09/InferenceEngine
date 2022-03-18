open GraphRepr
open InfixOperators
open ListMonad
open Yojson.Basic
open EdgeLabel

type t = G.V.t array [@@deriving compare, equal]

module Hashtbl = Caml.Hashtbl
module Cache = G.MemoMap
module LV = G.LiteralVertex

module PathUtils = struct
  let is_reachable (source : LV.t) (dest : LV.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let module DFS = Graph.Traverse.Dfs (G) in
    let source_vertex = LV.to_vertex source graph.graph
    and dest_vertex = LV.to_vertex dest graph.graph in
    let descendants = DFS.fold_component List.cons [] graph source_vertex in
    List.mem ~equal:G.V.equal descendants dest_vertex


  (** For every leaf, print paths to the leaf from the given source, where the given graph may
      contain a cycle, using a customized DFS algorithm **)
  let enumerate_df_paths_from_source_to_leaves (g : G.t) (source : LV.t) : G.V.t array array =
    let havebeenmap =
      let out = Hashtbl.create 777 in
      G.iter_edges_e_label (fun (v1, _, v2) -> Hashtbl.add out (v1, v2) 0) g ~label:DataFlow ;
      out
    in
    let rec inner (current : LV.t) (smol_acc : Vertex.t list) (big_acc : Vertex.t list list) :
        G.V.t list list =
      if G.is_df_leaf g current then List.rev smol_acc :: big_acc
      else
        let children = G.get_succs g current ~label:DataFlow in
        List.fold
          ~f:(fun acc child ->
            let havebeen_num = Hashtbl.find havebeenmap (LV.to_vertex current g.graph, child) in
            if havebeen_num >= 1 then acc
            else (
              Hashtbl.replace havebeenmap (LV.to_vertex current g.graph, child) (havebeen_num + 1) ;
              inner (LV.of_vertex child) (child :: smol_acc) acc ) )
          ~init:big_acc children
    in
    Array.of_list @@ List.map ~f:Array.of_list @@ inner source [LV.to_vertex_cheap source] []


  (* Array version of the above *)
  let find_paths_from_source_to_dest (graph : G.t) (source : LV.t) (dest : G.V.t) : t array =
    let paths_to_all_leaves = enumerate_df_paths_from_source_to_leaves graph source in
    let paths_to_dest =
      Array.filter ~f:(fun path -> Array.mem ~equal:G.V.equal path dest) paths_to_all_leaves
    in
    let take_while_to_dest_elem_index =
      Array.map
        ~f:(fun path_to_dest ->
          let dest_elem_index, _ =
            Array.findi_exn ~f:(fun _ elem -> G.V.equal elem dest) path_to_dest
          in
          Array.slice path_to_dest 0 (dest_elem_index + 1) )
        paths_to_dest
    in
    take_while_to_dest_elem_index
end

let to_string (trunk : t) : string =
  let acc = Array.fold ~f:(fun acc vertex -> acc ^ Vertex.to_string vertex ^ ", ") ~init:"" trunk in
  "[" ^ acc ^ "]"


let find_longest_path (paths : t array) : t =
  Option.value_exn
    (Array.max_elt ~compare:(fun p1 p2 -> Int.compare (Array.length p1) (Array.length p2)) paths)


let identify_longest_trunks : G.t -> t array =
  let cache = ref Cache.empty in
  fun (graph : G.t) : t array ->
    match Cache.find_opt graph.graph !cache with
    | None ->
        let out =
          let df_only_graph = G.leave_only_df_edges graph in
          let roots = Array.of_list @@ G.collect_df_roots df_only_graph
          and leaves = Array.of_list @@ G.collect_df_leaves df_only_graph in
          let carpro = Array.cartesian_product roots leaves in
          (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
          let reachable_root_and_leaf_pairs =
            Array.filter
              ~f:(fun (root, leaf) ->
                PathUtils.is_reachable (LV.of_vertex root) (LV.of_vertex leaf) df_only_graph )
              carpro
          in
          (* now, find the path between the root and the leaf. *)
          Array.map
            ~f:(fun (root, leaf) ->
              let all_paths =
                PathUtils.find_paths_from_source_to_dest df_only_graph (LV.of_vertex root) leaf
              in
              find_longest_path all_paths )
            reachable_root_and_leaf_pairs
        in
        cache := Cache.add graph.graph out !cache ;
        out
    | Some res ->
        res


type vertex_position = Close_to_Root | Close_to_Leaf | Right_at_Middle

let find_my_roots (graph : G.t) (vertex : LV.t) =
  let ancestors = List.map ~f:fst @@ get_recursive_preds graph vertex ~label:DataFlow in
  List.filter ~f:(fun vertex -> G.is_df_root graph (LV.of_vertex vertex)) ancestors


let longest_trunks_where_i_belong (graph : G.t) (vertex : LV.t) =
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
      Array.sort my_trunks ~compare:(fun t1 t2 -> -Int.compare (Array.length t1) (Array.length t2)) ;
      my_trunks.(0)
    with _ ->
      G.serialize_to_bin ~suffix:"ahahahhah" graph ;
      failwith (LV.to_string vertex)
  in
  out


let find_position_of_vertex (graph : G.t) (vertex : G.LiteralVertex.t) : vertex_position =
  let recursive_preds_and_distances =
    Array.of_list @@ get_recursive_preds graph vertex ~label:DataFlow
  and recursive_succs_and_distances =
    Array.of_list @@ get_recursive_succs graph vertex ~label:DataFlow
  in
  Array.sort recursive_preds_and_distances ~compare:(fun (_, distance1) (_, distance2) ->
      -Int.compare distance1 distance2 ) ;
  Array.sort recursive_succs_and_distances ~compare:(fun (_, distance1) (_, distance2) ->
      -Int.compare distance1 distance2 ) ;
  let farthest_pred_distance = if Array.is_empty recursive_preds_and_distances then 0 else snd recursive_preds_and_distances.(0)
  and farthest_succ_distance = if Array.is_empty recursive_succs_and_distances then 0 else snd recursive_succs_and_distances.(0) in
  match Int.compare farthest_pred_distance farthest_succ_distance with
  | -1 ->
      Close_to_Root
  | 0 ->
      Right_at_Middle
  | 1 ->
      Close_to_Leaf
  | _ ->
      failwith "WTF"


let longest_trunk_finder ~(start : LV.t) ~(end_ : LV.t) (graph : G.t) : t array =
  Array.filter
    ~f:(fun trunk ->
      Vertex.equal (LV.to_vertex start graph.graph) trunk.(0)
      && Vertex.equal (LV.to_vertex end_ graph.graph) (Array.last trunk) )
    (identify_longest_trunks graph)


let find_trunks_containing_vertex (graph : G.t) (vertex : G.V.t) =
  let all_trunks = identify_longest_trunks graph in
  Array.filter ~f:(fun trunk -> Array.mem ~equal:Vertex.equal trunk vertex) all_trunks


let make_trunk_dictionary (graph : G.t) : (string * t) array =
  let all_trunks = identify_longest_trunks graph in
  Array.mapi ~f:(fun index trunk -> (F.asprintf "id%d" index, trunk)) all_trunks


module Serializer = struct
  type json = Yojson.Basic.t

  let to_json (trunk : t) : json =
    `List
      ( trunk
      |> Array.map ~f:(fun vertex ->
             LV.of_vertex vertex |> LV.to_string |> fun string -> `String string )
      |> Array.to_list )


  let all_longest_trunks_to_json (graph : G.t) : json =
    let trunk_dict = make_trunk_dictionary graph in
    `Assoc (Array.to_list @@ Array.map ~f:(fun (id, trunk) -> (id, to_json trunk)) trunk_dict)


  let serialize_graph_trunks_to_json (graph : G.t) : unit =
    let filename = F.asprintf "%s_all_longest_trunks.json" graph.comp_unit in
    let out_chan = Out_channel.create filename in
    pretty_to_channel out_chan (all_longest_trunks_to_json graph) ;
    Out_channel.flush out_chan ;
    Out_channel.close out_chan
end
