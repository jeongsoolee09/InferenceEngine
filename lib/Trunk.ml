open GraphRepr
open InfixOperators
open ListMonad
open Yojson.Basic

type t = G.V.t array [@@deriving compare, equal]

module Hashtbl = Caml.Hashtbl

module PathUtils = struct
  let is_reachable (source : G.V.t) (dest : G.V.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let module DFS = Graph.Traverse.Dfs (G) in
    let descendants = DFS.fold_component List.cons [] graph source in
    List.mem ~equal:G.V.equal descendants dest


  (** For every leaf, print paths to the leaf from the given source, where the given graph may
      contain a cycle, using a customized DFS algorithm **)
  let enumerate_paths_from_source_to_leaves (g : G.t) (source : G.LiteralVertex.t) :
      G.V.t array array =
    let havebeenmap =
      let out = Hashtbl.create 777 in
      G.iter_edges (fun v1 v2 -> Hashtbl.add out (v1, v2) 0) g ;
      out
    in
    let rec inner (current : G.LiteralVertex.t) (smol_acc : Vertex.t list)
        (big_acc : Vertex.t list list) : G.V.t list list =
      if G.is_leaf current g then List.rev smol_acc :: big_acc
      else
        let children = G.succ g (G.LiteralVertex.to_vertex_cheap current) in
        List.fold
          ~f:(fun acc child ->
            let havebeen_num =
              Hashtbl.find havebeenmap (G.LiteralVertex.to_vertex_cheap current, child)
            in
            if havebeen_num >= 1 then acc
            else
              let current_alist_updated =
                Hashtbl.replace havebeenmap
                  (G.LiteralVertex.to_vertex_cheap current, child)
                  (havebeen_num + 1)
              in
              inner (G.LiteralVertex.of_vertex child) (child :: smol_acc) acc )
          ~init:big_acc children
    in
    Array.of_list @@ List.map ~f:Array.of_list
    @@ inner source [G.LiteralVertex.to_vertex_cheap source] []


  (* Array version of the above *)
  let find_paths_from_source_to_dest (graph : G.t) (source : G.LiteralVertex.t) (dest : G.V.t) :
      t array =
    let paths_to_all_leaves = enumerate_paths_from_source_to_leaves graph source in
    let paths_to_dest =
      Array.filter ~f:(fun path -> Array.mem ~equal:G.V.equal path dest) paths_to_all_leaves
    in
    let take_while_to_dest_elem_index =
      Array.map
        ~f:(fun path_to_dest ->
          let dest_elem_index, _ =
            Array.findi_exn ~f:(fun index elem -> G.V.equal elem dest) path_to_dest
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


let identify_longest_trunks (graph : G.t) : t array =
  let df_only_graph = G.leave_only_df_edges graph in
  let roots = Array.of_list @@ G.collect_df_roots df_only_graph in
  let leaves = Array.of_list @@ G.collect_df_leaves df_only_graph in
  let carpro = Array.cartesian_product roots leaves in
  (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
  let reachable_root_and_leaf_pairs =
    Array.filter ~f:(fun (root, leaf) -> PathUtils.is_reachable root leaf df_only_graph) carpro
  in
  (* now, find the path between the root and the leaf. *)
  Array.map
    ~f:(fun (root, leaf) ->
      let all_paths =
        PathUtils.find_paths_from_source_to_dest df_only_graph (G.LiteralVertex.of_vertex root) leaf
      in
      find_longest_path all_paths )
    reachable_root_and_leaf_pairs


let find_trunks_containing_vertex (graph : G.t) (vertex : G.V.t) =
  let all_trunks = identify_longest_trunks graph in
  Array.filter ~f:(fun trunk -> Array.mem ~equal:Vertex.equal trunk vertex) all_trunks


module Serializer = struct
  type json = Yojson.Basic.t

  let to_json (index : int) (trunk : t) : string * json =
    let trunk_json_repr =
      `List
        ( trunk
        |> Array.map ~f:(fun vertex ->
               G.LiteralVertex.of_vertex vertex |> G.LiteralVertex.to_string
               |> fun string -> `String string )
        |> Array.to_list )
    in
    (string_of_int index, trunk_json_repr)


  let all_longest_trunks_to_json (graph : G.t) : json =
    let all_longest_trunks = identify_longest_trunks graph in
    `Assoc (Array.to_list @@ Array.mapi ~f:to_json all_longest_trunks)


  let serialize_graph_trunks_to_json (graph : G.t) : unit =
    let filename = F.asprintf "%s_all_longest_trunks.json" graph.comp_unit in
    let out_chan = Out_channel.create filename in
    pretty_to_channel out_chan (all_longest_trunks_to_json graph) ;
    Out_channel.flush out_chan ;
    Out_channel.close out_chan
end
