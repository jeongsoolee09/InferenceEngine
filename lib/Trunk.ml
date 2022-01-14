open GraphRepr
open InfixOperators
open ListMonad

type t = G.V.t list [@@deriving compare, equal]

module Hashtbl = Caml.Hashtbl

module PathUtils = struct
  let is_reachable (source : G.V.t) (dest : G.V.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let module DFS = Graph.Traverse.Dfs (G) in
    let descendants = DFS.fold_component List.cons [] graph source in
    List.mem ~equal:G.V.equal descendants dest


  let increment_option prev =
    let ( >>= ) = Option.( >>= ) and return = Option.return in
    prev >>= fun x -> return (x + 1)


  (** For every leaf, print paths to the leaf from the given source, where the given graph may
      contain a cycle, using a customized DFS algorithm **)
  let enumerate_paths_from_source_to_leaves (g : G.t) (source : G.LiteralVertex.t) : G.V.t list list
      =
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
    inner source [G.LiteralVertex.to_vertex_cheap source] []


  (** Find all paths from the given source to the given destination. **)
  let find_paths_from_source_to_dest (graph : G.t) (source : G.LiteralVertex.t) (dest : G.V.t) :
      t list =
    enumerate_paths_from_source_to_leaves graph source
    |> List.filter ~f:(fun path -> List.mem ~equal:G.V.equal path dest)
    >>| List.take_while ~f:(fun vertex -> not @@ G.V.equal vertex dest)
    >>| fun list -> List.append list [dest]
end

let to_string (trunk : t) : string =
  let acc = List.fold ~f:(fun acc vertex -> acc ^ Vertex.to_string vertex ^ ", ") ~init:"" trunk in
  "[" ^ acc ^ "]"


let find_longest_path (paths : t list) : t =
  Option.value_exn
    (List.max_elt ~compare:(fun p1 p2 -> Int.compare (List.length p1) (List.length p2)) paths)


let identify_longest_trunks (graph : G.t) : t list =
  let df_only_graph = G.leave_only_df_edges graph in
  let roots = G.collect_df_roots df_only_graph in
  let leaves = G.collect_df_leaves df_only_graph in
  let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
  (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
  let reachable_root_and_leaf_pairs =
    List.filter ~f:(fun (root, leaf) -> PathUtils.is_reachable root leaf df_only_graph) carpro
  in
  (* now, find the path between the root and the leaf. *)
  reachable_root_and_leaf_pairs
  >>| fun (root, leaf) ->
  PathUtils.find_paths_from_source_to_dest df_only_graph (G.LiteralVertex.of_vertex root) leaf
  |> find_longest_path


let find_trunks_containing_vertex (graph : G.t) (vertex : G.V.t) =
  let all_trunks = identify_longest_trunks graph in
  List.filter ~f:(fun trunk -> List.mem ~equal:Vertex.equal trunk vertex) all_trunks


(** 어떤 root로부터 시작하는 trunk 중 가장 먼 곳에 있는 leaf *)
let find_longest_trunk_from_leaf_to_sink ~(root : G.V.t) ~(leaf : G.V.t) (graph : G.t) : t =
  let all_trunks = identify_longest_trunks graph in
  let trunks_in_question =
    List.filter all_trunks ~f:(fun trunk ->
        let root_match = Vertex.equal (List.hd_exn trunk) root in
        let leaf_match = Vertex.equal (List.last_exn trunk) leaf in
        root_match && leaf_match )
  in
  List.hd_exn
  @@ List.sort trunks_in_question ~compare:(fun trunk1 trunk2 ->
         -Int.compare (List.length trunk1) (List.length trunk2) )
