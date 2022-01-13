open GraphRepr
open InfixOperators
open ListMonad

type t = G.V.t list [@@deriving compare, equal]

let pp (trunk : t) : unit =
  print_endline "[" ;
  List.iter ~f:(fun vertex -> print_endline @@ Vertex.to_string vertex ^ ", ") trunk ;
  print_endline "]"


let identify_trunks (graph : G.t) : t list =
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
  >>= fun (root, leaf) ->
  PathUtils.find_path_from_source_to_dest df_only_graph (G.LiteralVertex.of_vertex root) leaf


let find_trunks_containing_vertex graph vertex =
  let all_trunks = identify_trunks graph in
  List.filter ~f:(fun trunk -> List.mem ~equal:Vertex.equal trunk vertex) all_trunks


(** 어떤 root로부터 시작하는 trunk 중 가장 먼 곳에 있는 leaf *)
let find_longest_trunk_from_leaf_to_sink ~(root : G.V.t) ~(leaf : G.V.t) (graph : G.t) : t =
  let all_trunks = identify_trunks graph in
  let trunks_in_question =
    List.filter all_trunks ~f:(fun trunk ->
        let root_match = Vertex.equal (List.hd_exn trunk) root in
        let leaf_match = Vertex.equal (List.last_exn trunk) leaf in
        root_match && leaf_match )
  in
  List.hd_exn
  @@ List.sort trunks_in_question ~compare:(fun trunk1 trunk2 ->
         -Int.compare (List.length trunk1) (List.length trunk2) )
