open GraphRepr

let ( >>= ) = List.( >>= )

and ( >>| ) = List.( >>| )

and return = List.return

module Trunk = struct
  type t = G.V.t list [@@deriving compare, equal]

  let pp (trunk : t) : unit =
    print_endline "[" ;
    List.iter ~f:(fun vertex -> print_endline @@ Vertex.to_string vertex ^ ", ") trunk ;
    print_endline "]"
end

type trunk = Trunk.t

let identify_trunks (graph : G.t) : trunk list =
  let roots = GraphUtils.collect_roots graph in
  let leaves = GraphUtils.collect_leaves graph in
  let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
  (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
  let reachable_root_and_leaf_pairs =
    List.filter ~f:(fun (root, leaf) -> GraphUtils.is_reachable root leaf graph) carpro
  in
  (* now, find the path between the root and the leaf. *)
  reachable_root_and_leaf_pairs
  >>= fun (root, leaf) -> GraphUtils.PathUtils.find_path_from_source_to_dest graph root leaf


(** Do the two trunks share the same callee? **)
let same_callee_in_trunk_count ((trunk1, trunk2) : trunk * trunk) : int =
  let trunk1_only_methods = trunk1 >>| fst and trunk2_only_methods = trunk2 >>| fst in
  List.fold
    ~f:(fun acc vertex ->
      let matching = List.mem ~equal:G.V.equal trunk2 vertex in
      if matching then acc + 1 else acc )
    ~init:0 trunk1


(** Do the two trunks share the same suffixes? If it does, what is its length? **)
let trunks_share_same_suffixes_length ((trunk1, trunk2) : trunk * trunk) : int =
  (* if the two trunks are not equal in size, prepends some fillers *)
  let trunk1_only_methods = trunk1 >>| fst and trunk2_only_methods = trunk2 >>| fst in
  let trunk1_length = List.length trunk1 and trunk2_length = List.length trunk2 in
  let trunk1_revised, trunk2_revised =
    match Int.compare trunk1_length trunk2_length with
    | -1 ->
        (* trunk1 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk2_length - trunk1_length) in
        (fillers @ trunk1_only_methods, trunk2_only_methods)
    | 0 ->
        (trunk1_only_methods, trunk2_only_methods)
    | 1 ->
        (* trunk2 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk1_length - trunk2_length) in
        (trunk1_only_methods, fillers @ trunk2_only_methods)
    | _ ->
        failwith "this is impossible"
  in
  let zipped = List.zip_exn trunk1_revised trunk2_revised in
  let suffix = List.take_while ~f:(fun (v1, v2) -> String.equal v1 v2) zipped in
  List.length suffix
