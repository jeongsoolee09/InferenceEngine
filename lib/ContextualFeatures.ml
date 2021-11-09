open GraphRepr

let ( >>= ) = List.( >>= )

and ( >>| ) = List.( >>| )

and return = List.return

module DFS = Graph.Traverse.Dfs (G)

module Trunk = struct
  type t = G.V.t list
end

module Utils = struct
  let is_reachable (source : G.V.t) (dest : G.V.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let descendants = DFS.fold_component List.cons [] graph source in
    List.mem ~equal:G.V.equal descendants dest


  let collect_roots (graph : G.t) : Trunk.t =
    G.fold_vertex
      (fun vertex acc -> if Int.( = ) (G.in_degree graph vertex) 0 then vertex :: acc else acc)
      graph []


  let collect_leaves (graph : G.t) : Trunk.t =
    G.fold_vertex
      (fun vertex acc -> if Int.( = ) (G.out_degree graph vertex) 0 then vertex :: acc else acc)
      graph []


  let is_root (vertex : G.V.t) (graph : G.t) : bool = Int.equal (G.in_degree graph vertex) 0

  let is_leaf (vertex : G.V.t) (graph : G.t) : bool = Int.equal (G.out_degree graph vertex) 0

  module PathUtils = struct
    module HaveBeenMap = struct
      module WithEdgeDomain = Caml.Map.Make (G.E)
      include WithEdgeDomain

      type value = int

      type t = Int.t WithEdgeDomain.t

      let init (graph : G.t) : t = G.fold_edges (fun v1 v2 acc -> add (v1, v2) 0 acc) graph empty
    end

    let increment_option (prev : int option) : int option =
      match prev with None -> None | Some n -> Some (n + 1)


    (** For every leaf, print paths to the leaf from the given source, where the given graph may
        contain a cycle, using a customized DFS algorithm **)
    let enumerate_paths_from_source_to_leaves (graph : G.t) (source : G.V.t) : Trunk.t list =
      let rec inner (current : G.V.t) (smol_acc : Trunk.t) (big_acc : Trunk.t list)
          (current_havebeenmap : HaveBeenMap.t) : Trunk.t list =
        if is_leaf current graph then List.rev smol_acc :: big_acc
        else
          let children = G.succ graph current in
          List.fold
            ~f:(fun acc child ->
              if HaveBeenMap.find (current, child) current_havebeenmap >= 1 then acc
              else
                let current_alist_updated =
                  HaveBeenMap.update (current, child) increment_option current_havebeenmap
                in
                inner child (child :: smol_acc) acc current_alist_updated )
            ~init:big_acc children
      in
      inner source [source] [] (HaveBeenMap.init graph)


    (** Find all paths from the given source to the given destination. **)
    let find_path_from_source_to_dest (graph : G.t) (source : G.V.t) (dest : G.V.t) : Trunk.t list =
      enumerate_paths_from_source_to_leaves graph source
      |> List.filter ~f:(fun path -> List.mem ~equal:G.V.equal path dest)
      >>| List.take_while ~f:(fun vertex -> not @@ G.V.equal vertex dest)
  end

  let identify_trunks (graph : G.t) : Trunk.t list =
    let roots = collect_roots graph in
    let leaves = collect_leaves graph in
    let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
    (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
    let reachable_root_and_leaf_pairs =
      List.filter ~f:(fun (root, leaf) -> is_reachable root leaf graph) carpro
    in
    (* now, find the path between the root and the leaf. *)
    reachable_root_and_leaf_pairs
    >>= fun (root, leaf) -> PathUtils.find_path_from_source_to_dest graph root leaf
end

(** Do the two trunks share the same callee? **)
let same_callee_in_trunk_count (trunk1 : Trunk.t) (trunk2 : Trunk.t) : int =
  let trunk1_only_methods = trunk1 >>| fst and trunk2_only_methods = trunk2 >>| fst in
  List.fold
    ~f:(fun acc vertex ->
      let matching = List.mem ~equal:G.V.equal trunk2 vertex in
      if matching then acc + 1 else acc )
    ~init:0 trunk1


(** Do the two trunks share the same suffixes? If it does, what is its length? **)
let trunks_share_same_suffixes_length (trunk1 : Trunk.t) (trunk2 : Trunk.t) : int =
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
