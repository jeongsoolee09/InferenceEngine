open GraphRepr

(* monad things *)

let ( >>= ) = List.( >>= )

and return = List.return

module DFS = Graph.Traverse.Dfs (G)

module NodeWiseUtils = struct
  let extract_package_name (unique_identifier : string) : string =
    let regex =
      Str.regexp "\\([a-z]+\\.[a-z]+\\.[a-z]+\\)\\.[a-zA-Z]+\\.[a-zA-Z]+(.*):[a-zA-Z]+\"\\)"
    in
    assert (Str.string_match regex unique_identifier 0) ;
    Str.matched_group 1 unique_identifier
end

module ContextualUtils = struct
  let is_reachable (source : G.V.t) (dest : G.V.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let descendants = DFS.fold_component List.cons [] graph source in
    List.mem ~equal:G.V.equal descendants dest


  let collect_roots (graph : G.t) : G.V.t list =
    G.fold_vertex
      (fun vertex acc -> if Int.( = ) (G.in_degree graph vertex) 0 then vertex :: acc else acc)
      graph []


  let collect_leaves (graph : G.t) : G.V.t list =
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
    let custom_dfs (graph : G.t) (source : G.V.t) : G.V.t list list =
      let rec inner (current : G.V.t) (smol_acc : G.V.t list) (big_acc : G.V.t list list)
          (current_havebeenmap : HaveBeenMap.t) : G.V.t list list =
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
  end

  let identify_trunks (graph : G.t) : G.t list =
    let roots = collect_roots graph in
    let leaves = collect_leaves graph in
    let carpro = roots >>= fun root -> leaves >>= fun leaf -> return (root, leaf) in
    (* not all leaves are reachable from all roots. So we filter out unreachable (root, leaf) pairs. *)
    let reachable_root_and_leaf_pairs =
      List.filter ~f:(fun (root, leaf) -> is_reachable root leaf graph) carpro
    in
    (* now, find the path between the root and the leaf. *)
    raise TODO
end

module NodeWiseFeatures = struct
  (* methname is the result of Procname.to_string *)
  let is_framework_code (methname : string) : bool =
    let skip_methods =
      Deserializer.deserialize_method_txt
        "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access/skip_func.txt"
    and udf_methods =
      Deserializer.deserialize_method_txt
        "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access/Methods.txt"
    in
    let this_project_package_name = NodeWiseUtils.extract_package_name @@ List.hd_exn udf_methods in
    let not_this_project_methods_and_java_methods =
      List.filter
        ~f:(fun str ->
          not
            ( String.is_substring ~substring:"java." str
            || String.is_substring ~substring:this_project_package_name str ) )
        skip_methods
    in
    (* TODO 밑의 expr는 정답이 아님. 더 코딩하기 싫어서 타입만 맞추고 냅둔 것. *)
    List.mem ~equal:String.equal not_this_project_methods_and_java_methods methname


  (* methname is the result of Procname.to_string *)
  let get_class (methname : string) : string =
    let regex = Str.regexp ".+ \\(.+\\)\\..+(.*)" in
    assert (Str.string_match regex methname 0) ;
    Str.matched_group 1 methname
end

module ContextualFeatures = struct end
