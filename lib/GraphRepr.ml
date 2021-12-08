open ListMonad
open InfixOperators
open Yojson.Basic

type json = Yojson.Basic.t

module Set = Caml.Set
module F = Format

module TaintLabel = struct
  type t = Source | Sink | Sanitizer | None | Indeterminate [@@deriving equal]

  let to_string (label : t) : string =
    match label with
    | Source ->
        "source"
    | Sink ->
        "sink"
    | Sanitizer ->
        "sanitizer"
    | None ->
        "none"
    | Indeterminate ->
        "indeterminate"


  let is_source (label : t) : bool = equal label Source

  let is_sink (label : t) : bool = equal label Sink

  let is_sanitizer (label : t) : bool = equal label Sanitizer

  let is_none (label : t) : bool = equal label None

  let is_indeterminate (label : t) : bool = equal label Indeterminate
end

module ProbQuadruple = struct
  type t = {src: float; sin: float; san: float; non: float} [@@deriving compare, equal]

  let initial = {src= 0.25; sin= 0.25; san= 0.25; non= 0.25}

  let dummy = {src= 0.; sin= 0.; san= 0.; non= 0.}

  let check_sanity (dist : t) : unit =
    let ( = ) = Float.( = ) and ( + ) = Float.( + ) in
    assert (dist.src + dist.sin + dist.san + dist.non = float 1)


  let winning_threshold = 0.1

  let alist_of_dist (dist : t) : (string * float) list =
    [("source", dist.src); ("sink", dist.sin); ("sanitizer", dist.san); ("none", dist.non)]


  type label = TaintLabel.t

  let label_of_string (str : string) : label =
    match str with
    | "source" ->
        Source
    | "sink" ->
        Sink
    | "sanitizer" ->
        Sanitizer
    | "none" ->
        None
    | "indeterminate" ->
        Indeterminate
    | otherwise ->
        failwith ("invalid string: " ^ otherwise)


  let to_string (quad : t) =
    F.asprintf "{src= %f; sin= %f; san= %f; non= %f}" quad.src quad.sin quad.san quad.non


  let determine_label (dist : t) : label =
    let alist = alist_of_dist dist in
    let sorted_decreasing =
      List.rev @@ List.sort alist ~compare:(fun (_, v1) (_, v2) -> Float.compare v1 v2)
    in
    let label, v1 = List.nth_exn sorted_decreasing 0 in
    let _, v2 = List.nth_exn sorted_decreasing 1 in
    if Float.( >= ) (Float.( - ) v1 v2) winning_threshold then label_of_string label
    else Indeterminate


  let is_source (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Source

  let is_sin (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Sink

  let is_sanitizer (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Sanitizer

  let is_none (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.None

  let is_indeterrminate (dist : t) : bool =
    TaintLabel.equal (determine_label dist) TaintLabel.Indeterminate
end

module Vertex = struct
  type t = string * string * ProbQuadruple.t [@@deriving compare]

  let hash = Hashtbl.hash

  let equal ((meth1, locset1, _) : t) ((meth2, locset2, _) : t) : bool =
    (* we ignore the quadruple in defining the identity: that's just an attribute *)
    String.equal meth1 meth2 && String.equal locset1 locset2


  let to_string ((procstring, locstring, _) : t) : string =
    F.asprintf "(\"%s\", \"%s\")" procstring locstring


  let vertex_list_to_string (lst : t list) : string =
    let accumed = List.fold ~f:(fun acc vertex -> acc ^ to_string vertex ^ "; ") ~init:"" lst in
    F.asprintf "[%s]" accumed
end

module EdgeLabel = struct
  type t = DataFlow | NodeWiseSimilarity | ContextualSimilarity [@@deriving equal, compare]

  let default : t = DataFlow (* beware when using add_edge, since its label defaults to DataFlow! *)
end

module VertexPair = struct
  type t = Vertex.t * Vertex.t [@@deriving equal, compare]
end

module G = struct
  module BiDiGraph = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (EdgeLabel)
  include BiDiGraph

  module LiteralVertex = struct
    type t = string * string [@@deriving compare, equal, sexp]

    let to_vertex ((meth, loc) : t) (graph : BiDiGraph.t) : Vertex.t =
      let res_opt =
        fold_vertex
          (fun ((target_meth, target_loc, dist) as vertex) acc ->
            if String.equal meth target_meth && String.equal loc target_loc then Some vertex
            else acc )
          graph None
      in
      match res_opt with
      | Some vertex ->
          vertex
      | None ->
          failwithf "could not find dist for (\"%s\", \"%s\")" meth loc ()


    let to_string ((meth, loc) : t) : string = F.asprintf "(\"%s\", \"%s\")" meth loc

    let of_vertex ((str1, str2, _) : Vertex.t) : t = (str1, str2)
  end

  module Saturation = struct
    let saturated_parameter = 0.2 (* TEMP: subject to change *)

    let dist_is_saturated (quad : ProbQuadruple.t) : bool =
      let sorted = List.sort ~compare:Float.compare [quad.src; quad.sin; quad.san; quad.non] in
      let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
      Float.( >= ) (first -. second) saturated_parameter


    let all_dists_in_graph_are_saturated (graph : t) : bool =
      fold_vertex
        (fun vertex acc ->
          let dist = trd3 vertex in
          dist_is_saturated dist && acc )
        graph true
  end

  let lookup_dist_for_meth_and_loc (meth : string) (loc : string) (graph : t) : ProbQuadruple.t =
    let res_opt =
      fold_vertex
        (fun (target_meth, target_loc, dist) acc ->
          if String.equal meth target_meth && String.equal loc target_loc then Some dist else acc )
        graph None
    in
    match res_opt with
    | Some dist ->
        dist
    | None ->
        failwithf "could not find dist for (\"%s\", \"%s\")" meth loc ()


  let print_snapshot_diff_verbose prev_snapshot next_snapshot =
    let all_pairs_without_dist =
      fold_vertex (fun (meth, loc, _) acc -> (meth, loc) :: acc) prev_snapshot []
    in
    let diff =
      List.fold
        ~f:(fun acc (meth, loc) ->
          let prev_snapshot_dist = lookup_dist_for_meth_and_loc meth loc prev_snapshot
          and next_snapshot_dist = lookup_dist_for_meth_and_loc meth loc next_snapshot in
          if not @@ ProbQuadruple.equal prev_snapshot_dist next_snapshot_dist then
            ((meth, loc), prev_snapshot_dist, next_snapshot_dist) :: acc
          else acc )
        all_pairs_without_dist ~init:[]
    in
    List.iter
      ~f:(fun ((meth, loc), prev_dist, next_dist) ->
        Out_channel.output_string Out_channel.stdout
        @@ F.asprintf "Vertex (%s, %s)'s dist was updated from %s to %s" meth loc
             (ProbQuadruple.to_string prev_dist)
             (ProbQuadruple.to_string next_dist) ;
        Out_channel.newline Out_channel.stdout ;
        Out_channel.newline Out_channel.stdout )
      diff


  let print_snapshot_diff prev_snapshot next_snapshot =
    let all_pairs_without_dist =
      fold_vertex (fun (meth, loc, _) acc -> (meth, loc) :: acc) prev_snapshot []
    in
    let diff =
      List.fold
        ~f:(fun acc (meth, loc) ->
          let prev_snapshot_label =
            ProbQuadruple.determine_label @@ lookup_dist_for_meth_and_loc meth loc prev_snapshot
          and next_snapshot_label =
            ProbQuadruple.determine_label @@ lookup_dist_for_meth_and_loc meth loc next_snapshot
          in
          if not @@ TaintLabel.equal prev_snapshot_label next_snapshot_label then
            ((meth, loc), prev_snapshot_label, next_snapshot_label) :: acc
          else acc )
        all_pairs_without_dist ~init:[]
    in
    List.iter
      ~f:(fun ((meth, loc), prev_label, next_label) ->
        Out_channel.output_string Out_channel.stdout
        @@ F.asprintf "Vertex (%s, %s)'s label was updated from %s to %s" meth loc
             (TaintLabel.to_string prev_label) (TaintLabel.to_string next_label) ;
        Out_channel.newline Out_channel.stdout ;
        Out_channel.newline Out_channel.stdout )
      diff


  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_name ((meth, locset, _) : V.t) : string = F.asprintf "\"(%s, %s)\"" meth locset

  let vertex_attributes ((meth, locset, dist) : V.t) =
    let shape = if Saturation.dist_is_saturated dist then [`Shape `Circle] else [`Shape `Box] in
    let label =
      match ProbQuadruple.determine_label dist with
      | Source ->
          [`Fillcolor 0xf54260; `Style `Filled]
      | Sink ->
          [`Fillcolor 0xf5e040; `Style `Filled]
      | Sanitizer ->
          [`Fillcolor 0xf59140; `Style `Filled]
      | None ->
          [`Fillcolor 0x58f540; `Style `Filled]
      | Indeterminate ->
          [`Fillcolor 0xffffff; `Style `Filled]
    in
    shape @ label


  let get_subgraph _ = None

  let default_edge_attributes _ = []

  let edge_attributes (_, label, _) =
    match label with
    | EdgeLabel.DataFlow ->
        [`Label "DF"; `Color 0x000000]
    | EdgeLabel.NodeWiseSimilarity ->
        [`Label "NS"; `Color 0xf54260]
    | EdgeLabel.ContextualSimilarity ->
        [`Label "CS"; `Color 0x3480eb]


  let pp_vertex = vertex_name

  let pp_edge (v1, v2) = F.asprintf "\"(%s, %s)\"" (vertex_name v1) (vertex_name v2)

  module Trunk = struct
    type t = V.t list [@@deriving compare, equal]

    let pp (trunk : t) : unit =
      print_endline "[" ;
      List.iter ~f:(fun vertex -> print_endline @@ Vertex.to_string vertex ^ ", ") trunk ;
      print_endline "]"
  end

  type trunk = Trunk.t

  let strong_update_dist (target_vertex : V.t) (new_dist : ProbQuadruple.t) (graph : t) : t =
    map_vertex
      (fun ((meth, label, _) as vertex) ->
        if Vertex.equal vertex target_vertex then (meth, label, new_dist) else vertex )
      graph


  let collect_roots (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc -> if Int.( = ) (in_degree graph vertex) 0 then vertex :: acc else acc)
      graph []


  let collect_leaves (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc -> if Int.( = ) (out_degree graph vertex) 0 then vertex :: acc else acc)
      graph []


  let is_root (vertex : LiteralVertex.t) (graph : t) : bool =
    Int.equal (in_degree graph (LiteralVertex.to_vertex vertex graph)) 0


  let is_leaf (vertex : LiteralVertex.t) (graph : t) : bool =
    Int.equal (out_degree graph (LiteralVertex.to_vertex vertex graph)) 0


  let any_label_preds (vertex : LiteralVertex.t) (graph : t) =
    fold_edges
      (fun v1 v2 acc ->
        if Vertex.equal v2 (LiteralVertex.to_vertex vertex graph) then v1 :: acc else acc )
      graph []


  let any_label_succs (vertex : LiteralVertex.t) (graph : t) =
    fold_edges
      (fun v1 v2 acc ->
        if Vertex.equal v1 (LiteralVertex.to_vertex vertex graph) then v2 :: acc else acc )
      graph []


  let find_in_edges (vertex : LiteralVertex.t) (graph : t) : E.t list =
    fold_edges_e
      (fun ((_, label, v2) as edge) acc ->
        if V.equal (LiteralVertex.to_vertex vertex graph) v2 then edge :: acc else acc )
      graph []


  let get_preds (vertex : LiteralVertex.t) (graph : t) ~(label : EdgeLabel.t) : Vertex.t list =
    fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, target_label, v2) ->
           Vertex.equal (LiteralVertex.to_vertex vertex graph) v2
           && EdgeLabel.equal label target_label )
    >>| fst3


  let get_succs (vertex : LiteralVertex.t) (graph : t) ~(label : EdgeLabel.t) : Vertex.t list =
    fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (v, target_label, _) ->
           Vertex.equal (LiteralVertex.to_vertex vertex graph) v
           && EdgeLabel.equal label target_label )
    >>| fst3


  let get_preds_any (vertex : LiteralVertex.t) (graph : t) : Vertex.t list =
    fold_pred List.cons graph (LiteralVertex.to_vertex vertex graph) []


  let get_succs_any (vertex : LiteralVertex.t) (graph : t) : Vertex.t list =
    fold_succ List.cons graph (LiteralVertex.to_vertex vertex graph) []


  let is_df_root (vertex : LiteralVertex.t) (graph : t) : bool =
    Int.equal (List.length (get_preds vertex graph ~label:EdgeLabel.DataFlow)) 0
    && Int.( > ) (List.length (get_succs vertex graph ~label:EdgeLabel.DataFlow)) 0


  let is_df_leaf (vertex : LiteralVertex.t) (graph : t) : bool =
    Int.equal (List.length (get_succs vertex graph ~label:EdgeLabel.DataFlow)) 0
    && Int.( > ) (List.length (get_preds vertex graph ~label:EdgeLabel.DataFlow)) 0


  let collect_df_roots (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if is_df_root (LiteralVertex.of_vertex vertex) graph then vertex :: acc else acc )
      graph []


  let collect_df_leaves (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if is_df_leaf (LiteralVertex.of_vertex vertex) graph then vertex :: acc else acc )
      graph []


  let get_edges (graph : t) ~(label : EdgeLabel.t) : E.t list =
    fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, target_label, _) -> EdgeLabel.equal label target_label)


  let get_succs (graph : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : V.t list =
    let out_df_edges =
      fold_edges_e
        (fun ((v1, target_label, _) as edge) acc ->
          if
            Vertex.equal v1 (LiteralVertex.to_vertex vertex graph)
            && EdgeLabel.equal target_label label
          then edge :: acc
          else acc )
        graph []
    in
    out_df_edges >>| trd3


  let get_preds (graph : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : V.t list =
    let in_df_edges =
      fold_edges_e
        (fun ((_, target_label, v2) as edge) acc ->
          if
            Vertex.equal v2 (LiteralVertex.to_vertex vertex graph)
            && EdgeLabel.equal target_label label
          then edge :: acc
          else acc )
        graph []
    in
    in_df_edges >>| fst3


  let is_pointing_to_each_other (v1 : LiteralVertex.t) (v2 : LiteralVertex.t) (graph : t)
      ~(label : EdgeLabel.t) : bool =
    let v2_is_v1's_succ =
      List.mem ~equal:Vertex.equal (get_succs graph v1 ~label) (LiteralVertex.to_vertex v2 graph)
    and v1_is_v2's_succ =
      List.mem ~equal:Vertex.equal (get_succs graph v2 ~label) (LiteralVertex.to_vertex v1 graph)
    in
    v2_is_v1's_succ && v1_is_v2's_succ


  let this_method_vertices (graph : t) (method_ : string) : V.t list =
    fold_vertex
      (fun vertex acc -> if String.equal method_ (fst3 vertex) then vertex :: acc else acc)
      graph []


  module GUndirected = Graph.Persistent.Graph.Concrete (Vertex)

  let to_undirected (graph : t) : GUndirected.t =
    let vertices = fold_vertex List.cons graph [] in
    let edges = fold_edges (fun v1 v2 acc -> (v1, v2) :: acc) graph [] in
    GUndirected.empty
    |> (fun graph ->
         List.fold ~f:(fun acc vertex -> GUndirected.add_vertex acc vertex) ~init:graph vertices )
    |> fun graph ->
    List.fold ~f:(fun acc (v1, v2) -> GUndirected.add_edge acc v1 v2) ~init:graph edges


  let leave_only_df_edges (graph : t) : t =
    fold_edges_e
      (fun ((_, label, _) as edge) acc ->
        if not @@ EdgeLabel.equal EdgeLabel.DataFlow label then remove_edge_e acc edge else acc )
      graph graph


  let all_vertices_of_graph (graph : t) : V.t list =
    let all_vertices_with_dup = fold_vertex List.cons graph [] in
    let module VertexSet = Caml.Set.Make (V) in
    all_vertices_with_dup |> VertexSet.of_list |> VertexSet.elements


  let all_methods_of_graph (graph : t) : string list =
    let all_vertices = all_vertices_of_graph graph in
    let module StringSet = Caml.Set.Make (String) in
    all_vertices >>| fst3 |> StringSet.of_list |> StringSet.elements


  let is_bidirectional_vertex (vertex : LiteralVertex.t) (graph : t) ~(label : EdgeLabel.t) : bool =
    let vertex_succs = get_succs graph vertex ~label in
    List.fold
      ~f:(fun acc succ_vertex ->
        let succ_vertex_preds = get_preds graph (LiteralVertex.of_vertex succ_vertex) ~label in
        let found =
          List.exists
            ~f:(fun succ_vertex_pred ->
              LiteralVertex.equal vertex (LiteralVertex.of_vertex succ_vertex_pred) )
            succ_vertex_preds
        in
        found || acc )
      ~init:false vertex_succs
end

module PathUtils = struct
  module HaveBeenMap = struct
    module WithEdgeDomain = Caml.Map.Make (VertexPair)
    include WithEdgeDomain

    type value = int

    type t = Int.t WithEdgeDomain.t

    let init (graph : G.t) : t = G.fold_edges (fun v1 v2 acc -> add (v1, v2) 0 acc) graph empty
  end

  let is_reachable (source : G.V.t) (dest : G.V.t) (graph : G.t) : bool =
    (* dest is reachable from source iff dest is one of the descendants of source. *)
    let module DFS = Graph.Traverse.Dfs (G) in
    let descendants = DFS.fold_component List.cons [] graph source in
    List.mem ~equal:G.V.equal descendants dest


  let increment_option (prev : int option) : int option =
    match prev with None -> None | Some n -> Some (n + 1)


  (** For every leaf, print paths to the leaf from the given source, where the given graph may
      contain a cycle, using a customized DFS algorithm **)
  let enumerate_paths_from_source_to_leaves (graph : G.t) (source : G.LiteralVertex.t) :
      G.V.t list list =
    let rec inner (current : G.LiteralVertex.t) (smol_acc : Vertex.t list)
        (big_acc : Vertex.t list list) (current_havebeenmap : HaveBeenMap.t) : G.V.t list list =
      if G.is_leaf current graph then List.rev smol_acc :: big_acc
      else
        let children = G.succ graph (G.LiteralVertex.to_vertex current graph) in
        List.fold
          ~f:(fun acc child ->
            if
              HaveBeenMap.find (G.LiteralVertex.to_vertex current graph, child) current_havebeenmap
              >= 1
            then acc
            else
              let current_alist_updated =
                HaveBeenMap.update
                  (G.LiteralVertex.to_vertex current graph, child)
                  increment_option current_havebeenmap
              in
              inner (G.LiteralVertex.of_vertex child) (child :: smol_acc) acc current_alist_updated
            )
          ~init:big_acc children
    in
    inner source [G.LiteralVertex.to_vertex source graph] [] (HaveBeenMap.init graph)


  (** Find all paths from the given source to the given destination. **)
  let find_path_from_source_to_dest (graph : G.t) (source : G.LiteralVertex.t) (dest : G.V.t) :
      G.V.t list list =
    enumerate_paths_from_source_to_leaves graph source
    |> List.filter ~f:(fun path -> List.mem ~equal:G.V.equal path dest)
    >>| List.take_while ~f:(fun vertex -> not @@ G.V.equal vertex dest)
    >>| fun list -> List.append list [dest]
end

module Dot = Graph.Graphviz.Dot (G)

module ChainSlice = struct
  type t =
    | DefineSlice of (string * string * string * string) (* current_method, access_path, location, using *)
    | CallSlice of (string * string * string * string) (* current_method, callee, location, with *)
    | VoidCallSlice of (string * string * string * string) (* current_method, callee, location, with *)
    | RedefineSlice of (string * string * string) (* current_method, location, access_path *)
    | DeadSlice of string (* current_method *)
    | DeadByCycleSlice of string (* current_method *)
    | Temp of (string * string)
  (* current_method, location *)
  [@@deriving equal, compare]

  let pp (slice : t) : string =
    match slice with
    | DefineSlice (curr, ap, loc, using) ->
        F.asprintf "DefineSlice (%s, %s, %s, %s)" curr ap loc using
    | CallSlice (curr, callee, loc, with_) ->
        F.asprintf "CallSlice (%s, %s, %s, %s)" curr callee loc with_
    | VoidCallSlice (curr, ap, loc, using) ->
        F.asprintf "VoidCallSlice (%s, %s, %s, %s)" curr ap loc using
    | RedefineSlice (curr, loc, ap) ->
        F.asprintf "RedefineSlice (%s, %s, %s)" curr loc ap
    | DeadSlice curr ->
        F.asprintf "DeadSlice (%s)" curr
    | DeadByCycleSlice curr ->
        F.asprintf "DeadByCycleSlice (%s)" curr
    | Temp (curr, loc) ->
        F.asprintf "Temp (%s, %s)" curr loc


  let to_string (slices : t list) : string =
    let contents = List.fold ~f:(fun acc slice -> acc ^ pp slice ^ ", ") slices ~init:"" in
    "[ " ^ contents ^ " ]"


  let is_define slice = match slice with DefineSlice _ -> true | _ -> false

  let is_call slice = match slice with CallSlice _ -> true | _ -> false

  let is_voidcall slice = match slice with VoidCallSlice _ -> true | _ -> false

  let is_redefine slice = match slice with RedefineSlice _ -> true | _ -> false

  let is_dead slice = match slice with DeadSlice _ -> true | _ -> false

  let is_deadbycycle slice = match slice with DeadByCycleSlice _ -> true | _ -> false

  let chain_slice_of_json_assoc (json_assoc : json) : t =
    match json_assoc with
    | `Assoc alist -> (
      match List.Assoc.find_exn alist "status" ~equal:String.equal with
      | `String "Define" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let access_path =
            Util.to_string @@ List.Assoc.find_exn alist "access_path" ~equal:String.equal
          in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let using = Util.to_string @@ List.Assoc.find_exn alist "using" ~equal:String.equal in
          DefineSlice (current_method, access_path, location, using)
      | `String "Call" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let callee = Util.to_string @@ List.Assoc.find_exn alist "callee" ~equal:String.equal in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let with_ = Util.to_string @@ List.Assoc.find_exn alist "with" ~equal:String.equal in
          CallSlice (current_method, callee, location, with_)
      | `String "VoidCall" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let callee = Util.to_string @@ List.Assoc.find_exn alist "callee" ~equal:String.equal in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let with_ = Util.to_string @@ List.Assoc.find_exn alist "with" ~equal:String.equal in
          VoidCallSlice (current_method, callee, location, with_)
      | `String "Redefine" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let access_path =
            Util.to_string @@ List.Assoc.find_exn alist "access_path" ~equal:String.equal
          in
          RedefineSlice (current_method, location, access_path)
      | `String "Dead" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          DeadSlice current_method
      | `String "DeadByCycle" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          DeadByCycleSlice current_method
      | otherwise ->
          raise @@ Invalid_argument (Yojson.Basic.to_string otherwise) )
    | _ ->
        failwith "Type Error1"
end

module ChainSliceManager = struct
  let wrapped_chain_list_of_raw_json : json -> json list = Util.to_list

  let chain_slice_list_of_wrapped_chain (json : json) : ChainSlice.t list =
    match Util.member "chain" json with
    | `List json_list ->
        json_list >>| ChainSlice.chain_slice_of_json_assoc
    | _ ->
        failwith "Type Error2"
end

module VertexMaker = struct
  let vertex_of_chain_slice (chain_slice : ChainSlice.t) : G.V.t =
    match chain_slice with
    | DefineSlice (current, _, loc, _) ->
        (current, loc, ProbQuadruple.initial)
    | CallSlice (current, callee, loc, _) ->
        (callee, loc, ProbQuadruple.initial)
    | VoidCallSlice (current, callee, loc, _) ->
        (callee, loc, ProbQuadruple.initial)
    | RedefineSlice (current, loc, _) ->
        (current, loc, ProbQuadruple.initial)
    | DeadSlice current ->
        (current, "", ProbQuadruple.initial)
    | DeadByCycleSlice current ->
        (current, "", ProbQuadruple.initial)
    | Temp (current, loc) ->
        (current, loc, ProbQuadruple.initial)


  module VertexSet = Set.Make (Vertex)

  let get_all_vertices (raw_json : json) : G.V.t list =
    let vertices_with_dup =
      ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
      >>= ChainSliceManager.chain_slice_list_of_wrapped_chain
      |> List.filter ~f:(fun slice ->
             (not @@ ChainSlice.is_dead slice) && (not @@ ChainSlice.is_deadbycycle slice) )
      >>| vertex_of_chain_slice
    in
    (* remove duplicates by switching to and from a set *)
    let out = vertices_with_dup |> VertexSet.of_list |> VertexSet.elements in
    out


  let deserialize_all_important_methods () =
    Deserializer.deserialize_method_txt ()
    |> List.filter ~f:(fun method_str ->
           (not @@ String.is_substring method_str ~substring:"lambda")
           && (not @@ String.is_substring method_str ~substring:"Lambda")
           && (not @@ String.is_substring method_str ~substring:"<init>")
           && (not @@ String.is_substring method_str ~substring:"<clinit>") )
end

module ChainRefiners = struct
  let parse_skip_func (raw_signature : string) : string =
    let pattern = Str.regexp ".*\\.\\(.+\\)(.*)" in
    assert (Str.string_match pattern raw_signature 0) ;
    Str.matched_group 1 raw_signature


  let delete_inner_deads (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let all_but_last = List.drop_last_exn chain_slices in
    let dead_filtered =
      List.filter
        ~f:(fun chain_slice ->
          (not @@ ChainSlice.is_dead chain_slice) && (not @@ ChainSlice.is_deadbycycle chain_slice)
          )
        all_but_last
    in
    dead_filtered


  (** make a stub slice in front of the define slice of the chain. *)
  let process_head_define (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let head_define = List.hd_exn chain_slices in
    let define_current_method_field, location_field, define_using_field =
      match head_define with
      | DefineSlice (current_method, _, location, using_method) ->
          (current_method, location, using_method)
      | _ ->
          failwith "ahahahahah"
    in
    let skip_func_method_names =
      Deserializer.deserialize_skip_func ()
      |> List.filter ~f:(fun str -> not @@ String.is_prefix ~prefix:"__" str)
      >>| parse_skip_func
    in
    if
      (not @@ String.equal define_current_method_field define_using_field)
      && List.mem ~equal:String.equal skip_func_method_names (parse_skip_func define_using_field)
    then Temp (define_using_field, location_field) :: chain_slices
    else chain_slices


  let process_chainslices (chainslices : ChainSlice.t list) : ChainSlice.t list =
    let processors = [delete_inner_deads; process_head_define] in
    List.fold ~f:(fun acc processor -> processor acc) ~init:chainslices processors
end

module EdgeMaker = struct
  let make_bicycle_chain (list : 'a list) : ('a * 'a) list =
    let all_but_last = List.drop_last_exn list and all_but_first = List.tl_exn list in
    List.zip_exn all_but_last all_but_first


  (** Refines a raw chain-slice list into a G.E.t list, also emit an optional info of a void-call
      vertex. *)
  let refine_bicycle_chain (bicycle_chain : (ChainSlice.t * ChainSlice.t) list) :
      (ChainSlice.t * ChainSlice.t) list * G.LiteralVertex.t option =
    let slice1, slice2 = List.last_exn bicycle_chain in
    match slice2 with
    | DefineSlice (_, ap, loc, using) as void_call_slice ->
        let is_frontend_tmp_var_ap = String.is_prefix ~prefix:"($" in
        if is_frontend_tmp_var_ap ap then
          (List.slice bicycle_chain 0 (List.length bicycle_chain - 1), Some (using, loc))
        else (bicycle_chain, None)
    | _ ->
        (bicycle_chain, None)


  let collect_voidcall_vertices (chain_slices : ChainSlice.t list) : G.LiteralVertex.t list =
    List.rev
    @@ List.fold
         ~f:(fun acc chain_slice ->
           if ChainSlice.is_voidcall chain_slice then
             (G.LiteralVertex.of_vertex @@ VertexMaker.vertex_of_chain_slice chain_slice) :: acc
           else acc )
         ~init:[] chain_slices


  (** Converts a raw chain-slice list into a G.E.t list, together with an optional info of a
      void-call vertex. *)
  let edge_list_of_chain_slice_list (chain_slices : ChainSlice.t list) :
      (G.E.t list * G.LiteralVertex.t list) list =
    let processed = ChainRefiners.process_chainslices chain_slices in
    let all_void_calls = collect_voidcall_vertices chain_slices in
    let bicycle_chain_of_chain_slices = make_bicycle_chain processed in
    let refined_bicycle_chain, frontend_define_vertex_opt =
      refine_bicycle_chain bicycle_chain_of_chain_slices
    in
    let edge_list =
      refined_bicycle_chain
      >>| fun (cs1, cs2) ->
      ( VertexMaker.vertex_of_chain_slice cs1
      , EdgeLabel.DataFlow
      , VertexMaker.vertex_of_chain_slice cs2 )
    in
    return
      ( edge_list
      , match frontend_define_vertex_opt with
        | Some frontend_define_vertex ->
            frontend_define_vertex :: all_void_calls
        | None ->
            all_void_calls )


  let get_all_edges_and_frontend_defines (raw_json : json) =
    let edge_list_and_frontend_define_vertex_opt_list =
      ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
      >>| ChainSliceManager.chain_slice_list_of_wrapped_chain >>= edge_list_of_chain_slice_list
    in
    ( edge_list_and_frontend_define_vertex_opt_list >>= fst
    , edge_list_and_frontend_define_vertex_opt_list >>= snd )


  let get_all_edges (raw_json : json) : G.E.t list =
    raw_json |> get_all_edges_and_frontend_defines |> fst


  let get_all_frontend_define_vertices (raw_json : json) : G.LiteralVertex.t list =
    raw_json |> get_all_edges_and_frontend_defines |> snd
end

let identify_trunks (graph : G.t) : G.Trunk.t list =
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


let find_ns_cluster (graph : G.t) : G.V.t list list =
  let rec inner (vertex : G.V.t) (acc : G.V.t list) : G.V.t list =
    let all_ns_bidirectionals =
      List.filter
        ~f:(fun other_vertex ->
          G.is_pointing_to_each_other
            (G.LiteralVertex.of_vertex vertex)
            (G.LiteralVertex.of_vertex other_vertex)
            graph ~label:EdgeLabel.NodeWiseSimilarity )
        (G.all_vertices_of_graph graph)
    in
    let vertices_to_explore =
      List.filter
        ~f:(fun vertex -> not @@ List.mem ~equal:Vertex.equal acc vertex)
        all_ns_bidirectionals
    in
    if
      not
      @@ G.is_bidirectional_vertex
           (G.LiteralVertex.of_vertex vertex)
           graph ~label:EdgeLabel.NodeWiseSimilarity
      || List.is_empty vertices_to_explore
    then acc (* we can't recurse anymore *)
    else
      List.fold
        ~f:(fun smol_acc new_vertex -> smol_acc @ inner new_vertex (vertex :: new_vertex :: acc))
        ~init:[] vertices_to_explore
  in
  List.fold
    ~f:(fun acc vertex ->
      if not @@ List.mem ~equal:G.V.equal (List.join acc) vertex then
        let res = inner vertex [] in
        if List.is_empty res then acc else res :: acc
      else acc )
    ~init:[] (G.all_vertices_of_graph graph)


let recursively_find_preds (graph : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) :
    G.V.t list =
  let rec inner (current_vertex : G.V.t) (big_acc : G.V.t list) =
    let current_vertex_df_preds =
      G.get_preds graph (G.LiteralVertex.of_vertex current_vertex) ~label
    in
    let to_explore =
      List.filter current_vertex_df_preds ~f:(fun pred ->
          not @@ List.mem big_acc pred ~equal:Vertex.equal )
    in
    if List.is_empty current_vertex_df_preds || List.is_empty to_explore then big_acc
    else
      List.fold
        ~f:(fun smol_acc vertex -> inner vertex (vertex :: smol_acc))
        ~init:big_acc to_explore
  in
  inner (G.LiteralVertex.to_vertex vertex graph) []
