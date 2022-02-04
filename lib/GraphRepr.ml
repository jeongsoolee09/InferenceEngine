open ListMonad
open InfixOperators
open Yojson.Basic

type json = Yojson.Basic.t

module Set = Caml.Set
module F = Format

let make_now_string (gmt_diff : int) : string =
  let open CalendarLib in
  let now_raw = Calendar.now () in
  let year = Calendar.year now_raw in
  let month = Date.int_of_month @@ Calendar.month now_raw in
  let day = Calendar.day_of_month now_raw in
  let hour = Calendar.hour now_raw + gmt_diff in
  let minute = Calendar.minute now_raw in
  let second = Calendar.second now_raw in
  F.asprintf "%d-%d-%d_%d:%d:%d" year month day hour minute second


module TaintLabel = struct
  type t = Source | Sink | Sanitizer | None | Indeterminate [@@deriving equal, compare]

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
      List.sort alist ~compare:(fun (_, v1) (_, v2) -> -Float.compare v1 v2)
    in
    let label, v1 = List.nth_exn sorted_decreasing 0 in
    let _, v2 = List.nth_exn sorted_decreasing 1 in
    if Float.( >= ) (Float.( - ) v1 v2) winning_threshold then label_of_string label
    else Indeterminate


  let is_source (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Source

  let is_sin (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Sink

  let is_sanitizer (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.Sanitizer

  let is_none (dist : t) : bool = TaintLabel.equal (determine_label dist) TaintLabel.None

  let is_indeterminate (dist : t) : bool =
    TaintLabel.equal (determine_label dist) TaintLabel.Indeterminate


  let dist_is_flat (dist : t) : bool = equal dist initial
end

module LocationSet = struct
  type t = String.t [@@deriving compare, equal, sexp]

  let of_string (string : String.t) : t =
    (* this doesn't have to be so stringent, does it? Let's not use regexp! *)
    let starts_with_open_curly_brace = String.is_prefix ~prefix:"{" string
    and ends_with_closing_curly_brace = String.is_suffix ~suffix:"}" string
    and has_line_as_substring = String.is_substring ~substring:"line" string in
    assert (starts_with_open_curly_brace && ends_with_closing_curly_brace && has_line_as_substring) ;
    string


  let to_string : t -> string = ident

  let dummy = "{ line -1 }"
end

module Vertex = struct
  type t = Method.t * LocationSet.t * ProbQuadruple.t [@@deriving compare]

  let hash = Hashtbl.hash

  let equal ((meth1, locset1, _) : t) ((meth2, locset2, _) : t) : bool =
    (* we ignore the quadruple in defining the identity: that's just an attribute *)
    Method.equal meth1 meth2 && LocationSet.equal locset1 locset2


  let get_method (meth, _, _) : Method.t = meth

  let get_loc (_, loc, _) : LocationSet.t = loc

  let get_dist (_, _, dist) : ProbQuadruple.t = dist

  let to_string ((procstring, locstring, _) : t) : string =
    F.asprintf "(\"%s\", \"%s\")" (Method.to_string procstring) (LocationSet.to_string locstring)


  let vertex_list_to_string (lst : t list) : string =
    let accumed = List.fold ~f:(fun acc vertex -> acc ^ to_string vertex ^ "; ") ~init:"" lst in
    F.asprintf "[%s]" accumed


  let dummy = (Method.dummy, LocationSet.dummy, ProbQuadruple.initial)
end

module EdgeLabel = struct
  type t = DataFlow | NodeWiseSimilarity | ContextualSimilarity [@@deriving equal, compare]

  let default : t = DataFlow (* beware when using add_edge, since its label defaults to DataFlow! *)
end

module VertexPair = struct
  type t = Vertex.t * Vertex.t [@@deriving equal, compare]
end

module BiDiGraph = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (EdgeLabel)

module G = struct
  type t = {graph: BiDiGraph.t; comp_unit: String.t; label: String.t; desc: String.t}

  (* ==================== Really Boring Wrapping Logic ==================== *)

  module V = BiDiGraph.V
  module E = BiDiGraph.E

  let edge_equal ((v11, l1, v12) : E.t) ((v21, l2, v22) : E.t) : bool =
    V.equal v11 v21 && EdgeLabel.equal l1 l2 && V.equal v12 v22


  let is_directed = BiDiGraph.is_directed

  let is_empty g = BiDiGraph.is_empty g.graph

  let nb_vertex g = BiDiGraph.nb_vertex g.graph

  let nb_edges g = BiDiGraph.nb_edges g.graph

  let out_degree g = BiDiGraph.out_degree g.graph

  let in_degree g = BiDiGraph.in_degree g.graph

  let mem_vertex g v = BiDiGraph.mem_vertex g.graph v

  let mem_edge g v1 v2 = BiDiGraph.mem_edge g.graph v1 v2

  let mem_edge_e g e = BiDiGraph.mem_edge_e g.graph e

  let find_edge g v1 v2 = BiDiGraph.find_edge g.graph v1 v2

  let find_all_edges g v1 v2 = BiDiGraph.find_all_edges g.graph v1 v2

  let succ g v = BiDiGraph.succ g.graph v

  let pred g v = BiDiGraph.pred g.graph v

  let succ_e g v = BiDiGraph.succ_e g.graph v

  let pred_e g v = BiDiGraph.pred_e g.graph v

  let iter_vertex f g = BiDiGraph.iter_vertex f g.graph

  let fold_vertex f g init = BiDiGraph.fold_vertex f g.graph init

  let iter_edges f g = BiDiGraph.iter_edges f g.graph

  let fold_edges f g init = BiDiGraph.fold_edges f g.graph init

  let iter_edges_e f g = BiDiGraph.iter_edges_e f g.graph

  let fold_edges_e f g init = BiDiGraph.fold_edges_e f g.graph init

  let map_vertex f g = {g with graph= BiDiGraph.map_vertex f g.graph}

  let iter_succ f g v = BiDiGraph.iter_succ f g.graph v

  let iter_pred f g v = BiDiGraph.iter_pred f g.graph v

  let fold_succ f g v init = BiDiGraph.fold_succ f g.graph v init

  let fold_pred f g v init = BiDiGraph.fold_pred f g.graph v init

  let iter_succ_e f g v = BiDiGraph.iter_succ_e f g.graph v

  let fold_succ_e f g v init = BiDiGraph.fold_succ_e f g.graph v init

  let iter_pred_e f g v = BiDiGraph.iter_pred_e f g.graph v

  let fold_pred_e f g v init = BiDiGraph.fold_pred_e f g.graph v

  let empty = {graph= BiDiGraph.empty; label= ""; desc= ""; comp_unit= ""}

  let add_vertex g v = {g with graph= BiDiGraph.add_vertex g.graph v}

  let remove_vertex g v = {g with graph= BiDiGraph.remove_vertex g.graph v}

  let add_edge g v1 v2 = {g with graph= BiDiGraph.add_edge g.graph v1 v2}

  let add_edge_e g e = {g with graph= BiDiGraph.add_edge_e g.graph e}

  let remove_edge g v1 v2 = {g with graph= BiDiGraph.remove_edge g.graph v1 v2}

  let remove_edge_e g e = {g with graph= BiDiGraph.remove_edge_e g.graph e}

  (* ====================================================================== *)

  let equal (snapshot1 : t) (snapshot2 : t) : bool =
    (* G1 = G2 <=> V1 = V2 && E1 = E2 *)
    let module VertexSet = Caml.Set.Make (V) in
    let module EdgeSet = Caml.Set.Make (E) in
    let snapshot1_vertices = VertexSet.of_list @@ fold_vertex List.cons snapshot1 []
    and snapshot2_vertices = VertexSet.of_list @@ fold_vertex List.cons snapshot2 []
    and snapshot1_edges = EdgeSet.of_list @@ fold_edges_e List.cons snapshot1 []
    and snapshot2_edges = EdgeSet.of_list @@ fold_edges_e List.cons snapshot2 [] in
    VertexSet.equal snapshot1_vertices snapshot2_vertices
    && EdgeSet.equal snapshot1_edges snapshot2_edges


  module LiteralVertex = struct
    type t = Method.t * LocationSet.t [@@deriving compare, equal, sexp]

    let to_vertex ((meth, loc) : t) (graph : BiDiGraph.t) : Vertex.t =
      let res_opt =
        BiDiGraph.fold_vertex
          (fun ((target_meth, target_loc, dist) as vertex) acc ->
            if Method.equal meth target_meth && LocationSet.equal loc target_loc then Some vertex
            else acc )
          graph None
      in
      match res_opt with
      | Some vertex ->
          vertex
      | None ->
          failwithf "could not find dist for (\"%s\", \"%s\")" (Method.to_string meth)
            (LocationSet.to_string loc) ()


    let to_vertex_cheap ((meth, loc) : t) : Vertex.t = (meth, loc, ProbQuadruple.initial)

    let to_string ((meth, loc) : t) : string =
      F.asprintf "(\"%s\", \"%s\")" (Method.to_string meth) (LocationSet.to_string loc)


    let of_vertex ((str1, str2, _) : Vertex.t) : t = (str1, str2)

    let of_string (string : String.t) : t =
      let regex = Re2.create_exn "('(.*)', '(.*)')" in
      let method_, locset =
        match
          ((Re2.find_submatches_exn regex string).(2), (Re2.find_submatches_exn regex string).(3))
        with
        | Some str1, Some str2 ->
            (str1, str2)
        | _, _ ->
            failwithf "converting of_string failed: %s" string ()
      in
      (method_, locset)
  end

  module Saturation = struct
    let saturated_parameter = 0.2 (* TEMP: subject to change *)

    let dist_is_saturated (quad : ProbQuadruple.t) : bool =
      let sorted =
        List.sort
          ~compare:(fun p1 p2 -> -Float.compare p1 p2)
          [quad.src; quad.sin; quad.san; quad.non]
      in
      let first = List.nth_exn sorted 0 and second = List.nth_exn sorted 1 in
      Float.( >= ) (first -. second) saturated_parameter


    let all_dists_in_graph_are_saturated (graph : t) : bool =
      fold_vertex
        (fun vertex acc ->
          (* TEMP: the vertex should not be isolated *)
          if Int.( = ) (out_degree graph vertex) 0 && Int.( = ) (in_degree graph vertex) 0 then acc
          else
            let dist = Vertex.get_dist vertex in
            dist_is_saturated dist && acc )
        graph true
  end

  let lookup_dist_for_meth_and_loc (meth : Method.t) (loc : LocationSet.t) (graph : t) :
      ProbQuadruple.t =
    let res_opt =
      fold_vertex
        (fun (target_meth, target_loc, dist) acc ->
          if Method.equal meth target_meth && LocationSet.equal loc target_loc then Some dist
          else acc )
        graph None
    in
    match res_opt with
    | Some dist ->
        dist
    | None ->
        failwithf "could not find dist for (\"%s\", \"%s\")" (Method.to_string meth)
          (LocationSet.to_string loc) ()


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
        @@ F.asprintf "Vertex (%s, %s)'s dist was updated from %s to %s" (Method.to_string meth)
             (LocationSet.to_string loc)
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
        @@ F.asprintf "Vertex (%s, %s)'s label was updated from %s to %s" (Method.to_string meth)
             (LocationSet.to_string loc) (TaintLabel.to_string prev_label)
             (TaintLabel.to_string next_label) ;
        Out_channel.newline Out_channel.stdout ;
        Out_channel.newline Out_channel.stdout )
      diff


  let graph_attributes g = [`Label g.label]

  let default_vertex_attributes _ = []

  let vertex_name ((meth, locset, _) : V.t) : string =
    F.asprintf "\"(%s, %s)\"" (Method.to_string meth) (LocationSet.to_string locset)


  let vertex_attributes ((meth, locset, dist) : V.t) =
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

  let serialize_to_bin ?(suffix = "") (graph : t) : unit =
    let out_chan =
      Out_channel.create (F.asprintf "%s_%s_%s.bin" (make_now_string 9) graph.comp_unit suffix)
    in
    Out_channel.set_binary_mode out_chan true ;
    Marshal.to_channel out_chan graph [] ;
    Out_channel.close out_chan


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


  let is_root (vertex : LiteralVertex.t) (g : t) : bool =
    Int.equal (in_degree g (LiteralVertex.to_vertex vertex g.graph)) 0


  let is_leaf (vertex : LiteralVertex.t) (g : t) : bool =
    Int.equal (out_degree g (LiteralVertex.to_vertex vertex g.graph)) 0


  let any_label_preds (vertex : LiteralVertex.t) (g : t) =
    fold_edges
      (fun v1 v2 acc ->
        if Vertex.equal v2 (LiteralVertex.to_vertex vertex g.graph) then v1 :: acc else acc )
      g []


  let any_label_succs (vertex : LiteralVertex.t) (g : t) =
    fold_edges
      (fun v1 v2 acc ->
        if Vertex.equal v1 (LiteralVertex.to_vertex vertex g.graph) then v2 :: acc else acc )
      g []


  let find_in_edges (vertex : LiteralVertex.t) (g : t) : E.t list =
    fold_edges_e
      (fun ((_, label, v2) as edge) acc ->
        if V.equal (LiteralVertex.to_vertex vertex g.graph) v2 then edge :: acc else acc )
      g []


  let get_preds_any (vertex : LiteralVertex.t) (g : t) : Vertex.t list =
    fold_pred List.cons g (LiteralVertex.to_vertex vertex g.graph) []


  let get_succs_any (vertex : LiteralVertex.t) (g : t) : Vertex.t list =
    fold_succ List.cons g (LiteralVertex.to_vertex vertex g.graph) []


  let is_df_root (vertex : LiteralVertex.t) (df_only_graph : t) : bool =
    try
      Int.equal (in_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
      && Int.( > ) (out_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
    with _ -> failwith @@ F.asprintf "is_df_root failed for %s" @@ LiteralVertex.to_string vertex


  let is_df_leaf (vertex : LiteralVertex.t) (df_only_graph : t) : bool =
    try
      Int.equal (out_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
      && Int.( > ) (in_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
    with _ -> failwith @@ F.asprintf "is_df_leaf failed for %s" @@ LiteralVertex.to_string vertex


  let is_df_internal (vertex : LiteralVertex.t) (df_only_graph : t) : bool =
    try
      Int.( > ) (out_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
      && Int.( > ) (in_degree df_only_graph (LiteralVertex.to_vertex vertex df_only_graph.graph)) 0
    with _ ->
      failwith @@ F.asprintf "is_df_internal failed for %s" @@ LiteralVertex.to_string vertex


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


  let get_unmarked_vertices (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
      graph []


  let get_unmarked_udfs (graph : t) : Method.t list =
    get_unmarked_vertices graph >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_udf


  let get_unmarked_apis (graph : t) : Method.t list =
    get_unmarked_vertices graph >>| Vertex.get_method |> List.stable_dedup
    |> List.filter ~f:Method.is_api
    |> List.filter ~f:(not << Method.is_frontend)


  let get_edges (graph : t) ~(label : EdgeLabel.t) : E.t list =
    fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, target_label, _) -> EdgeLabel.equal label target_label)


  let get_succs (g : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : V.t list =
    let out_df_edges =
      fold_edges_e
        (fun ((v1, target_label, _) as edge) acc ->
          if
            Vertex.equal v1 (LiteralVertex.to_vertex vertex g.graph)
            && EdgeLabel.equal target_label label
          then edge :: acc
          else acc )
        g []
    in
    out_df_edges >>| trd3


  let get_preds (g : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : V.t list =
    let in_df_edges =
      fold_edges_e
        (fun ((_, target_label, v2) as edge) acc ->
          if
            Vertex.equal v2 (LiteralVertex.to_vertex vertex g.graph)
            && EdgeLabel.equal target_label label
          then edge :: acc
          else acc )
        g []
    in
    in_df_edges >>| fst3


  let is_pointing_to_each_other (v1 : LiteralVertex.t) (v2 : LiteralVertex.t) (g : t)
      ~(label : EdgeLabel.t) : bool =
    let v2_is_v1's_succ =
      List.mem ~equal:Vertex.equal (get_succs g v1 ~label) (LiteralVertex.to_vertex v2 g.graph)
    and v1_is_v2's_succ =
      List.mem ~equal:Vertex.equal (get_succs g v2 ~label) (LiteralVertex.to_vertex v1 g.graph)
    in
    v2_is_v1's_succ && v1_is_v2's_succ


  let this_method_vertices (graph : t) (method_ : Method.t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if Method.equal method_ (Vertex.get_method vertex) then vertex :: acc else acc )
      graph []


  let leave_only_df_edges (graph : t) : t =
    fold_edges_e
      (fun ((_, label, _) as edge) acc ->
        if not @@ EdgeLabel.equal EdgeLabel.DataFlow label then remove_edge_e acc edge else acc )
      graph graph


  let all_vertices_of_graph (graph : t) : V.t list =
    let all_vertices_with_dup = fold_vertex List.cons graph [] in
    let module VertexSet = Caml.Set.Make (V) in
    all_vertices_with_dup |> VertexSet.of_list |> VertexSet.elements


  let all_non_frontend_vertices_of_graph (graph : t) : V.t list =
    all_vertices_of_graph graph
    |> List.filter ~f:(fun vertex ->
           not
           @@ String.is_substring
                (vertex |> Vertex.get_method |> Method.to_string)
                ~substring:"$Lambda$" )
    |> List.filter ~f:(fun vertex ->
           not
           @@ String.is_substring
                (vertex |> Vertex.get_method |> Method.to_string)
                ~substring:"lambda$" )
    |> List.filter ~f:(fun vertex ->
           not @@ String.is_prefix (vertex |> Vertex.get_method |> Method.to_string) ~prefix:"__" )


  let all_methods_of_graph (graph : t) : Method.t list =
    let all_vertices = all_vertices_of_graph graph in
    let module MethodSet = Caml.Set.Make (Method) in
    all_vertices >>| Vertex.get_method |> MethodSet.of_list |> MethodSet.elements


  let all_non_frontend_methods_of_graph (graph : t) : Method.t list =
    all_methods_of_graph graph
    |> List.filter ~f:(not << String.is_substring ~substring:"$Lambda$" << Method.to_string)
    |> List.filter ~f:(not << String.is_substring ~substring:"lambda$" << Method.to_string)
    |> List.filter ~f:(not << String.is_prefix ~prefix:"__" << Method.to_string)


  let all_edges_of_graph (graph : t) : E.t list = fold_edges_e List.cons graph []

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


  let dist_is_all_flat (graph : t) : bool =
    fold_vertex
      (fun vertex acc -> ProbQuadruple.equal (Vertex.get_dist vertex) ProbQuadruple.initial)
      graph true


  let delete_all_bidirectional_vertices (graph : t) : t =
    fold_vertex
      (fun vertex acc ->
        if is_bidirectional_vertex (LiteralVertex.of_vertex vertex) acc ~label:DataFlow then
          remove_vertex acc vertex
        else acc )
      graph graph
end

module Dot = Graph.Graphviz.Dot (G)

let all_ns_clusters (graph : G.t) : G.V.t list list =
  let module Hashtbl = Caml.Hashtbl in
  let cache = Hashtbl.create 777 in
  match Hashtbl.find_opt cache graph.label with
  | None ->
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
      let out =
        List.fold
          ~f:(fun acc vertex ->
            if not @@ List.mem ~equal:G.V.equal (List.join acc) vertex then
              let res = inner vertex [] in
              if List.is_empty res then acc else res :: acc
            else acc )
          ~init:[] (G.all_vertices_of_graph graph)
        >>| List.stable_dedup
      in
      Hashtbl.add cache graph.label out ;
      out
  | Some memoized_result ->
      memoized_result


let get_recursive_preds (g : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) : G.V.t list =
  let rec inner (current_vertex : G.V.t) (big_acc : G.V.t list) =
    let current_vertex_df_preds = G.get_preds g (G.LiteralVertex.of_vertex current_vertex) ~label in
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
  inner (G.LiteralVertex.to_vertex vertex g.graph) []


let get_recursive_succs (g : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) : G.V.t list =
  let rec inner (current_vertex : G.V.t) (big_acc : G.V.t list) =
    let current_vertex_df_succs = G.get_succs g (G.LiteralVertex.of_vertex current_vertex) ~label in
    let to_explore =
      List.filter current_vertex_df_succs ~f:(fun succ ->
          not @@ List.mem big_acc succ ~equal:Vertex.equal )
    in
    if List.is_empty current_vertex_df_succs || List.is_empty to_explore then big_acc
    else
      List.fold
        ~f:(fun smol_acc vertex -> inner vertex (vertex :: smol_acc))
        ~init:big_acc to_explore
  in
  inner (G.LiteralVertex.to_vertex vertex g.graph) []
