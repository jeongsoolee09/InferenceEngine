open ListMonad
open InfixOperators
open Yojson.Basic

type json = Yojson.Basic.t

module Set = Caml.Set
module F = Format
module Hashtbl = Caml.Hashtbl

let make_now_string (gmt_diff : int) : string =
  let open CalendarLib in
  let now_raw = Calendar.now () in
  let year = Calendar.year now_raw
  and month = Date.int_of_month @@ Calendar.month now_raw
  and day = Calendar.day_of_month now_raw
  and hour = Calendar.hour now_raw + gmt_diff
  and minute = Calendar.minute now_raw
  and second = Calendar.second now_raw in
  F.asprintf "%d-%d-%d_%d:%d:%d" year month day hour minute second


module TaintLabel = struct
  type t = Source | Sink | Sanitizer | None | Indeterminate [@@deriving equal, compare]

  let of_string : string -> t = function
    | "src" | "\"src\"" | "source" | "Source" ->
        Source
    | "sin" | "\"sin\"" | "sink" | "Sink" ->
        Sink
    | "san" | "\"san\"" | "sanitizer" | "Sanitizer" ->
        Sanitizer
    | "non" | "\"non\"" | "none" | "None" ->
        None
    | otherwise ->
        raise @@ Invalid_argument otherwise


  let to_string : t -> string = function
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


  let to_string_short : t -> string = function
    | Source ->
        "src"
    | Sink ->
        "sin"
    | Sanitizer ->
        "san"
    | None ->
        "non"
    | Indeterminate ->
        "???"


  let is_source (label : t) : bool = equal label Source

  let is_sink (label : t) : bool = equal label Sink

  let is_sanitizer (label : t) : bool = equal label Sanitizer

  let is_none (label : t) : bool = equal label None

  let is_indeterminate (label : t) : bool = equal label Indeterminate

  let string_of_list (labels : t list) : string =
    let acc =
      List.fold labels
        ~f:(fun acc label -> acc ^ F.asprintf "%s, " (to_string_short label))
        ~init:""
    in
    "[" ^ acc ^ "]"
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


  let is_srm (dist : t) : bool =
    match determine_label dist with Source | Sink | Sanitizer -> true | _ -> false


  let is_determined (dist : t) : bool =
    not @@ TaintLabel.equal (determine_label dist) TaintLabel.Indeterminate


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

  let dummy2 = "{ line -2 }"

  let to_int_list (locset : t) : int list =
    let regex = Re2.create_exn "[0-9-]+" in
    let all_matches = Re2.find_all_exn regex locset in
    List.map all_matches ~f:int_of_string


  let of_int_list (ints : int list) =
    let int_strs =
      List.stable_dedup ints |> List.map ~f:string_of_int |> List.map ~f:(fun str -> "line " ^ str)
    in
    List.intersperse int_strs ~sep:", " |> String.concat |> fun out -> "{ " ^ out ^ " }"


  let earliest_location (locset : t) : int =
    List.hd_exn @@ List.sort (to_int_list locset) ~compare:Int.compare
end

module Vertex = struct
  (* type t = Method.t * LocationSet.t * ProbQuadruple.t [@@deriving compare] *)
  type t = {method_: Method.t; locset: LocationSet.t; dist: ProbQuadruple.t; depth: int}
  [@@deriving compare]

  let hash = Hashtbl.hash

  let equal (v1 : t) (v2 : t) : bool =
    (* we ignore the quadruple in defining the identity: that's just an attribute *)
    Method.equal v1.method_ v2.method_ && LocationSet.equal v1.locset v2.locset


  let make_initial (meth : Method.t) (loc : String.t) : t =
    {method_= meth; locset= loc; dist= ProbQuadruple.initial; depth= -1}


  let make (meth : Method.t) (loc : String.t) (dist : ProbQuadruple.t) : t =
    {method_= meth; locset= loc; dist; depth= -1}


  let get_method (v : t) : Method.t = v.method_

  let get_loc (v : t) : LocationSet.t = v.locset

  let get_dist (v : t) : ProbQuadruple.t = v.dist

  let get_depth (v : t) : int = v.depth

  let to_string (v : t) : string =
    F.asprintf "(\"%s\", \"%s\")" (Method.to_string v.method_) (LocationSet.to_string v.locset)


  let vertex_list_to_string (lst : t list) : string =
    let accumed = List.fold ~f:(fun acc vertex -> acc ^ to_string vertex ^ "; ") ~init:"" lst in
    F.asprintf "[%s]" accumed


  let dummy = (Method.dummy, LocationSet.dummy, ProbQuadruple.initial)
end

module EdgeLabel = struct
  type t = DataFlow | NodeWiseSimilarity | ContextualSimilarity [@@deriving equal, compare]

  let default : t = DataFlow (* beware when using add_edge, since its label defaults to DataFlow! *)

  let is_df = function DataFlow -> true | _ -> false

  let is_ns = function NodeWiseSimilarity -> true | _ -> false

  let is_cs = function ContextualSimilarity -> true | _ -> false
end

module VertexPair = struct
  type t = Vertex.t * Vertex.t [@@deriving equal, compare]
end

module BiDiGraph = struct
  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (EdgeLabel)

  let compare bdg1 bdg2 =
    let bdg1_num_edges = nb_edges bdg1 and bdg2_num_edges = nb_edges bdg2 in
    Int.compare bdg1_num_edges bdg2_num_edges
end

module G = struct
  type t = {graph: BiDiGraph.t; comp_unit: String.t; label: String.t; desc: String.t}
  [@@deriving compare]

  (* ==================== Really Boring Wrappers ==================== *)

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

  let fold_pred_e f g v init = BiDiGraph.fold_pred_e f g.graph v init

  let empty = {graph= BiDiGraph.empty; label= ""; desc= ""; comp_unit= ""}

  let add_vertex g v = {g with graph= BiDiGraph.add_vertex g.graph v}

  let remove_vertex g v = {g with graph= BiDiGraph.remove_vertex g.graph v}

  let add_edge g v1 v2 = {g with graph= BiDiGraph.add_edge g.graph v1 v2}

  let add_edge_e g e = {g with graph= BiDiGraph.add_edge_e g.graph e}

  let remove_edge g v1 v2 = {g with graph= BiDiGraph.remove_edge g.graph v1 v2}

  let remove_edge_e g e = {g with graph= BiDiGraph.remove_edge_e g.graph e}

  (* ====================================================================== *)

  module MemoMap = struct
    module WithGraphDomain = Caml.Map.Make (BiDiGraph)
    include WithGraphDomain

    type t = Int.t WithGraphDomain.t
  end

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

    let get_method = fst

    let get_loc = snd

    let to_vertex_inner ((meth, loc) : t) (graph : BiDiGraph.t) : Vertex.t =
      let res_opt =
        BiDiGraph.fold_vertex
          (fun vertex acc ->
            if
              Method.equal meth (Vertex.get_method vertex)
              && LocationSet.equal loc (Vertex.get_loc vertex)
            then Some vertex
            else acc )
          graph None
      in
      match res_opt with
      | Some vertex ->
          vertex
      | None ->
          failwithf "could not find dist for (\"%s\", \"%s\")" (Method.to_string meth)
            (LocationSet.to_string loc) ()


    let to_vertex =
      let cache = Hashtbl.create 777 in
      fun ((meth, loc) : t) (graph : BiDiGraph.t) : Vertex.t ->
        match Hashtbl.find_opt cache (meth, loc) with
        | None ->
            to_vertex_inner (meth, loc) graph
        | Some res ->
            res


    let to_vertex_cheap ((meth, loc) : t) : Vertex.t =
      {method_= meth; locset= loc; dist= ProbQuadruple.initial; depth= -1}


    let to_string ((meth, loc) : t) : string =
      F.asprintf "(\"%s\", \"%s\")" (Method.to_string meth) (LocationSet.to_string loc)


    let of_vertex (v : Vertex.t) : t = (Vertex.get_method v, Vertex.get_loc v)

    let of_string (string : String.t) : t =
      try
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
      with _ -> failwithf "of_string failed on %s" string ()
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
        (fun vertex acc ->
          if
            Method.equal meth (Vertex.get_method vertex)
            && LocationSet.equal loc (Vertex.get_loc vertex)
          then Some (Vertex.get_dist vertex)
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
      fold_vertex
        (fun vertex acc -> (Vertex.get_method vertex, Vertex.get_loc vertex) :: acc)
        prev_snapshot []
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
      fold_vertex
        (fun vertex acc -> (Vertex.get_method vertex, Vertex.get_loc vertex) :: acc)
        prev_snapshot []
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


  (* ============ Graphviz stuffs ============ *)

  let graph_attributes g = [`Label g.label]

  let default_vertex_attributes _ = []

  let vertex_name (vertex : V.t) : string =
    F.asprintf "\"(%s, %s)\""
      (Method.to_string (Vertex.get_method vertex))
      (LocationSet.to_string (Vertex.get_loc vertex))


  let vertex_attributes (vertex : V.t) =
    match ProbQuadruple.determine_label (Vertex.get_dist vertex) with
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

  (* ============ supported computations ============ *)

  let serialize_to_bin ?(suffix = "") (graph : t) : unit =
    let out_chan =
      if String.is_empty suffix then
        Out_channel.create (F.asprintf "%s_%s.bin" (make_now_string 9) graph.comp_unit)
      else Out_channel.create (F.asprintf "%s_%s_%s.bin" (make_now_string 9) graph.comp_unit suffix)
    in
    Out_channel.set_binary_mode out_chan true ;
    Marshal.to_channel out_chan graph [] ;
    Out_channel.close out_chan


  let strong_update_dist (target_vertex : V.t) (new_dist : ProbQuadruple.t) (graph : t) : t =
    map_vertex
      (fun vertex ->
        if Vertex.equal vertex target_vertex then {vertex with dist= new_dist} else vertex )
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
      (fun ((_, _, v2) as edge) acc ->
        if V.equal (LiteralVertex.to_vertex vertex g.graph) v2 then edge :: acc else acc )
      g []


  let get_preds_any (g : t) (vertex : LiteralVertex.t) : Vertex.t list =
    fold_pred List.cons g (LiteralVertex.to_vertex vertex g.graph) []


  let get_succs_any (g : t) (vertex : LiteralVertex.t) : Vertex.t list =
    fold_succ List.cons g (LiteralVertex.to_vertex vertex g.graph) []


  let in_degree_label (g : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : int =
    let this_vertex_df_edges =
      fold_edges_e
        (fun ((_, label_, v2) as edge) acc ->
          if EdgeLabel.equal label label_ && LiteralVertex.equal vertex (LiteralVertex.of_vertex v2)
          then edge :: acc
          else acc )
        g []
    in
    List.length this_vertex_df_edges


  let out_degree_label (g : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : int =
    let this_vertex_df_edges =
      fold_edges_e
        (fun ((v1, label_, _) as edge) acc ->
          if EdgeLabel.equal label label_ && LiteralVertex.equal vertex (LiteralVertex.of_vertex v1)
          then edge :: acc
          else acc )
        g []
    in
    List.length this_vertex_df_edges


  let is_df_root (graph : t) (vertex : LiteralVertex.t) : bool =
    Int.equal (in_degree_label graph vertex ~label:DataFlow) 0
    && Int.( > ) (out_degree_label graph vertex ~label:DataFlow) 0


  let is_df_leaf (graph : t) (vertex : LiteralVertex.t) : bool =
    Int.equal (out_degree_label graph vertex ~label:DataFlow) 0
    && Int.( > ) (in_degree_label graph vertex ~label:DataFlow) 0


  let is_df_internal (graph : t) (vertex : LiteralVertex.t) : bool =
    Int.( > ) (out_degree_label graph vertex ~label:DataFlow) 0
    && Int.( > ) (in_degree_label graph vertex ~label:DataFlow) 0


  let collect_df_roots (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if is_df_root graph (LiteralVertex.of_vertex vertex) then vertex :: acc else acc )
      graph []


  let collect_df_leaves (graph : t) : V.t list =
    fold_vertex
      (fun vertex acc ->
        if is_df_leaf graph (LiteralVertex.of_vertex vertex) then vertex :: acc else acc )
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
    fold_edges_e
      (fun (v1, target_label, v2) acc ->
        if
          Vertex.equal v1 (LiteralVertex.to_vertex vertex g.graph)
          && EdgeLabel.equal target_label label
        then v2 :: acc
        else acc )
      g []


  let get_preds (g : t) (vertex : LiteralVertex.t) ~(label : EdgeLabel.t) : V.t list =
    fold_edges_e
      (fun (v1, target_label, v2) acc ->
        if
          Vertex.equal v2 (LiteralVertex.to_vertex vertex g.graph)
          && EdgeLabel.equal target_label label
        then v1 :: acc
        else acc )
      g []


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


  let this_method_vertices_array (graph : t) (method_ : Method.t) : V.t array =
    Array.of_list
    @@ fold_vertex
         (fun vertex acc ->
           if Method.equal method_ (Vertex.get_method vertex) then vertex :: acc else acc )
         graph []


  let leave_only_df_edges (graph : t) : t =
    fold_edges_e
      (fun ((_, label, _) as edge) acc ->
        try if not @@ EdgeLabel.equal EdgeLabel.DataFlow label then remove_edge_e acc edge else acc
        with Invalid_argument _ -> acc )
      graph graph


  let leave_only_ns_edges (graph : t) : t =
    empty
    |> fun g ->
    fold_vertex (fun vertex acc -> add_vertex acc vertex) graph g
    |> fun g ->
    List.fold
      ~f:(fun acc edge -> add_edge_e acc edge)
      (get_edges graph ~label:EdgeLabel.NodeWiseSimilarity)
      ~init:g


  let leave_only_cs_edges (graph : t) : t =
    empty
    |> fun g ->
    fold_vertex (fun vertex acc -> add_vertex acc vertex) graph g
    |> fun g ->
    List.fold
      ~f:(fun acc edge -> add_edge_e acc edge)
      (get_edges graph ~label:EdgeLabel.ContextualSimilarity)
      ~init:g


  let all_vertices_of_graph (graph : t) =
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


  let all_udf_vertices_of_graph (graph : t) : V.t list =
    List.filter ~f:(Method.is_udf << Vertex.get_method) (all_vertices_of_graph graph)


  let all_api_vertices_of_graph (graph : t) : V.t list =
    List.filter ~f:(Method.is_api << Vertex.get_method) (all_vertices_of_graph graph)


  let all_methods_of_graph =
    let cache = ref MemoMap.empty in
    fun (graph : t) : Method.t list ->
      match MemoMap.find_opt graph.graph !cache with
      | None ->
          let out =
            let all_vertices = all_vertices_of_graph graph in
            let module MethodSet = Caml.Set.Make (Method) in
            all_vertices >>| Vertex.get_method |> MethodSet.of_list |> MethodSet.elements
          in
          cache := MemoMap.add graph.graph out !cache ;
          out
      | Some res ->
          res


  let all_non_frontend_methods_of_graph (graph : t) : Method.t list =
    all_methods_of_graph graph
    |> List.filter ~f:(not << String.is_substring ~substring:"$Lambda$" << Method.to_string)
    |> List.filter ~f:(not << String.is_substring ~substring:"lambda$" << Method.to_string)
    |> List.filter ~f:(not << String.is_prefix ~prefix:"__" << Method.to_string)


  let all_edges_of_graph (graph : t) = fold_edges_e List.cons graph []

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


  let merge (lhs : t) (rhs : t) : t =
    let big_acc = {empty with comp_unit= lhs.comp_unit ^ "_and_" ^ rhs.comp_unit} in
    big_acc
    |> (fun g -> List.fold (all_vertices_of_graph lhs) ~f:add_vertex ~init:g)
    |> (fun g -> List.fold (all_edges_of_graph lhs) ~f:add_edge_e ~init:g)
    |> (fun g -> List.fold (all_vertices_of_graph rhs) ~f:add_vertex ~init:g)
    |> fun g -> List.fold (all_edges_of_graph rhs) ~f:add_edge_e ~init:g


  let dist_is_all_flat (graph : t) : bool =
    fold_vertex
      (fun vertex acc -> ProbQuadruple.equal (Vertex.get_dist vertex) ProbQuadruple.initial && acc)
      graph true


  let delete_all_bidirectional_vertices (graph : t) : t =
    fold_vertex
      (fun vertex acc ->
        if is_bidirectional_vertex (LiteralVertex.of_vertex vertex) acc ~label:DataFlow then
          remove_vertex acc vertex
        else acc )
      graph graph


  let snapshot_to_json (snapshot : t) : unit =
    let vertices = all_vertices_of_graph snapshot in
    let vertices_and_labels =
      List.map
        ~f:(fun vertex ->
          let label = ProbQuadruple.determine_label (Vertex.get_dist vertex) in
          (vertex, label) )
        vertices
    in
    let vertex_strs_and_label_strs =
      List.map
        ~f:(fun (vertex, label) -> (Vertex.to_string vertex, TaintLabel.to_string_short label))
        vertices_and_labels
    in
    let json_repr =
      `Assoc
        (List.map
           ~f:(fun (vertex_str, label_str) -> (vertex_str, `String label_str))
           vertex_strs_and_label_strs )
    in
    let json_filename = F.asprintf "%s_labels.json" @@ make_now_string 9 in
    let out_channel = Out_channel.create json_filename in
    pretty_to_channel out_channel json_repr ;
    Out_channel.close out_channel


  let fold_edges_e_label f g init ~label =
    let this_label_edges_only =
      List.filter (all_edges_of_graph g) ~f:(EdgeLabel.equal label << snd3)
    in
    List.fold ~f ~init this_label_edges_only


  let iter_edges_e_label f g ~label =
    let this_label_edges_only =
      List.filter (all_edges_of_graph g) ~f:(EdgeLabel.equal label << snd3)
    in
    List.iter ~f this_label_edges_only
end

module Dot = Graph.Graphviz.Dot (G)

let get_recursive_preds (g : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) :
    (G.V.t * int) list =
  let rec inner (current_vertex : G.V.t) (big_acc : (G.V.t * int) list) (count : int) =
    let current_vertex_df_preds = G.get_preds g (G.LiteralVertex.of_vertex current_vertex) ~label in
    let to_explore =
      List.filter current_vertex_df_preds ~f:(fun pred ->
          not @@ List.mem (big_acc >>| fst) pred ~equal:Vertex.equal )
    in
    if List.is_empty current_vertex_df_preds || List.is_empty to_explore then big_acc
    else
      List.fold
        ~f:(fun smol_acc vertex -> inner vertex ((vertex, count) :: smol_acc) (count + 1))
        ~init:big_acc to_explore
  in
  inner (G.LiteralVertex.to_vertex vertex g.graph) [] 1


let get_recursive_succs (g : G.t) (vertex : G.LiteralVertex.t) ~(label : EdgeLabel.t) :
    (G.V.t * int) list =
  let rec inner (current_vertex : G.V.t) (big_acc : (G.V.t * int) list) (count : int) =
    let current_vertex_df_succs = G.get_succs g (G.LiteralVertex.of_vertex current_vertex) ~label in
    let to_explore =
      List.filter current_vertex_df_succs ~f:(fun succ ->
          not @@ List.mem (big_acc >>| fst) succ ~equal:Vertex.equal )
    in
    if List.is_empty current_vertex_df_succs || List.is_empty to_explore then big_acc
    else
      List.fold
        ~f:(fun smol_acc vertex -> inner vertex ((vertex, count) :: smol_acc) (count + 1))
        ~init:big_acc to_explore
  in
  inner (G.LiteralVertex.to_vertex vertex g.graph) [] 1


let vertex_list_to_method_list (vertex_list : Vertex.t list) : Method.t list =
  List.stable_dedup @@ List.map ~f:Vertex.get_method vertex_list
