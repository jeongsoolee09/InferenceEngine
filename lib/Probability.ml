open GraphRepr
open ListMonad

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
end

module ProbQuadruple = struct
  type t = {src: float; sin: float; san: float; non: float}

  let initial = {src= 0.25; sin= 0.25; san= 0.25; non= 0.25}

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
end

module ProbMap = struct
  module VertexMap = Caml.Map.Make (G.V)
  include VertexMap

  type t = ProbQuadruple.t VertexMap.t (* map from G.V.t to ProbQuadruple.t *)

  let strong_update vertex new_ distmap = remove vertex distmap |> add vertex new_
end

(** make a map initialized with flat 0.25 distribution for every vertex. *)
let make_map_for_graph graph : ProbMap.t =
  let all_vertices =
    G.fold_vertex
      (fun vertex acc -> if List.mem ~equal:G.V.equal acc vertex then acc else vertex :: acc)
      graph []
  in
  List.fold
    ~f:(fun acc vertex -> ProbMap.add vertex ProbQuadruple.initial acc)
    ~init:ProbMap.empty all_vertices


module Utils = struct
  let df_succs (vertex : G.V.t) (graph : G.t) =
    G.fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, label, _) -> EdgeLabel.equal label EdgeLabel.DataFlow)
    >>| trd3


  let ns_succs (vertex : G.V.t) (graph : G.t) =
    G.fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, label, _) -> EdgeLabel.equal label EdgeLabel.NodeWiseSimilarity)
    >>| trd3


  let cs_succs (vertex : G.V.t) (graph : G.t) =
    G.fold_edges_e List.cons graph []
    |> List.filter ~f:(fun (_, label, _) -> EdgeLabel.equal label EdgeLabel.ContextualSimilarity)
    >>| trd3
end
