open ListMonad
open InfixOperators
open GraphRepr
module U = Graph.Persistent.Graph.ConcreteLabeled (Vertex) (EdgeLabel)

let to_undirected (digraph : G.t) : U.t =
  U.empty
  |> (fun graph ->
       List.fold
         ~f:(fun acc vertex -> U.add_vertex acc vertex)
         ~init:graph (G.all_vertices_of_graph digraph) )
  |> fun graph ->
  List.fold ~f:(fun acc edge -> U.add_edge_e acc edge) ~init:graph (G.all_edges_of_graph digraph)


module Weight = struct
  type edge = U.edge

  type t = int

  let weight _ = 1

  let compare = Int.compare

  let add = Int.( + )

  let zero = 0
end

module UPrim = Graph.Prim.Make (U) (Weight)

let run_prim (undigraph : U.t) = UPrim.spanningtree undigraph

let prune_to_mst (digraph : G.t) : G.t =
  let undirected_of_digraph = to_undirected digraph in
  let mst = run_prim undirected_of_digraph in
  G.empty
  |> fun graph ->
  List.fold
    ~f:(fun acc vertex -> G.add_vertex acc vertex)
    ~init:graph (G.all_vertices_of_graph digraph)
  |> fun graph ->
  List.fold
    ~f:(fun acc (v1, label, v2) ->
      G.add_edge_e acc (v1, label, v2) |> fun g -> G.add_edge_e g (v2, label, v1) )
    ~init:graph mst


let diet_edge_list (edgelist : G.E.t list) : G.E.t list =
  let subgraph = List.fold ~f:(fun acc edge -> G.add_edge_e acc edge) ~init:G.empty edgelist in
  let subgraph_pruned = prune_to_mst subgraph in
  G.all_edges_of_graph subgraph_pruned
