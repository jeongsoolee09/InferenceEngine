open ListMonad
open InfixOperators
open GraphRepr
module U = Graph.Persistent.Graph.ConcreteLabeled (Vertex) (EdgeLabel)

let to_undirected (digraph : G.t) : U.t =
  U.empty
  |> (fun graph -> List.fold ~f:U.add_vertex ~init:graph (G.all_vertices_of_graph digraph))
  |> fun graph -> List.fold ~f:U.add_edge_e ~init:graph (G.all_edges_of_graph digraph)


module Weight = struct
  type edge = U.edge

  type t = int

  let weight _ = 1

  let compare = Int.compare

  let add = Int.( + )

  let zero = 0
end

module KWeight = struct
  type t = EdgeLabel.t

  let compare = EdgeLabel.compare
end

module UKruskal = Graph.Kruskal.Make (U) (KWeight)

let run_kruskal (undigraph : U.t) : Weight.edge list = UKruskal.spanningtree undigraph

let prune_to_mst (digraph : G.t) : G.t =
  (* mst stands for minimum spanning tree *)
  let undirected_of_digraph = to_undirected digraph in
  let mst = run_kruskal undirected_of_digraph in
  G.empty
  |> fun graph ->
  List.fold ~f:G.add_vertex ~init:graph (G.all_vertices_of_graph digraph)
  |> fun graph ->
  List.fold
    ~f:(fun acc (v1, label, v2) ->
      G.add_edge_e acc (v1, label, v2) |> fun g -> G.add_edge_e g (v2, label, v1) )
    ~init:graph mst


let diet_edge_list (edgelist : G.E.t list) : G.E.t list =
  let subgraph = List.fold ~f:G.add_edge_e ~init:G.empty edgelist in
  let subgraph_pruned = prune_to_mst subgraph in
  G.all_edges_of_graph subgraph_pruned
