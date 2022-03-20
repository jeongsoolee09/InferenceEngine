open ListMonad
open InfixOperators
open GraphRepr
module U = Graph.Persistent.Graph.ConcreteLabeled (Vertex) (EdgeLabel)

let to_undirected (g : G.t) : U.t =
  let empty = U.empty in
  let vertices = G.all_vertices_of_graph g and edges = G.all_edges_of_graph g in
  empty
  |> fun subgraph ->
  List.fold vertices ~f:U.add_vertex ~init:subgraph
  |> fun subgraph -> List.fold edges ~f:U.add_edge_e ~init:subgraph


module Components = Graph.Components.Make (U)

let find_distinct_subgraphs (g : G.t) : G.t array =
  let undirected = to_undirected g in
  let vertices_array = Components.scc_array undirected in
  Array.map vertices_array ~f:(fun vertex_list ->
      G.empty
      |> fun subgraph ->
      List.fold vertex_list ~f:G.add_vertex ~init:subgraph
      |> fun subgraph ->
      List.fold (G.all_edges_of_graph g)
        ~f:(fun acc ((v1, _, v2) as edge) ->
          if List.mem vertex_list v1 ~equal:G.V.equal && List.mem vertex_list v2 ~equal:G.V.equal
          then G.add_edge_e acc edge
          else acc )
        ~init:subgraph )


let find_distinct_subgraphs_with_edges (g : G.t) : G.t array =
  let undirected = to_undirected g in
  let vertices_array = Components.scc_array undirected in
  let out =
    Array.map vertices_array ~f:(fun vertex_list ->
        G.empty
        |> fun subgraph ->
        List.fold vertex_list ~f:G.add_vertex ~init:subgraph
        |> fun subgraph ->
        List.fold (G.all_edges_of_graph g)
          ~f:(fun acc ((v1, _, v2) as edge) ->
            if List.mem vertex_list v1 ~equal:G.V.equal && List.mem vertex_list v2 ~equal:G.V.equal
            then G.add_edge_e acc edge
            else acc )
          ~init:subgraph )
  in
  Array.filter ~f:(fun subgraph -> G.nb_edges subgraph >= 1) out
