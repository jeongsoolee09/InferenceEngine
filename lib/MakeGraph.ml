open Yojson.Basic
open GraphRepr
open SimilarityHandler
module G = GraphRepr.G

type json = Yojson.Basic.t

module GraphMaker = struct
  let batch_add_vertex (raw_json : json) (graph : G.t) =
    List.fold ~f:G.add_vertex ~init:graph (VertexMaker.get_all_vertices raw_json)


  let batch_add_edge (raw_json : json) (graph : G.t) =
    List.fold
      ~f:(fun acc edge -> G.add_edge_e acc edge)
      ~init:graph
      (EdgeMaker.get_all_edges raw_json)


  let init_graph (json : json) : G.t =
    G.empty |> batch_add_vertex json |> batch_add_edge json
    |> EstablishSimEdges.make_nodewise_sim_edge |> EstablishSimEdges.make_contextual_sim_edge


  (** Function for debugging by exporting Ocamlgraph to Graphviz Dot *)
  let graph_to_dot (graph : G.t) ?(filename = "initial_graph.dot") : unit =
    let out_channel = Out_channel.create filename in
    Dot.output_graph out_channel graph ;
    Out_channel.flush out_channel ;
    Out_channel.close out_channel
end
