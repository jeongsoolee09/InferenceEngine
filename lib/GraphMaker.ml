open Yojson.Basic
open GraphRepr
open SimilarityHandler
module G = GraphRepr.G

type json = Yojson.Basic.t

let batch_add_vertex (raw_json : json) (graph : G.t) =
  List.fold ~f:G.add_vertex ~init:graph (VertexMaker.get_all_vertices raw_json)


let batch_add_edge (raw_json : json) (graph : G.t) =
  let all_edges, all_void_calls = EdgeMaker.get_all_edges_and_void_calls raw_json in
  let edge_added = List.fold ~f:(fun acc edge -> G.add_edge_e acc edge) ~init:graph all_edges in
  (edge_added, all_void_calls)


let remove_bogus (graph : G.t) =
  let boguses =
    G.fold_vertex
      (fun ((meth, _, _) as vertex) acc -> if String.is_empty meth then vertex :: acc else acc)
      graph []
  in
  List.fold ~f:(fun acc bogus -> G.remove_vertex acc bogus) ~init:graph boguses


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


(** Function for debugging by exporting Ocamlgraph to Graphviz Dot *)
let graph_to_dot (graph : G.t) ?(filename = "initial_graph.dot") : unit =
  let out_channel = Out_channel.create filename in
  Dot.output_graph out_channel graph ;
  Out_channel.flush out_channel ;
  Out_channel.close out_channel


let init_graph (json : json) : G.t =
  let out =
    let df_vertices_added = batch_add_vertex json G.empty in
    let df_edges_added, all_void_calls = batch_add_edge json df_vertices_added in
    let ns_edges_added = EstablishSimEdges.make_nodewise_sim_edge df_edges_added in
    let cs_edges_added = EstablishSimEdges.make_contextual_sim_edge ns_edges_added in
    let hs_edges_added = EstablishSimEdges.make_hybrid_sim_edge all_void_calls cs_edges_added in
    remove_bogus hs_edges_added
  in
  graph_to_dot out ~filename:(make_now_string 9 ^ ".dot") ;
  out
