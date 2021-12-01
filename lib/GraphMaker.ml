open Yojson.Basic
open GraphRepr
open SimilarityHandler
module G = GraphRepr.G

type json = Yojson.Basic.t

let batch_add_vertex (raw_json : json) (graph : G.t) =
  List.fold ~f:G.add_vertex ~init:graph (VertexMaker.get_all_vertices raw_json)


let batch_add_edge (raw_json : json) (graph : G.t) =
  List.fold
    ~f:(fun acc edge -> G.add_edge_e acc edge)
    ~init:graph
    (EdgeMaker.get_all_edges raw_json)


let remove_bogus (graph : G.t) =
  let boguses =
    G.fold_vertex
      (fun ((meth, _, _) as vertex) acc -> if String.is_empty meth then vertex :: acc else acc)
      graph []
  in
  List.fold ~f:(fun acc bogus -> G.remove_vertex acc bogus) ~init:graph boguses


(* I don't know why, but this gives a GMT+0:00 timestring. *)
let make_now_string () =
  let open CalendarLib in
  let now_raw = Calendar.now () in
  let year = Calendar.year now_raw in
  let month = Date.int_of_month @@ Calendar.month now_raw in
  let day = Calendar.day_of_month now_raw in
  let hour = Calendar.hour now_raw in
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
    G.empty |> batch_add_vertex json |> batch_add_edge json
    |> EstablishSimEdges.make_nodewise_sim_edge |> EstablishSimEdges.make_contextual_sim_edge
    |> remove_bogus
  in
  graph_to_dot out ~filename:(make_now_string () ^ ".dot") ;
  out