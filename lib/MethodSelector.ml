(* NOTE: Experimental. *)

open ListMonad
open Utils
open InfixOperators
open GraphRepr

let init_vertex_out_degree_assoc (graph : G.t) : (Vertex.t * int) array =
  let inited =
    Array.of_list
    @@ List.map (G.all_vertices_of_graph graph) ~f:(fun vertex ->
           (vertex, G.out_degree graph vertex) )
  in
  Array.sort ~compare:(fun (_, od1) (_, od2) -> -Int.compare od1 od2) inited ;
  inited


let pop (arr : 'a array ref) : 'a =
  let popped = !arr.(0) in
  arr := Array.slice !arr 1 0 ;
  popped


let ask_by_degree (graph : G.t) (received_responses : Response.t list) : Question.t =
  let vertex_out_degree_assoc = ref (init_vertex_out_degree_assoc graph) in
  let popped = ref (pop vertex_out_degree_assoc) in
  let asked_methods = received_responses >>| Response.get_method in
  while
    List.mem asked_methods (Vertex.get_method @@ fst !popped) ~equal:Method.equal
    || (not @@ ProbQuadruple.is_indeterminate (Vertex.get_dist @@ fst !popped))
  do
    popped := pop vertex_out_degree_assoc
  done ;
  print_endline
  @@ F.asprintf "%s has out_degree %d" (Vertex.get_method @@ fst !popped) (snd !popped) ;
  AskingForLabel (Vertex.get_method @@ fst !popped)
