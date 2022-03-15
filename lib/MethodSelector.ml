open ListMonad
open Utils
open InfixOperators
open GraphRepr

let random_select (methods : Method.t list) (received_responses : Response.t list) : 'a =
  let unasked_methods =
    List.filter methods ~f:(fun method_ ->
        not @@ List.mem (received_responses >>| Response.get_method) method_ ~equal:Method.equal )
  in
  if List.is_empty unasked_methods then failwith "cannot select from an empty list" ;
  let random_index = Random.int_incl 0 (List.length unasked_methods - 1) in
  List.nth_exn unasked_methods random_index


let init_vertex_out_degree_assoc =
  let cache = ref G.MemoMap.empty in
  fun (graph : G.t) : (Vertex.t * int) array ->
    match G.MemoMap.find_opt graph.graph !cache with
    | None ->
        let out =
          let inited =
            Array.of_list
            @@ List.map (G.all_vertices_of_graph graph) ~f:(fun vertex ->
                   (vertex, G.out_degree graph vertex) )
          in
          Array.sort ~compare:(fun (_, od1) (_, od2) -> -Int.compare od1 od2) inited ;
          inited
        in
        cache := G.MemoMap.add graph.graph out !cache ;
        out
    | Some res ->
        res

        (* If java.lang, then NS edge *)

let select_by_degree (methods : Method.t list) (received_responses : Response.t list) (graph : G.t)
    : Method.t =
  (* select an unasked method that has the most out_degree. *)
      if List.is_empty methods then failwith "empty list";
  let methods_and_out_degrees_sorted = init_vertex_out_degree_assoc graph in
  let unasked_methods =
    Array.filter methods_and_out_degrees_sorted ~f:(fun (vertex, _) ->
        List.mem methods (Vertex.get_method vertex) ~equal:Method.equal
        && not
           @@ List.mem
                (received_responses >>| Response.get_method)
                (Vertex.get_method vertex) ~equal:Method.equal )
  in
  Vertex.get_method @@ fst unasked_methods.(0)
