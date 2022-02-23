open GraphRepr
open ListMonad
open InfixOperators
open SimilarityHandler
open Chain
open EdgeLabel
module G = GraphRepr.G

exception TODO

let move_to_line (lines : string array) (goto : int) : string =
  (* NOTE line numbers start from 1 *)
  lines.(goto - 1)


let location_is_correct (line : string) : bool = String.is_suffix ~suffix:";" (String.strip line)

let rectify_return_stmt_location (file_containing_method : string) (before_location : int) : int =
  (* NOTE: `file_containing_method` should be an absolute directory. *)
  Out_channel.print_endline
  @@ F.asprintf "file_containing_method: %s, before_location: %d\n" file_containing_method
       before_location ;
  let lines = Array.of_list @@ In_channel.read_lines file_containing_method in
  let current_location = ref before_location in
  let current_location_line = ref (move_to_line lines !current_location) in
  while not @@ location_is_correct !current_location_line do
    current_location := !current_location + 1 ;
    current_location_line := move_to_line lines !current_location
  done ;
  !current_location


let is_return_stmt_location_vertex (methname : Method.t) (locset : LocationSet.t) : bool =
  let this_method_return_stmt_lines = Method.get_return_stmt_lines methname in
  List.exists (LocationSet.to_int_list locset) ~f:(fun loc ->
      List.mem this_method_return_stmt_lines loc ~equal:Int.equal )


let rectify_return_stmt_location_vertex (vertex : G.V.t) : G.V.t =
  Out_channel.print_endline @@ F.asprintf "rectifying %s" (Vertex.to_string vertex) ;
  let methname, locset, dist =
    (Vertex.get_method vertex, Vertex.get_loc vertex, Vertex.get_dist vertex)
  in
  try
    if
      String.equal (Method.get_return_type methname) "void"
      || (not @@ is_return_stmt_location_vertex methname locset)
      || (not @@ Method.is_udf methname)
    then vertex
    else
      let locset_repaired =
        LocationSet.to_int_list locset
        |> List.map ~f:(fun loc ->
               let decl_file = Method.get_declaration_file methname in
               rectify_return_stmt_location decl_file loc )
        |> LocationSet.of_int_list
      in
      Vertex.make methname locset_repaired dist
  with Not_found_s _ -> vertex


let repair_vertices_with_incorrect_return_loc (graph : G.t) : G.t =
  G.map_vertex
    (fun vertex ->
      let methname, locset, dist =
        (Vertex.get_method vertex, Vertex.get_loc vertex, Vertex.get_dist vertex)
      in
      try
        if
          String.equal (Method.get_return_type methname) "void"
          || (not @@ is_return_stmt_location_vertex methname locset)
          || (not @@ Method.is_udf methname)
        then vertex
        else
          let locset_repaired =
            LocationSet.to_int_list locset
            |> List.map ~f:(fun loc ->
                   let decl_file = Method.get_declaration_file methname in
                   rectify_return_stmt_location decl_file loc )
            |> LocationSet.of_int_list
          in
          Vertex.make methname locset_repaired dist
      with Not_found_s _ -> vertex )
    graph
