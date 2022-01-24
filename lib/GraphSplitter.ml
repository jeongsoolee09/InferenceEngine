open ListMonad
open InfixOperators
open GraphRepr
module Hashtbl = Caml.Hashtbl

module CompUnit = struct
  type t = Known of string | Unknown [@@deriving equal]

  let to_string (comp_unit : t) = match comp_unit with Known unit_ -> unit_ | Unknown -> "Unknown"
end

let get_comp_unit =
  let cache = Hashtbl.create 777 in
  fun (method_ : Method.t) : CompUnit.t ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        let (out : CompUnit.t) =
          let root_dir = Deserializer.deserialize_config () in
          let abs_dirs_and_classnames =
            DirectoryManager.Classnames.classnames_by_compilation_unit root_dir
          in
          let out =
            List.find
              ~f:(fun (abs_dir, classnames) ->
                List.mem classnames (Method.get_class_name method_) ~equal:String.equal )
              abs_dirs_and_classnames
          in
          match out with
          | None ->
              Unknown
          | Some (abs_dir, _) ->
              Known (List.last_exn @@ String.split ~on:'/' abs_dir)
        in
        Hashtbl.add cache method_ out ;
        out
    | Some res ->
        res


let create_comp_unit_lookup_table (all_methods : Method.t list) : (Method.t, CompUnit.t) Hashtbl.t =
  let out = Hashtbl.create 777 in
  List.iter ~f:(fun method_ -> Hashtbl.add out method_ (get_comp_unit method_)) all_methods ;
  out


let split_graph_by_single_comp_unit (df_graph : G.t)
    (lookup_table : (Method.t, CompUnit.t) Hashtbl.t) (comp_unit_name : String.t) : G.t =
  print_endline @@ F.asprintf "splitting for %s..." comp_unit_name ;
  let all_vertices = G.all_non_frontend_vertices_of_graph df_graph
  and all_methods = G.all_methods_of_graph df_graph
  and all_edges = G.fold_edges_e List.cons df_graph [] in
  let this_comp_unit_udfs =
    List.filter
      ~f:(fun method_ ->
        match Hashtbl.find lookup_table method_ with
        | Known comp_unit ->
            String.equal comp_unit comp_unit_name
        | Unknown ->
            false )
      all_methods
  in
  let this_comp_unit_vertices =
    List.filter all_vertices ~f:(fun vertex ->
        let vertex_method = Vertex.get_method vertex in
        List.mem ~equal:Method.equal this_comp_unit_udfs vertex_method )
  in
  let edges_involving_this_comp_unit_udfs =
    List.filter
      ~f:(fun (v1, _, v2) ->
        List.mem ~equal:Vertex.equal this_comp_unit_vertices v1
        || List.mem ~equal:Vertex.equal this_comp_unit_vertices v2 )
      all_edges
  in
  {G.empty with comp_unit= comp_unit_name}
  |> fun graph ->
  List.fold this_comp_unit_vertices ~f:G.add_vertex ~init:graph
  |> fun graph -> List.fold edges_involving_this_comp_unit_udfs ~f:G.add_edge_e ~init:graph


let split_graph_by_comp_unit (graph : G.t) : G.t list =
  let all_comp_units =
    DirectoryManager.get_compilation_unit_subdirs (Deserializer.deserialize_config ())
  in
  List.iter ~f:(fun comp_unit ->
      Out_channel.print_endline @@ F.asprintf "comp_unit %s identified." comp_unit ) all_comp_units ;
  let lookup_table = create_comp_unit_lookup_table (G.all_methods_of_graph graph) in
  all_comp_units >>| split_graph_by_single_comp_unit graph lookup_table
