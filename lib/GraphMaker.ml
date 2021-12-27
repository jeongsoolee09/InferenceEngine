open Yojson.Basic
open GraphRepr
open ListMonad
open InfixOperators
open SimilarityHandler
module G = GraphRepr.G

type json = Yojson.Basic.t

let batch_add_vertex (raw_json : json) (graph : G.t) =
  List.fold ~f:G.add_vertex ~init:graph (VertexMaker.get_all_vertices raw_json)


let batch_add_edge (raw_json : json) (graph : G.t) =
  let all_edges, all_void_calls = EdgeMaker.get_all_edges_and_frontend_defines raw_json in
  let edge_added = List.fold ~f:(fun acc edge -> G.add_edge_e acc edge) ~init:graph all_edges in
  let out_channel = Out_channel.create "void_calls.lisp" in
  Sexp.output out_channel (Sexp.List (List.map ~f:G.LiteralVertex.sexp_of_t all_void_calls)) ;
  Out_channel.close out_channel ;
  edge_added


(* let remove_bogus (graph : G.t) = *)
(*   let boguses = *)
(*     G.fold_vertex *)
(*       (fun ((meth, _, _) as vertex) acc -> if String.is_empty meth then vertex :: acc else acc) *)
(*       graph [] *)
(*   in *)
(*   List.fold ~f:(fun acc bogus -> G.remove_vertex acc bogus) ~init:graph boguses *)


(** Function for debugging by exporting Ocamlgraph to Graphviz Dot *)
let graph_to_dot (graph : G.t) ?(filename = "initial_graph.dot") : unit =
  let out_channel = Out_channel.create filename in
  Dot.output_graph out_channel graph ;
  Out_channel.flush out_channel ;
  Out_channel.close out_channel


let graph_already_serialized (suffix : string) : string option =
  Array.find (Sys.readdir ".") ~f:(fun str -> String.is_substring str ~substring:suffix)


let init_graph (json : json) ~(debug : bool) : G.t =
  (* let out = *)
  (*   G.empty |> batch_add_vertex json |> batch_add_edge json *)
  (*   |> EstablishSimEdges.make_nodewise_sim_edge |> EstablishSimEdges.make_contextual_sim_edge *)
  (*   |> remove_bogus *)
  let df_edges_added =
    match graph_already_serialized "df_edges" with
    | None ->
        let result = G.empty |> batch_add_vertex json |> batch_add_edge json in
        G.serialize_to_bin result ~suffix:"df_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  print_endline "\nDF edges established.\n" ;
  let nodewise_sim_edges_added =
    match graph_already_serialized "ns_edges" with
    | None ->
        let result = EstablishSimEdges.make_nodewise_sim_edge df_edges_added in
        G.serialize_to_bin result ~suffix:"ns_edges" ;
        result
    | Some filename ->
        Deserializer.deserialize_graph filename
  in
  print_endline "\nNS edges established.\n" ;
  let out = EstablishSimEdges.make_contextual_sim_edge nodewise_sim_edges_added (* |> remove_bogus *) in
  if debug then graph_to_dot out ~filename:(make_now_string 9 ^ ".dot") ;
  Memoize.NSClusters.set_ns_cluster (all_ns_clusters out) ;
  out


module DirectoryManager = struct
  let walk_for_extension (root_dir : string) (extension : string) : string list =
    let rec inner (current_dir : string) (filename_acc : string list) =
      let subdirectories =
        Array.filter
          ~f:(fun name -> match Sys.is_directory name with `Yes -> true | _ -> false)
          (Array.map ~f:(fun name -> current_dir ^ "/" ^ name) (Sys.readdir current_dir))
      in
      let files_matching_extension =
        Array.filter
          ~f:(fun name -> match Sys.is_directory name with `No -> true | _ -> false)
          (Array.map ~f:(fun name -> current_dir ^ "/" ^ name) (Sys.readdir current_dir))
        |> Array.fold
             ~f:(fun acc elem ->
               if String.is_suffix elem ~suffix:extension then elem :: acc else acc )
             ~init:[]
      in
      if Array.is_empty subdirectories then filename_acc @ files_matching_extension
      else
        Array.fold subdirectories
          ~f:(fun acc subdirectory -> inner subdirectory acc)
          ~init:(filename_acc @ files_matching_extension)
    in
    inner root_dir []
end
