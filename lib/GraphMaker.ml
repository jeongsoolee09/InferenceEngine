open Yojson.Basic
open GraphRepr
open ListMonad
open InfixOperators
open SimilarityHandler
open Chain
module G = GraphRepr.G

type json = Yojson.Basic.t

module VertexMaker = struct
  let vertex_of_chain_slice (chain_slice : ChainSlice.t) : G.V.t =
    match chain_slice with
    | DefineSlice (current, _, loc, _) ->
        (Method.of_string current, LocationSet.of_string loc, ProbQuadruple.initial)
    | CallSlice (current, callee, loc, _) ->
        (Method.of_string callee, LocationSet.of_string loc, ProbQuadruple.initial)
    | VoidCallSlice (current, callee, loc, _) ->
        (Method.of_string callee, LocationSet.of_string loc, ProbQuadruple.initial)
    | RedefineSlice (current, loc, _) ->
        (Method.of_string current, LocationSet.of_string loc, ProbQuadruple.initial)
    | DeadSlice current ->
        (Method.of_string current, LocationSet.of_string "{ line }", ProbQuadruple.initial)
    | DeadByCycleSlice current ->
        (Method.of_string current, LocationSet.of_string "{ line }", ProbQuadruple.initial)
    | Temp (current, loc) ->
        (Method.of_string current, LocationSet.of_string loc, ProbQuadruple.initial)


  module VertexSet = Set.Make (Vertex)

  let get_all_vertices (raw_json : json) : G.V.t list =
    let vertices_with_dup =
      ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
      >>= ChainSliceManager.chain_slice_list_of_wrapped_chain
      |> List.filter ~f:(fun slice ->
             (not @@ ChainSlice.is_dead slice) && (not @@ ChainSlice.is_deadbycycle slice) )
      >>| vertex_of_chain_slice
    in
    (* remove duplicates by switching to and from a set *)
    let out = vertices_with_dup |> VertexSet.of_list |> VertexSet.elements in
    out


  let deserialize_all_important_methods () =
    Deserializer.deserialize_method_txt ()
    |> List.filter ~f:(fun method_str ->
           (not @@ String.is_substring method_str ~substring:"lambda")
           && (not @@ String.is_substring method_str ~substring:"Lambda")
           && (not @@ String.is_substring method_str ~substring:"<init>")
           && (not @@ String.is_substring method_str ~substring:"<clinit>") )
end

module ChainRefiners = struct
  let parse_skip_func (raw_signature : string) : string =
    let pattern = Str.regexp ".*\\.\\(.+\\)(.*)" in
    assert (Str.string_match pattern raw_signature 0) ;
    Str.matched_group 1 raw_signature


  let delete_inner_deads (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let all_but_last = List.drop_last_exn chain_slices in
    let dead_filtered =
      List.filter
        ~f:(fun chain_slice ->
          (not @@ ChainSlice.is_dead chain_slice) && (not @@ ChainSlice.is_deadbycycle chain_slice)
          )
        all_but_last
    in
    dead_filtered


  (** make a stub slice in front of the define slice of the chain. *)
  let process_head_define (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let head_define =
      if ChainSlice.is_define @@ List.hd_exn chain_slices then List.hd_exn chain_slices
      else List.nth_exn chain_slices 1
    in
    let define_current_method_field, location_field, define_using_field =
      match head_define with
      | DefineSlice (current_method, _, location, using_method) ->
          (current_method, location, using_method)
      | otherwise ->
          Printf.printf "\nfailed on: %s\n" (ChainSlice.to_string otherwise) ;
          failwith "ahahahahah"
    in
    let skip_func_method_names = Deserializer.deserialize_skip_func () >>| parse_skip_func in
    if
      (not @@ String.equal define_current_method_field define_using_field)
      &&
      try List.mem ~equal:String.equal skip_func_method_names (parse_skip_func define_using_field)
      with Assert_failure _ -> true
    then Temp (define_using_field, location_field) :: chain_slices
    else chain_slices


  let process_chainslices (chainslices : ChainSlice.t list) : ChainSlice.t list =
    let processors = [delete_inner_deads; process_head_define] in
    List.fold ~f:(fun acc processor -> processor acc) ~init:chainslices processors
end

module EdgeMaker = struct
  let make_bicycle_chain (list : 'a list) : ('a * 'a) list =
    let all_but_last = List.drop_last_exn list and all_but_first = List.tl_exn list in
    List.zip_exn all_but_last all_but_first


  (** Refines a raw chain-slice list into a G.E.t list, also emit an optional info of a void-call
      vertex. *)
  let refine_bicycle_chain (bicycle_chain : (ChainSlice.t * ChainSlice.t) list) :
      (ChainSlice.t * ChainSlice.t) list * G.LiteralVertex.t option =
    if List.is_empty bicycle_chain then ([], None)
    else
      let slice1, slice2 = List.last_exn bicycle_chain in
      match slice2 with
      | DefineSlice (_, ap, loc, using) as void_call_slice ->
          let is_frontend_tmp_var_ap = String.is_prefix ~prefix:"($" in
          if is_frontend_tmp_var_ap ap then
            ( List.slice bicycle_chain 0 (List.length bicycle_chain - 1)
            , Some (Method.of_string using, LocationSet.of_string loc) )
          else (bicycle_chain, None)
      | _ ->
          (bicycle_chain, None)


  let collect_voidcall_vertices (chain_slices : ChainSlice.t list) : G.LiteralVertex.t list =
    List.rev
    @@ List.fold
         ~f:(fun acc chain_slice ->
           if ChainSlice.is_voidcall chain_slice then
             (G.LiteralVertex.of_vertex @@ VertexMaker.vertex_of_chain_slice chain_slice) :: acc
           else acc )
         ~init:[] chain_slices


  (** Converts a raw chain-slice list into a G.E.t list, together with an optional info of a
      void-call vertex. *)
  let edge_list_of_chain_slice_list (chain_slices : ChainSlice.t list) :
      (G.E.t list * G.LiteralVertex.t list) list =
    let processed = ChainRefiners.process_chainslices chain_slices in
    let all_void_calls = collect_voidcall_vertices chain_slices in
    let bicycle_chain_of_chain_slices = make_bicycle_chain processed in
    let refined_bicycle_chain, frontend_define_vertex_opt =
      refine_bicycle_chain bicycle_chain_of_chain_slices
    in
    let edge_list =
      refined_bicycle_chain
      >>| fun (cs1, cs2) ->
      ( VertexMaker.vertex_of_chain_slice cs1
      , EdgeLabel.DataFlow
      , VertexMaker.vertex_of_chain_slice cs2 )
    in
    return
      ( edge_list
      , match frontend_define_vertex_opt with
        | Some frontend_define_vertex ->
            frontend_define_vertex :: all_void_calls
        | None ->
            all_void_calls )


  let get_all_edges_and_frontend_defines (raw_json : json) =
    let edge_list_and_frontend_define_vertex_opt_list =
      ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
      >>| ChainSliceManager.chain_slice_list_of_wrapped_chain >>= edge_list_of_chain_slice_list
    in
    ( edge_list_and_frontend_define_vertex_opt_list >>= fst
    , edge_list_and_frontend_define_vertex_opt_list >>= snd )


  let get_all_edges (raw_json : json) : G.E.t list =
    raw_json |> get_all_edges_and_frontend_defines |> fst


  let get_all_frontend_define_vertices (raw_json : json) : G.LiteralVertex.t list =
    raw_json |> get_all_edges_and_frontend_defines |> snd
end

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
  let out =
    EstablishSimEdges.make_contextual_sim_edge nodewise_sim_edges_added
    (* |> remove_bogus *)
  in
  if debug then graph_to_dot out ~filename:(make_now_string 9 ^ ".dot") ;
  out
