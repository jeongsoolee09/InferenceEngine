open Yojson.Basic
open GraphRepr
open ListMonad
open InfixOperators
open SimilarityHandler
open Chain
open EdgeLabel
open Utils
open GraphRepr
module LV = G.LiteralVertex

type json = Yojson.Basic.t

module VertexMaker = struct
  let vertex_of_chain_slice (chain_slice : ChainSlice.t) : G.V.t =
    match chain_slice with
    | DefineSlice (current, _, loc, _) ->
        Vertex.make_initial (Method.of_string current) (LocationSet.of_string loc)
    | CallSlice (_, callee, loc, _) ->
        Vertex.make_initial (Method.of_string callee) (LocationSet.of_string loc)
    | VoidCallSlice (_, callee, loc, _) ->
        Vertex.make_initial (Method.of_string callee) (LocationSet.of_string loc)
    | RedefineSlice (current, loc, _) ->
        Vertex.make_initial (Method.of_string current) (LocationSet.of_string loc)
    | DeadSlice current ->
        Vertex.make_initial (Method.of_string current) LocationSet.dummy
    | DeadByCycleSlice current ->
        Vertex.make_initial (Method.of_string current) LocationSet.dummy
    | Temp (current, loc) ->
        Vertex.make_initial (Method.of_string current) (LocationSet.of_string loc)


  module VertexSet = Set.Make (Vertex)

  let get_all_vertices (raw_json : json) : G.V.t list =
    let test_classnames =
      DirectoryManager.Classnames.get_test_classnames Deserializer.project_root
    in
    let intermediate =
      wrapped_chain_list_of_raw_json raw_json
      >>= chain_slice_list_of_wrapped_chain
      |> List.filter ~f:(fun slice ->
             let current_method = ChainSlice.get_current_method slice in
             if Method.is_frontend current_method then false
             else
               not
               @@ List.mem ~equal:String.equal test_classnames
                    (Method.get_class_name current_method)
               && (not @@ ChainSlice.is_dead slice)
               && (not @@ ChainSlice.is_deadbycycle slice) )
      >>| vertex_of_chain_slice
    in
    intermediate |> VertexSet.of_list |> VertexSet.elements
    |> List.filter ~f:(not << Method.is_frontend << Vertex.get_method)
end

module ChainRefiners = struct
  let parse_skip_func (raw_signature : string) : string =
    let pattern = Str.regexp ".*\\.\\(.+\\)(.*)" in
    assert (Str.string_match pattern raw_signature 0) ;
    Str.matched_group 1 raw_signature


  let delete_inner_deads (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let dead_filtered =
      List.filter
        ~f:(fun chain_slice ->
          (not @@ ChainSlice.is_dead chain_slice) && (not @@ ChainSlice.is_deadbycycle chain_slice)
          )
        chain_slices
    in
    if ChainSlice.is_dead @@ List.last_exn chain_slices then
      dead_filtered @ [List.last_exn chain_slices]
    else dead_filtered


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
      let _, slice2 = List.last_exn bicycle_chain in
      match slice2 with
      | DefineSlice (_, ap, loc, using) ->
          let is_frontend_tmp_var_ap = String.is_prefix ~prefix:"($" in
          if is_frontend_tmp_var_ap ap then
            (bicycle_chain, Some (Method.of_string using, LocationSet.of_string loc))
          else (bicycle_chain, None)
      | _ ->
          (bicycle_chain, None)


  let collect_additional_voidcall_vertices (chain : Chain.t) : ChainSlice.t list =
    (* call을 한 다음에 바로 다음이 Define인데, using이 그놈이고 define이 frontend면 그놈은 voidcall이얌!!! 끼얏호 *)
    let all_call_slices = List.filter chain ~f:ChainSlice.is_call in
    let all_call_slices_and_next =
      List.map all_call_slices ~f:(fun call_slice ->
          let next_slice = get_next_elem chain ~next_to:call_slice ~equal:ChainSlice.equal in
          (call_slice, next_slice) )
    in
    List.filter all_call_slices_and_next ~f:(fun (call_slice, next_slice) ->
        ChainSlice.is_define next_slice
        && Method.equal (ChainSlice.get_callee call_slice) (ChainSlice.get_callee next_slice)
        &&
        let is_frontend_tmp_var_ap = String.is_prefix ~prefix:"($" in
        is_frontend_tmp_var_ap (ChainSlice.get_access_path next_slice)
        && (ChainSlice.is_dead @@ get_next_elem chain ~next_to:next_slice ~equal:ChainSlice.equal) )
    |> List.map ~f:fst


  let collect_voidcall_vertices (chain : Chain.t) : ChainSlice.t list =
    List.filter ~f:ChainSlice.is_voidcall chain


  let collect_all_voidcall_vertices (chain : Chain.t) : ChainSlice.t list =
    collect_voidcall_vertices chain @ collect_additional_voidcall_vertices chain


  (** Converts a raw chain-slice list into a G.E.t list, together with an optional info of a
      void-call vertex. *)
  let edge_list_of_chain_slice_list (chain : Chain.t) : (G.E.t list * G.LiteralVertex.t list) list =
    let processed = ChainRefiners.process_chainslices chain in
    let all_void_calls =
      ReturnValUsedInCaller.filter_really_returnval_not_used @@ collect_all_voidcall_vertices chain
      >>| (VertexMaker.vertex_of_chain_slice >> G.LiteralVertex.of_vertex)
    in
    let bicycle_chain_of_chain_slices = make_bicycle_chain processed in
    let refined_bicycle_chain, frontend_define_vertex_opt =
      refine_bicycle_chain bicycle_chain_of_chain_slices
    in
    let edge_list =
      let last_elem = List.last_exn refined_bicycle_chain in
      let chain_first_slice = List.hd_exn chain in
      if ChainSlice.is_define chain_first_slice then
        let chain_first_current_method = ChainSlice.get_current_method chain_first_slice
        and chain_first_locset = ChainSlice.get_locset chain_first_slice
        and chain_first_callee = ChainSlice.get_callee chain_first_slice in
        let chain_first_edge =
          ( Vertex.make_initial chain_first_callee LocationSet.dummy2
          , DataFlow
          , Vertex.make_initial chain_first_current_method chain_first_locset )
        in
        if not @@ Method.equal chain_first_callee chain_first_current_method then
          chain_first_edge
          :: ( ( if ChainSlice.is_voidcall (fst last_elem) then refined_bicycle_chain
               else List.drop_last_exn @@ refined_bicycle_chain )
             >>| fun (cs1, cs2) ->
             (VertexMaker.vertex_of_chain_slice cs1, DataFlow, VertexMaker.vertex_of_chain_slice cs2)
             )
        else
          ( if ChainSlice.is_voidcall (fst last_elem) then refined_bicycle_chain
          else List.drop_last_exn @@ refined_bicycle_chain )
          >>| fun (cs1, cs2) ->
          (VertexMaker.vertex_of_chain_slice cs1, DataFlow, VertexMaker.vertex_of_chain_slice cs2)
      else
        ( if ChainSlice.is_voidcall (fst last_elem) then refined_bicycle_chain
        else List.drop_last_exn @@ refined_bicycle_chain )
        >>| fun (cs1, cs2) ->
        (VertexMaker.vertex_of_chain_slice cs1, DataFlow, VertexMaker.vertex_of_chain_slice cs2)
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
      wrapped_chain_list_of_raw_json raw_json
      >>| chain_slice_list_of_wrapped_chain >>= edge_list_of_chain_slice_list
    in
    ( edge_list_and_frontend_define_vertex_opt_list >>= fst
    , edge_list_and_frontend_define_vertex_opt_list >>= snd )


  let get_all_edges (raw_json : json) : G.E.t list =
    raw_json |> get_all_edges_and_frontend_defines |> fst


  let get_all_frontend_define_vertices (raw_json : json) : G.LiteralVertex.t list =
    raw_json |> get_all_edges_and_frontend_defines |> snd
end

let batch_add_vertex (raw_json : json) (graph : G.t) =
  let out = List.fold ~f:G.add_vertex ~init:graph (VertexMaker.get_all_vertices raw_json) in
  print_endline "adding vertex done" ;
  out


let sequential_add_edge (graph : G.t ref) (all_edges : G.E.t list) =
  let all_edges_arr = Array.of_list all_edges in
  for i = 0 to List.length all_edges - 1 do
    let v1, _, v2 = all_edges_arr.(i) in
    if
      (not @@ Method.is_frontend (Vertex.get_method v1))
      && (not @@ Method.is_frontend (Vertex.get_method v2))
    then graph := G.add_edge_e !graph all_edges_arr.(i)
  done


let sequential_dedup_edge (all_edges : G.E.t array) (out : G.E.t list ref) =
  let module EdgeSet = Caml.Set.Make (G.E) in
  out := all_edges |> Array.to_list |> EdgeSet.of_list |> EdgeSet.elements


let batch_add_edge (raw_json : json) (graph : G.t) =
  let all_edges, all_void_calls = EdgeMaker.get_all_edges_and_frontend_defines raw_json in
  let acc = ref graph in
  let all_edges_arr = Array.of_list all_edges in
  let deduped = ref [] in
  sequential_dedup_edge all_edges_arr deduped ;
  let repaired_edges =
    List.map
      ~f:(fun (v1, label, v2) ->
        let v1_repaired = ReturnStmtLocation.rectify_return_stmt_location_vertex v1
        and v2_repaired = ReturnStmtLocation.rectify_return_stmt_location_vertex v2 in
        (v1_repaired, label, v2_repaired) )
      !deduped
  in
  sequential_add_edge acc repaired_edges ;
  let edge_added = !acc in
  let out_channel = Out_channel.create "void_calls.lisp" in
  Sexp.output out_channel (Sexp.List (List.map ~f:G.LiteralVertex.sexp_of_t all_void_calls)) ;
  Out_channel.close out_channel ;
  edge_added


(** Function for debugging by exporting Ocamlgraph to Graphviz Dot *)
let graph_to_dot (graph : G.t) ?(filename = "initial_graph.dot") : unit =
  let out_channel = Out_channel.create filename in
  Dot.output_graph out_channel graph ;
  Out_channel.flush out_channel ;
  Out_channel.close out_channel


let graph_already_serialized ~(comp_unit : String.t) ~(suffix : String.t) ?(finished = false) :
    String.t option =
  Array.find (Sys.readdir ".") ~f:(fun filename ->
      (not @@ String.is_prefix filename ~prefix:".")
      && ( if finished then String.is_suffix filename ~suffix:"finished.bin"
         else String.is_suffix filename ~suffix:".bin" )
      && String.is_substring filename ~substring:(F.asprintf "%s_%s" comp_unit suffix) )


module Repair = struct
  let find_return_vertex_of_method (method_ : Method.t) (graph : G.t) : G.V.t =
    let return_stmt_locs = Deserializer.deserialize_return_stmts () in
    let this_method_decl_filename = Method.get_declaration_file method_ in
    let this_method_return_locs =
      List.Assoc.find_exn return_stmt_locs method_ ~equal:Method.equal
      >>| fun loc -> ReturnStmtLocation.rectify_return_stmt_location this_method_decl_filename loc
    in
    List.hd_exn
    @@ G.fold_vertex
         (fun vertex acc ->
           let methname = Vertex.get_method vertex and locset = Vertex.get_loc vertex in
           let match_ =
             Method.equal methname method_
             && List.exists
                  ~f:(fun loc -> List.mem this_method_return_locs loc ~equal:Int.equal)
                  (LocationSet.to_int_list locset)
           in
           if match_ then vertex :: acc else acc )
         graph []


  (* spotting (4) *)
  let find_fourths (method_ : Method.t) (graph : G.t) : G.V.t list =
    G.fold_vertex
      (fun vertex acc ->
        let methname = Vertex.get_method vertex and locset = Vertex.get_loc vertex in
        if Method.equal methname method_ && LocationSet.equal locset LocationSet.dummy2 then
          vertex :: acc
        else acc )
      graph []


  let find_methods_that_need_connecting (graph : G.t) : Method.t list =
    let methods_with_return_stmt_vertices =
      List.fold
        ~f:(fun acc method_ ->
          try
            let this_method_return_locs =
              List.Assoc.find_exn
                (Deserializer.deserialize_return_stmts ())
                method_ ~equal:Method.equal
            in
            let vertices_with_retun_stmts =
              G.fold_vertex
                (fun vertex acc ->
                  let methname = Vertex.get_method vertex and locset = Vertex.get_loc vertex in
                  let match_ =
                    Method.equal methname method_
                    && List.exists
                         ~f:(fun loc -> List.mem this_method_return_locs loc ~equal:Int.equal)
                         (LocationSet.to_int_list locset)
                  in
                  if match_ then vertex :: acc else acc )
                graph []
            in
            if not @@ List.is_empty vertices_with_retun_stmts then method_ :: acc else acc
          with Not_found_s _ -> acc )
        (G.all_methods_of_graph graph) ~init:[]
    in
    let dummy1_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if LocationSet.equal (Vertex.get_loc vertex) LocationSet.dummy then vertex :: acc else acc
          )
        graph []
    in
    List.fold
      ~f:(fun acc method_ ->
        if
          List.exists dummy1_vertices ~f:(fun dummy1_vertex ->
              Method.equal (Vertex.get_method dummy1_vertex) method_ )
        then method_ :: acc
        else acc )
      ~init:[] methods_with_return_stmt_vertices


  let reconnect_disconnected_edges (graph : G.t) : G.t =
    let methods_that_need_connecting = find_methods_that_need_connecting graph in
    List.fold
      ~f:(fun acc method_ ->
        let third = find_return_vertex_of_method method_ acc
        and fourths = find_fourths method_ acc in
        let fourth_succs =
          fourths
          >>= fun vertex -> G.get_succs acc (G.LiteralVertex.of_vertex vertex) ~label:DataFlow
        in
        let third_fourths_connected =
          List.fold fourth_succs
            ~f:(fun smol_acc fourth_succ -> G.add_edge_e smol_acc (third, DataFlow, fourth_succ))
            ~init:acc
        in
        List.fold
          ~f:(fun smol_acc fourth -> G.remove_vertex smol_acc fourth)
          ~init:third_fourths_connected fourths )
      ~init:graph methods_that_need_connecting
end
