open GraphRepr
open Utils

(** (1) output a dot file of this snapshot, (2) render a svg off the dot file, and (3) show the svg
    file. *)
let visualize_snapshot (snapshot : G.t) ~(micro : bool) ~(autoopen : bool) : unit =
  let open DataFlowEdges in
  let now_timestring = make_now_string 9 in
  let filename_without_extension =
    if micro then F.asprintf "%s_micro" now_timestring else now_timestring
  in
  graph_to_dot snapshot ~filename:(filename_without_extension ^ ".dot") ;
  ignore
  @@ Unix.system
       (F.asprintf "dot -Tsvg -o %s.svg %s.dot" filename_without_extension
          filename_without_extension ) ;
  if autoopen then
    ignore
    @@ Unix.system (F.asprintf "open %s.svg -a 'Google Chrome.app'" filename_without_extension)


let visualize_and_open (snapshot : G.t) = visualize_snapshot snapshot ~autoopen:true ~micro:false

(* For testing ContextualFeatures. *)

module TrunkView = struct
  include G

  let make_bicycle_chain_array (array : 'a array) : ('a * 'a) array =
    let all_but_last = Array.slice array 0 (Array.length array - 1)
    and all_but_first = Array.slice array 1 0 in
    Array.zip_exn all_but_last all_but_first


  let make_trunkview (trunks : Trunk.t array) : t =
    let out = ref empty in
    let counter = ref 1 in
    Array.iter trunks ~f:(fun trunk ->
        let trunk_made_distinct =
          Array.map trunk ~f:(fun vertex ->
              let method_ = Vertex.get_method vertex in
              { vertex with
                method_=
                  ( counter := !counter + 1 ;
                    F.asprintf "%s %s.%s%d(%s)" (Method.get_return_type method_)
                      (Method.get_class_name method_) (Method.get_method_name method_) !counter
                      (Method.get_arg_list method_) ) } )
        in
        Array.iter (make_bicycle_chain_array trunk_made_distinct) ~f:(fun (v1, v2) ->
            out := add_edge_e !out (v1, DataFlow, v2) ) ) ;
    !out


  let deuniqufy_method_name (method_ : Method.t) : Method.t =
    if Method.is_initializer method_ then
      Method.of_string
      @@ F.asprintf "%s.%s(%s)" (Method.get_class_name method_)
           (Method.get_method_name method_ |> String.rstrip ~drop:Char.is_digit)
           (Method.get_arg_list method_)
    else
      Method.of_string
      @@ F.asprintf "%s %s.%s(%s)" (Method.get_return_type method_) (Method.get_class_name method_)
           (Method.get_method_name method_ |> String.rstrip ~drop:Char.is_digit)
           (Method.get_arg_list method_)


  (* ============ Overriding GraphViz functions ============  *)

  let graph_attributes _ = [`Rankdir `LeftToRight]

  let vertex_name (vertex : V.t) : string =
    F.asprintf "\"%s.%s\""
      (vertex |> Vertex.get_method |> Method.get_class_name)
      (vertex |> Vertex.get_method |> Method.get_method_name)


  let vertex_attributes (vertex : V.t) =
    (* if it's an UDF, then do nothing *)
    (* if it's an API /srm/, then mark it as pink *)
    (* if it's an API /non/, then mark it as green *)
    let restored_method_name = deuniqufy_method_name @@ Vertex.get_method @@ vertex in
    match (Method.is_api restored_method_name, AutoTest.Scoring.is_srm restored_method_name) with
    | true, true ->
        [`Fillcolor 0xfd93cd; `Style `Filled]
    | true, false ->
        [`Fillcolor 0x58f540; `Style `Filled]
    | false, _ ->
        [`Fillcolor 0xffffff; `Style `Filled]
end

module TrunkViewDot = Graph.Graphviz.Dot (TrunkView)

