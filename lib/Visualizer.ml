open GraphRepr

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
  if autoopen then ignore @@ Unix.system (F.asprintf "open %s.svg" filename_without_extension)


let visualize_and_open (snapshot : G.t) = visualize_snapshot snapshot ~autoopen:true ~micro:false
