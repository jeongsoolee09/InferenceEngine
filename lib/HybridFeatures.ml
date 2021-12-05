open ListMonad
open InfixOperators
open GraphRepr

(** Hybrid Features take both nodewise features and contextual features into account. *)

exception TODO

let is_void_call_leaf (methstring : string) (graph : G.t) : bool =
  let methstring_vertices = G.this_method_vertices graph methstring in
  assert (Int.( = ) (List.length methstring_vertices) 1) ;
  let methstring_vertex = List.hd_exn methstring_vertices in
  G.is_df_leaf (G.LiteralVertex.of_vertex methstring_vertex) graph && raise TODO

(* TODO define is_void_call!! *)
