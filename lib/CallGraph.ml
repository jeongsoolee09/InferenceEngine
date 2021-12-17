module Vertex = String
module BiDiGraph = Graph.Persistent.Digraph.ConcreteBidirectional (Vertex)
include BiDiGraph

let of_string_pair_list (string_pair_list : (string * string) list) : t =
  List.fold ~init:empty
    ~f:(fun current_graph (s1, s2) -> add_edge current_graph s1 s2)
    string_pair_list


let find_callees (graph : t) (method_ : string) : string list = fold_succ List.cons graph method_ []
