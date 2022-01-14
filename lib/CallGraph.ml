module Vertex = Method
module BiDiGraph = Graph.Persistent.Digraph.ConcreteBidirectional (Vertex)
include BiDiGraph

let of_string_pair_list (string_pair_list : (string * string) list) : t =
  List.fold ~init:empty
    ~f:(fun current_graph (s1, s2) -> add_edge current_graph s1 s2)
    string_pair_list


let find_callees (callgraph : t) (method_ : Method.t) : string list =
  fold_succ List.cons callgraph method_ []


let init_callgraph () : t = Deserializer.deserialize_callgraph () |> of_string_pair_list

let has_no_active_caller (callgraph : t) (method_ : Method.t) : bool =
  Int.( = ) (in_degree callgraph method_) 0


(* 이 계산은... 얼마나 비쌀까...?? *)
let collect_all_deadcode (callgraph : t) : Vertex.t list =
  let initial_deadcodes =
    fold_vertex
      (fun vertex acc -> if has_no_active_caller callgraph vertex then vertex :: acc else acc)
      callgraph []
  in
  fold_vertex
    (fun vertex deadcode_acc ->
      let callees_are_all_deadcodes =
        fold_pred
          (fun pred smol_acc -> List.mem ~equal:Vertex.equal deadcode_acc pred && smol_acc)
          callgraph vertex true
      in
      if callees_are_all_deadcodes then vertex :: deadcode_acc else deadcode_acc )
    callgraph initial_deadcodes

(* 아 이건 너무 나이브하구나 *)

(* 에바네ㅋㅋ *)
