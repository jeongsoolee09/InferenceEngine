open ListMonad
open InfixOperators
open GraphRepr
open SpawnPython
open TaintLabel

let read_csv_file () : (Method.t * TaintLabel.t list) array = raise TODO

let resolve_multiple_labels (method_ : Method.t) (labels : TaintLabel.t list) (next_graph : G.t) =
  raise TODO


let transfer_single_method (method_ : Method.t) (label : TaintLabel.t) (next_graph : G.t) : G.t =
  raise TODO


let tranfer_graph (prev_graph : G.t) (next_graph : G.t) : G.t =
  let csv_file_read = read_csv_file () in
  Array.fold csv_file_read
    ~f:(fun acc (method_, labels) ->
      match labels with
      | [label] ->
          transfer_single_method method_ label acc
      | [_; _] as labels ->
          resolve_multiple_labels method_ labels acc
      | _ ->
          (* skip instead of raising *)
          acc )
    ~init:next_graph
