open InfixOperators
open ListMonad
open GraphRepr
open TaintLabel
module Hashtbl = Caml.Hashtbl

(* - 루프를 도는데:
   - 현재의 최신 스냅샷에 대한 정확도를 채점하고, determinate한 메소드와 버텍스의 개수를 센다.
   - 물음에 자동으로 답한다 (정답을 답해야 한다).

   ==> 이걸 stdout에도 출력하고, 로그파일에도 쓰게 하자. 야호! *)

exception TODO

let prepare_solution (raw_solution : (string * string) array) : (string, TaintLabel.t) Hashtbl.t =
  let acc = Hashtbl.create 777 in
  Array.iter
    ~f:(fun (method_, label_str) ->
      let label =
        match label_str with
        | "src" ->
            Source
        | "sin" ->
            Sink
        | "san" ->
            Sanitizer
        | "non" ->
            None
        | otherwise ->
            raise @@ Invalid_argument otherwise
      in
      Hashtbl.add acc method_ label )
    raw_solution ;
  acc


let single_result_is_correct ~(inferred : TaintLabel.t list) ~(ground : TaintLabel.t list) : bool =
  List.fold
    ~f:(fun acc inferred_label ->
      if TaintLabel.is_none inferred_label then acc
      else
        let inferred_label_is_correct = List.mem ground inferred_label ~equal:TaintLabel.equal in
        inferred_label_is_correct && acc )
    ~init:true inferred


let get_precision_of_snapshot (snapshot : G.t) (solution : (string * TaintLabel.t) array) : Float.t
    =
  let correct_entries_count =
    Array.fold ~f:(fun acc (method_, label) -> raise TODO) solution ~init:0
  in
  (* get the percentage. *)
  raise TODO


(** Main functionality. *)
let auto_test_spechunter_for_snapshot (initial_snapshot : G.t) =
  (*  *)
  raise TODO
