open GraphRepr
open InfixOperators
open ListMonad

type trunk = G.Trunk.t

(** Do the two trunks share the same callee? **)
let same_callee_in_trunk_count ((trunk1, trunk2) : trunk * trunk) : int =
  let trunk1_only_methods = trunk1 >>| fst and trunk2_only_methods = trunk2 >>| fst in
  List.fold
    ~f:(fun acc vertex ->
      let matching = List.mem ~equal:G.V.equal trunk2 vertex in
      if matching then (* print_endline "same callee!!" ; *)
        acc + 1 else acc )
    ~init:0 trunk1


(** Do the two trunks share the same suffixes? If it does, what is its length? **)
let trunks_share_same_suffixes_length ((trunk1, trunk2) : trunk * trunk) : int =
  (* if the two trunks are not equal in size, prepends some fillers *)
  let trunk1_only_methods = trunk1 >>| fst and trunk2_only_methods = trunk2 >>| fst in
  let trunk1_length = List.length trunk1 and trunk2_length = List.length trunk2 in
  let trunk1_revised, trunk2_revised =
    match Int.compare trunk1_length trunk2_length with
    | -1 ->
        (* trunk1 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk2_length - trunk1_length) in
        (fillers @ trunk1_only_methods, trunk2_only_methods)
    | 0 ->
        (trunk1_only_methods, trunk2_only_methods)
    | 1 ->
        (* trunk2 is shorter: prepend some fillers *)
        let fillers = List.init ~f:(fun _ -> "filler") (trunk1_length - trunk2_length) in
        (trunk1_only_methods, fillers @ trunk2_only_methods)
    | _ ->
        failwith "this is impossible"
  in
  let zipped = List.zip_exn trunk1_revised trunk2_revised in
  let suffix = List.take_while ~f:(fun (v1, v2) -> String.equal v1 v2) zipped in
  (* if List.length suffix >= 1 then print_endline "same suffixes length!" ; *)
  List.length suffix
