exception TODO

module JSON = Yojson.Basic

type delimiters = Start | End

let catMaybes (lst : 'a option list) : 'a list =
  List.rev
  @@ List.fold
       ~f:(fun acc elem_opt -> match elem_opt with None -> acc | Some elem -> elem :: acc)
       ~init:[] lst


let random_elem (list : 'a list) : 'a =
  if List.is_empty list then failwith "cannot select from an empty list" ;
  let random_index = Random.int_incl 0 (List.length list - 1) in
  List.nth_exn list random_index


let rec get_next_elem (lst : 'a list) ~(next_to : 'a) ~(equal : 'a -> 'a -> bool) =
  match lst with
  | [] | [_] ->
      failwith "get_next_elem failed"
  | x1 :: (x2 :: _ as rest) ->
      if equal x1 next_to then x2 else get_next_elem rest ~next_to ~equal
