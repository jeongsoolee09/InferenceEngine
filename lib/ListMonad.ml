let ( >>= ) = List.( >>= )

let ( >>| ) = List.( >>| )

let return = List.return

let ( let* ) lst f = match lst with [] -> [] | _ -> List.join @@ List.map ~f lst

let catMaybes (lst : 'a option list) : 'a list =
  List.rev
  @@ List.fold
       ~f:(fun acc elem_opt -> match elem_opt with None -> acc | Some elem -> elem :: acc)
       ~init:[] lst
