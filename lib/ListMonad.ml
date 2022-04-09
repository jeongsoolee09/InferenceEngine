let ( >>= ) = List.( >>= )

let ( >>| ) = List.( >>| )

let return = List.return

let ( let* ) lst f = match lst with [] -> [] | _ -> List.join @@ List.map ~f lst
