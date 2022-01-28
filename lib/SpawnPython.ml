open ListMonad
open InfixOperators
module F = Format

let spawn_python ~(args : string list) ~(pyfile : string) : unit =
  let command =
    F.asprintf "python %s" pyfile
    ^ List.fold ~f:(fun acc arg -> acc ^ " " ^ arg ^ " ") ~init:"" args
  in
  ignore @@ Unix.system command
