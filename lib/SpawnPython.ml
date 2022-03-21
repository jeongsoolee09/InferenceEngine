open ListMonad
open InfixOperators
module F = Format

let spawn_python ~(args : string list) ~(pyfile : string) : unit =
  let command =
    F.asprintf "python %s %s" pyfile (String.concat @@ List.intersperse ~sep:" " args)
  in
  ignore @@ Unix.system command
