open ListMonad
open InfixOperators

module F = Format

let spawn_python ~(args : string list) ~(pyfile : string) : unit =
  ignore @@ Unix.system @@ F.asprintf "python %s" pyfile
