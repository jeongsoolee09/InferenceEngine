open ListMonad
open InfixOperators

exception TODO

module F = Format

let spawn_python (args : string list) (python_file_abs_dir : string) : unit =
  ignore @@ Unix.system @@ F.asprintf "python %s" python_file_abs_dir
