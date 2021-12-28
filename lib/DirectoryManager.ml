open ListMonad
open InfixOperators

let is_directory (abs_dir : string) : bool =
  match Sys.is_directory abs_dir with `Yes -> true | _ -> false


let walk_for_extension (root_dir : string) (extension : string) : string list =
  let rec inner (current_dir : string) (filename_acc : string list) =
    let subdirectories =
      Array.filter ~f:is_directory
        (Array.map ~f:(fun name -> current_dir ^ "/" ^ name) (Sys.readdir current_dir))
    in
    let files_matching_extension =
      Array.filter ~f:(not << is_directory)
        (Array.map ~f:(fun name -> current_dir ^ "/" ^ name) (Sys.readdir current_dir))
      |> Array.fold
           ~f:(fun acc elem -> if String.is_suffix elem ~suffix:extension then elem :: acc else acc)
           ~init:[]
    in
    if Array.is_empty subdirectories then filename_acc @ files_matching_extension
    else
      Array.fold subdirectories
        ~f:(fun acc subdirectory -> inner subdirectory acc)
        ~init:(filename_acc @ files_matching_extension)
  in
  inner root_dir []


let extract_filename_without_extension_and_dirs (full_filename : string) : string =
  let filename_only = List.last_exn @@ String.split ~on:'/' full_filename in
  List.hd_exn @@ String.split ~on:'.' filename_only


let get_test_classnames (root_dir : string) =
  walk_for_extension root_dir ".java"
  |> List.filter ~f:(String.is_substring ~substring:"/test/")
  >>| extract_filename_without_extension_and_dirs


(** does this directory have any java file (recursively?) *)
let has_java (dir : string) : bool = not @@ List.is_empty @@ walk_for_extension dir ".java"

(** get the subdirs containing .java files. *)
let get_compilation_unit_subdirs (root_dir : string) : string list =
  Sys.readdir root_dir
  |> Array.map ~f:(fun subdir -> root_dir ^ "/" ^ subdir)
  |> Array.filter ~f:is_directory |> Array.filter ~f:has_java |> Array.to_list
