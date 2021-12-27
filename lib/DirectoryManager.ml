open ListMonad
open InfixOperators

let walk_for_extension (root_dir : string) (extension : string) : string list =
  let rec inner (current_dir : string) (filename_acc : string list) =
    let subdirectories =
      Array.filter
        ~f:(fun name -> match Sys.is_directory name with `Yes -> true | _ -> false)
        (Array.map ~f:(fun name -> current_dir ^ "/" ^ name) (Sys.readdir current_dir))
    in
    let files_matching_extension =
      Array.filter
        ~f:(fun name -> match Sys.is_directory name with `No -> true | _ -> false)
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
