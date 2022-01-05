open ListMonad
open InfixOperators
module F = Format
module Hashtbl = Caml.Hashtbl

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


(** does this directory have any java file (recursively?) *)
let has_java (dir : string) : bool = not @@ List.is_empty @@ walk_for_extension dir ".java"

(** get the subdirs containing .java files. *)
let get_compilation_unit_absdirs (root_dir : string) : string list =
  Sys.readdir root_dir
  |> Array.map ~f:(fun subdir -> root_dir ^ subdir)
  |> Array.filter ~f:is_directory |> Array.filter ~f:has_java |> Array.to_list


let get_compilation_unit_subdirs (root_dir : string) : string list =
  let absdirs = get_compilation_unit_absdirs root_dir in
  List.map ~f:(fun absdir -> List.last_exn @@ String.split ~on:'/' absdir) absdirs


module ClassnameScraper = struct
  let is_class_decl (line : string) : bool =
    let stripped = String.strip line in
    let regexp = Str.regexp "[a-z ]*[a-zA-Z@.() ]*class .*{" in
    Str.string_match regexp stripped 0


  let extract_classname_from_class_decl (class_decl : string) : string =
    let rec inner (splitted : string list) : string =
      match splitted with
      | this :: next :: t when String.equal this "class" ->
          next
      | this :: t when not @@ String.equal this "class" ->
          inner t
      | _ ->
          failwith @@ F.asprintf "could not find classname from class declaration: %s\n" class_decl
    in
    inner @@ String.split ~on:' ' class_decl


  let scrape_classes_in_single_file =
    let cache = Caml.Hashtbl.create 777 in
    fun (file_absdir : string) : string list ->
      match Hashtbl.find_opt cache file_absdir with
      | None ->
          let out =
            let file_line_by_line = In_channel.read_lines file_absdir in
            let class_decl_lines = List.filter file_line_by_line ~f:is_class_decl in
            class_decl_lines >>| extract_classname_from_class_decl
          in
          Hashtbl.add cache file_absdir out ;
          out
      | Some res ->
          res


  let scrape_classes_in_directory (root_dir : string) : string list =
    let java_files = walk_for_extension root_dir ".java" in
    java_files >>= scrape_classes_in_single_file
end

module Classnames = struct
  let classnames_by_compilation_unit =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : (string * string list) list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let absdirs = get_compilation_unit_absdirs root_dir in
          let out =
            absdirs
            >>| fun absdir ->
            let classnames_in_this_absdir =
              walk_for_extension absdir ".java" >>= ClassnameScraper.scrape_classes_in_single_file
            in
            (absdir, classnames_in_this_absdir)
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res


  let get_test_classnames (root_dir : string) =
    walk_for_extension root_dir ".java"
    |> List.filter ~f:(String.is_substring ~substring:"/test/")
    >>| extract_filename_without_extension_and_dirs


  let get_comp_unit_classnames (root_dir : string) (comp_unit : string) =
    walk_for_extension root_dir ".java"
    |> List.filter ~f:(String.is_substring ~substring:(Format.asprintf "/%s/" comp_unit))
    >>| extract_filename_without_extension_and_dirs
end
