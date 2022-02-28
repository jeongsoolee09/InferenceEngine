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
  let regexp = Re2.create_exn "[a-z ]*(@[a-zA-Z.() ]*)? ?(class|enum)+ ([a-zA-Z]+).*{"

  let is_class_decl (line : string) : bool =
    let stripped = String.strip line in
    Re2.matches regexp stripped


  let extract_classname_from_class_decl (class_decl : string) : string =
    Re2.find_submatches_exn regexp class_decl |> Array.to_list |> catMaybes |> List.last_exn


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


  let get_filenames_and_their_classes (root_dir : string) : (string * string list) list =
    let java_files = walk_for_extension root_dir ".java" in
    java_files >>| fun java_file -> (java_file, scrape_classes_in_single_file java_file)
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


  let get_test_classnames =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : string list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let out =
            walk_for_extension root_dir ".java"
            |> List.filter ~f:(String.is_substring ~substring:"/test/")
            >>| extract_filename_without_extension_and_dirs
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res


  let get_comp_unit_classnames (root_dir : string) (comp_unit : string) =
    walk_for_extension root_dir ".java"
    |> List.filter ~f:(String.is_substring ~substring:(Format.asprintf "/%s/" comp_unit))
    >>| extract_filename_without_extension_and_dirs
end

module PackageScraper = struct
  let import_regex = Re2.create_exn "import ([a-z.]+\.[A-Za-z]+);"

  let is_import_stmt = Re2.matches import_regex

  let extract_package_from_import_stmt (import_stmt : string) : string =
    String.split ~on:' ' import_stmt |> List.last_exn |> String.rstrip ~drop:(Char.equal ';')


  let scrape_imports_in_single_file =
    let cache = Hashtbl.create 777 in
    fun (file_absdir : string) : string list ->
      match Hashtbl.find_opt cache file_absdir with
      | None ->
          let out =
            let file_line_by_line = In_channel.read_lines file_absdir in
            let import_lines = List.filter file_line_by_line ~f:is_import_stmt in
            import_lines >>| extract_package_from_import_stmt
          in
          Hashtbl.add cache file_absdir out ;
          out
      | Some res ->
          res


  let scrape_imports_in_directory =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : string list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let out =
            let java_files = walk_for_extension root_dir ".java" in
            java_files >>= scrape_imports_in_single_file
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res


  let package_regex = Re2.create_exn "package ([a-zA-Z.]+);"

  let is_package_decl = Re2.matches package_regex

  let extract_package_from_package_decl package_decl =
    String.split ~on:' ' package_decl |> List.last_exn |> String.rstrip ~drop:(Char.equal ';')


  let scrape_package_decls_in_single_file =
    let cache = Hashtbl.create 777 in
    fun (file_absdir : string) : string list ->
      match Hashtbl.find_opt cache file_absdir with
      | None ->
          let out =
            let file_line_by_line = In_channel.read_lines file_absdir in
            let package_decl_lines = List.filter file_line_by_line ~f:is_package_decl in
            package_decl_lines >>| extract_package_from_import_stmt
          in
          Hashtbl.add cache file_absdir out ;
          out
      | Some res ->
          res


  let scrape_package_decls_in_directory =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : string list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let out =
            let java_files = walk_for_extension root_dir ".java" in
            java_files >>= scrape_package_decls_in_single_file
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res
end

module InterfaceScraper = struct
  let regexp = Re2.create_exn "[a-z ]*([@a-zA-Z.()]+)? ?implements ([a-zA-Z<>]+).*{"

  let is_implements_decl = Re2.matches regexp

  let has_implements_decl = List.exists ~f:is_implements_decl

  let extract_class_interface_pair_from_implements_decl (implements_decl : string) : string * string
      =
    if not @@ is_implements_decl implements_decl then
      raise @@ Invalid_argument (F.asprintf "%s is not an implements decl" implements_decl)
    else
      let matches = Re2.find_submatches_exn regexp implements_decl in
      let classname = Option.value_exn matches.(1)
      and interfacename = Option.value_exn matches.(2) in
      (classname, interfacename)


  let scrape_class_interface_pair_from_single_file =
    let cache = Hashtbl.create 777 in
    fun (file_absdir : string) : (string * string) option ->
      match Hashtbl.find_opt cache file_absdir with
      | None ->
          let out =
            let file_line_by_line = In_channel.read_lines file_absdir in
            let implements_line_opt = List.find file_line_by_line ~f:is_implements_decl in
            match implements_line_opt with
            | None ->
                None
            | Some line ->
                Some (extract_class_interface_pair_from_implements_decl line)
          in
          Hashtbl.add cache file_absdir out ;
          out
      | Some res ->
          res


  let scrape_class_interface_pairs_from_directory =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : (string * string) list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let out =
            let java_files = walk_for_extension root_dir ".java" in
            List.fold java_files
              ~f:(fun acc java_file ->
                match scrape_class_interface_pair_from_single_file java_file with
                | None ->
                    acc
                | Some res ->
                    res :: acc )
              ~init:[]
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res


  let scrape_implemented_interfaces_from_directory =
    let cache = Hashtbl.create 777 in
    fun (root_dir : string) : string list ->
      match Hashtbl.find_opt cache root_dir with
      | None ->
          let out =
            scrape_class_interface_pairs_from_directory root_dir
            |> List.fold
                 ~f:(fun acc (_, interfacename) ->
                   let normalized =
                     String.take_while interfacename ~f:(fun c -> not @@ Char.equal '<' c)
                   in
                   if not @@ List.mem ~equal:String.equal acc normalized then normalized :: acc
                   else acc )
                 ~init:[]
          in
          Hashtbl.add cache root_dir out ;
          out
      | Some res ->
          res

  (* let scrape_interfaces_from_directory = *)
end
