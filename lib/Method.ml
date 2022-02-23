open ListMonad
open InfixOperators
open DirectoryManager
module F = Format
module Hashtbl = Caml.Hashtbl

type t = String.t [@@deriving compare, equal, sexp]

let dummy = "Dummy Dummy.dummy()"

let is_frontend (method_ : t) : bool =
  String.is_substring ~substring:"$Lambda$" method_
  || String.is_substring ~substring:"lambda$" method_
  || String.is_prefix ~prefix:"__" method_


let is_dunder (method_ : t) : bool = String.is_prefix ~prefix:"__" method_

let hash = Hashtbl.hash

let is_initializer (method_ : t) : bool = String.is_substring ~substring:"<init>" method_

module UniqueID = struct
  (* unique_identifiers are strings of the format {package}.{classname}.{method_name}:{return_type_with_package}
     they are obtained from Procname.pp_unique_id. *)

  let id_regex = Re2.create_exn "(.*)\.([A-Z][a-zA-Z$0-9_]+).([a-zA-Z<>$0-9_]+)\(.*\)(:.*)?"

  let get_package_name (unique_identifier : string) : string =
    try
      assert (Re2.matches id_regex unique_identifier) ;
      let matches =
        Re2.find_submatches_exn id_regex unique_identifier |> Array.to_list |> catMaybes
      in
      List.nth_exn matches 1
    with Assert_failure _ ->
      failwithf "extract_package_name_from_id failed: %s" unique_identifier ()


  let get_class_name (unique_identifier : string) : string =
    try
      assert (Re2.matches id_regex unique_identifier) ;
      let matches =
        Re2.find_submatches_exn id_regex unique_identifier |> Array.to_list |> catMaybes
      in
      List.nth_exn matches 2
    with Assert_failure _ -> failwithf "extract_class_name_from_id: %s" unique_identifier ()


  let get_method_name (unique_identifier : string) : string =
    try
      assert (Re2.matches id_regex unique_identifier) ;
      let matches =
        Re2.find_submatches_exn id_regex unique_identifier |> Array.to_list |> catMaybes
      in
      List.nth_exn matches 3
    with Assert_failure _ -> failwithf "extract_method_name_from_id: %s" unique_identifier ()
end

module NormalString = struct
  (** Pattern that captures (1) rtntype, (2) class name, and (3) method name from a unique
      identifier. *)
  let normalstring_regex = Str.regexp "\\(.+\\) \\([a-zA-Z0-9$_]+\\)\\.\\([a-zA-Z<>0-9$_.]+\\)(.*)"

  let get_return_type (normalstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex normalstring 0) ;
      Str.matched_group 1 normalstring
    with Assert_failure _ -> ""


  let get_class_name (normalstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex normalstring 0) ;
      let out = Str.matched_group 2 normalstring in
      if String.is_substring out ~substring:"$" then
        String.split normalstring ~on:' ' |> List.last_exn |> String.split ~on:'.' |> List.hd_exn
        |> String.split ~on:'$' |> List.last_exn
      else out
    with Assert_failure _ -> ""


  let get_method_name (normalstring : String.t) : string =
    try
      if is_dunder normalstring then normalstring
      else (
        assert (Str.string_match normalstring_regex normalstring 0) ;
        Str.matched_group 3 normalstring )
    with Assert_failure _ -> ""
end

module InitString = struct
  let initstring_regex = Str.regexp "\\([a-zA-Z0-9$_]+\\)\\.\\([a-zA-Z0-9<>$_]+\\)(.*)"

  let get_class_name (initstring : String.t) : string =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      let match_ = Str.matched_group 1 initstring in
      if String.is_substring ~substring:"$" match_ then
        String.take_while ~f:(fun char -> not @@ Char.equal '$' char) match_
      else match_
    with Assert_failure _ -> failwithf "extract_class_name_from_initstring: %s" initstring ()


  let get_method_name (initstring : String.t) : string =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      Str.matched_group 2 initstring
    with Assert_failure _ -> failwithf "extract_method_name_from_initstring: %s" initstring ()
end

let to_string : t -> string = ident

let of_string : t -> string = (* we omit input validation to make it cheap *) ident

let get_return_type (method_ : t) : string =
  if is_initializer method_ then "" (* nothing, bro! *) else NormalString.get_return_type method_


let get_class_name (method_ : t) : string =
  if String.is_prefix method_ ~prefix:"__" then ""
  else if is_initializer method_ then InitString.get_class_name method_
  else NormalString.get_class_name method_


let get_method_name (method_ : t) : string =
  if is_initializer method_ then InitString.get_method_name method_
  else NormalString.get_method_name method_


let is_udf (method_ : t) : bool =
  try
    let classname = get_class_name method_ and methodname = get_method_name method_ in
    let classname_methodname = F.asprintf "%s.%s" classname methodname in
    Array.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methodname line)
      (Array.of_list @@ Deserializer.deserialize_method_txt ())
  with _ -> false


let is_api (method_ : t) : bool =
  (* try *)
  (*   let classname = get_class_name method_ and methodname = get_method_name method_ in *)
  (*   let classname_methodname = F.asprintf "%s.%s" classname methodname in *)
  (*   List.exists *)
  (*     ~f:(fun line -> String.is_substring ~substring:classname_methodname line) *)
  (*     (Deserializer.deserialize_skip_func ()) *)
  (* with _ -> true *)
  if is_dunder method_ then true else not @@ is_udf method_


let is_testcode (method_ : t) : bool =
  List.mem ~equal:String.equal
    (DirectoryManager.Classnames.get_test_classnames (Deserializer.deserialize_config ()))
    (get_class_name method_)


let is_corner_case (method_ : t) : bool = (not @@ is_api method_) && (not @@ is_udf method_)

let find_unique_identifier (method_ : t) : string =
  let method_classname = get_class_name method_ and method_name = get_method_name method_ in
  List.find_exn
    ~f:(fun unique_id ->
      String.is_substring
        ~substring:(Format.asprintf "%s.%s" method_classname method_name)
        unique_id )
    (Deserializer.deserialize_method_txt () @ Deserializer.deserialize_skip_func ())


module PackageResolver = struct
  let resolve_via_import_stmts (method_ : t) : string =
    (* find from the list of scraped packages *)
    let imports_in_directory =
      DirectoryManager.PackageScraper.scrape_imports_in_directory
        (Deserializer.deserialize_config ())
    in
    let method_classname = get_class_name method_ in
    match
      List.find
        ~f:(fun str -> String.equal (List.last_exn (String.split ~on:'.' str)) method_classname)
        imports_in_directory
    with
    | None ->
        failwithf "could not get package name for %s\n" method_ ()
    | Some res ->
        DirectoryManager.PackageScraper.extract_package_from_import_stmt res


  let resolve_via_package_decls (method_ : t) : string =
    if is_initializer method_ then
      let java_filenames =
        DirectoryManager.walk_for_extension (Deserializer.deserialize_config ()) ".java"
      in
      let classname = get_class_name method_ in
      match
        List.find
          ~f:(fun java_filename ->
            let filename_only = List.last_exn @@ String.split ~on:'/' java_filename in
            String.equal java_filename (classname ^ ".java") )
          java_filenames
      with
      | Some java_filename ->
          List.hd_exn
          @@ DirectoryManager.PackageScraper.scrape_package_decls_in_single_file java_filename
      | None ->
          failwithf "resolving via package decls failed: %s\n" method_ ()
    else failwithf "non-initializer method resolving is not yet supported: %s\n" method_ ()
end

let get_package_name (method_ : t) : string =
  try
    let unique_id = find_unique_identifier method_ in
    UniqueID.get_package_name unique_id
  with Not_found_s _ ->
    let other_method_with_same_classname =
      List.find_exn
        ~f:(fun other_method ->
          String.equal (get_class_name method_) (UniqueID.get_class_name other_method) )
        (Deserializer.deserialize_skip_func () @ Deserializer.deserialize_method_txt ())
    in
    UniqueID.get_package_name other_method_with_same_classname


let is_java_method (method_ : t) : bool =
  String.is_prefix ~prefix:"java." (get_package_name method_)


let rtntype_is_void (method_ : t) : bool = String.equal "void" (get_return_type method_)

let is_main_method (method_ : t) : bool = String.is_substring ~substring:"main(" (to_string method_)

let is_well_known_java_method =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        Array.exists
          ~f:(fun (classname, methname) ->
            String.equal classname (get_class_name method_)
            && String.equal methname (get_method_name method_) )
          JavaExpert.all_well_known_methods
    | Some res ->
        res


let is_well_known_java_source_method =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        Array.exists
          ~f:(fun (classname, methname) ->
            String.equal classname (get_class_name method_)
            && String.equal methname (get_method_name method_) )
          JavaExpert.java_source_methods
    | Some res ->
        res


let is_well_known_java_sink_method =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        Array.exists
          ~f:(fun (classname, methname) ->
            String.equal classname (get_class_name method_)
            && String.equal methname (get_method_name method_) )
          JavaExpert.java_sink_methods
    | Some res ->
        res


let is_well_known_java_sanitizer_method =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        Array.exists
          ~f:(fun (classname, methname) ->
            String.equal classname (get_class_name method_)
            && String.equal methname (get_method_name method_) )
          JavaExpert.java_sanitizer_methods
    | Some res ->
        res


let is_well_known_java_none_method =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : bool ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        Array.exists
          ~f:(fun (classname, methname) ->
            String.equal classname (get_class_name method_)
            && String.equal methname (get_method_name method_) )
          JavaExpert.java_none_methods
    | Some res ->
        res


let get_return_stmt_lines =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : int list ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        let out =
          let return_stmt_lines = Deserializer.deserialize_return_stmts () in
          List.Assoc.find_exn return_stmt_lines method_ ~equal
        in
        Hashtbl.add cache method_ out ;
        out
    | Some res ->
        res


let get_declaration_file_candidates =
  let cache = Hashtbl.create 777 in
  fun (method_ : t) : string list ->
    match Hashtbl.find_opt cache method_ with
    | None ->
        let out =
          let this_method_classname = get_class_name method_ in
          let root_dir = Deserializer.deserialize_config () in
          let files_and_their_methods = ClassnameScraper.get_filenames_and_their_classes root_dir in
          List.fold
            ~f:(fun acc (filename, classes) ->
              if List.mem classes this_method_classname ~equal:String.equal then (
                print_endline filename ;
                filename :: acc )
              else acc )
            ~init:[] files_and_their_methods
        in
        Hashtbl.add cache method_ out ;
        out
    | Some res ->
        res


exception TODO

let get_declaration_file (method_ : t) : string =
  (* 1차로, classname을 가진 java file이 있는지 확인. 대부분은 여기서 해결됨 *)
  let this_method_classname = get_class_name method_
  and all_filenames = DirectoryManager.walk_for_extension Deserializer.project_root ".java" in
  let result =
    List.find all_filenames ~f:(fun filename ->
        let filename_short = List.last_exn @@ String.split ~on:'/' filename in
        String.equal filename_short (this_method_classname ^ ".java") )
  in
  if is_some result then Option.value_exn result
  else (
    print_endline "hihi" ;
    (* 1차가 안 되면, 2차로 classes_and_files를 본다. *)
    let all_filenames_and_their_classes =
      ClassnameScraper.get_filenames_and_their_classes Deserializer.project_root
    in
    let result =
      List.find all_filenames_and_their_classes ~f:(fun (filename, classnames) ->
          List.mem ~equal:String.equal classnames this_method_classname )
    in
    match result with
    | None ->
        (* 그래도 안되면 나는 모르겠다 *)
        failwithf "get_declaration_file failed for %s" method_ ()
    | Some (filename, _) ->
        filename )
