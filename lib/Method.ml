open ListMonad
open InfixOperators
module F = Format

type t = String.t [@@deriving compare, equal, sexp]

let dummy = "Dummy Dummy.dummy()"

let is_frontend (method_ : t) : bool =
  String.is_substring ~substring:"$Lambda$" method_
  || String.is_substring ~substring:"lambda$" method_
  || String.is_prefix ~prefix:"__" method_


let is_initializer (method_ : t) : bool = String.is_substring ~substring:"<init>" method_

module UniqueID = struct
  let id_regex = Str.regexp "\\(.*\\)\\.?\\([A-Z][a-zA-Z$0-9]+\\)\\.\\([a-zA-Z<>$0-9]+\\)(.*)"

  let get_package_name (unique_identifier : string) : string =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      Str.matched_group 1 unique_identifier
    with Assert_failure _ ->
      failwithf "extract_package_name_from_id failed: %s" unique_identifier ()


  let get_class_name (unique_identifier : string) : string =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      let match_ = Str.matched_group 2 unique_identifier in
      if String.is_substring ~substring:"$" match_ then
        (* whoo it's a subclass *)
        String.take_while ~f:(fun char -> not @@ Char.equal '$' char) match_
      else match_
    with Assert_failure _ -> failwithf "extract_class_name_from_id: %s" unique_identifier ()


  let get_method_name (unique_identifier : string) : string =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      Str.matched_group 3 unique_identifier
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
    with Assert_failure _ -> failwithf "extract_rtntype_from_normalstring: %s" normalstring ()


  let get_class_name (normalstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex normalstring 0) ;
      let match_ = Str.matched_group 2 normalstring in
      if String.is_substring ~substring:"$" match_ then
        (* whoo it's a subclass *)
        String.take_while ~f:(fun char -> not @@ Char.equal '$' char) match_
      else match_
    with Assert_failure _ -> failwithf "extract_class_name_from_normalstring: %s" normalstring ()


  let get_method_name (normalstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex normalstring 0) ;
      Str.matched_group 3 normalstring
    with Assert_failure _ -> failwithf "extract_method_name_from_normalstring: %s" normalstring ()
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

let to_string (method_ : t) : string = method_

let of_string (string : String.t) : t =
  (* assert ( *)
  (*   Str.string_match NormalString.normalstring_regex string 0 *)
  (*   || Str.string_match InitString.initstring_regex string 0 ) ; *)
  string


let get_return_type (method_ : t) : string =
  if is_initializer method_ then "" (* nothing, bro! *) else NormalString.get_return_type method_


let get_class_name (method_ : t) : string =
  if is_initializer method_ then InitString.get_class_name method_
  else NormalString.get_class_name method_


let get_method_name (method_ : t) : string =
  if is_initializer method_ then InitString.get_method_name method_
  else NormalString.get_method_name method_


let is_api (method_ : t) : bool =
  try
    let classname = get_class_name method_ and methodname = get_method_name method_ in
    let classname_methodname = F.asprintf "%s.%s" classname methodname in
    List.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methodname line)
      (Deserializer.deserialize_skip_func ())
  with _ -> true


let is_udf (method_ : t) : bool =
  try
    let classname = get_class_name method_ and methodname = get_method_name method_ in
    let classname_methodname = F.asprintf "%s.%s" classname methodname in
    List.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methodname line)
      (Deserializer.deserialize_method_txt ())
  with _ -> false


let is_corner_case (method_ : t) : bool = (not @@ is_api method_) && (not @@ is_udf method_)

let find_unique_identifier (method_ : t) : string =
  let method_classname = get_class_name method_ and method_name = get_method_name method_ in
  List.find_exn
    ~f:(fun unique_id ->
      String.is_substring
        ~substring:(Format.asprintf "%s.%s" method_classname method_name)
        unique_id )
    (Deserializer.deserialize_method_txt () @ Deserializer.deserialize_skip_func ())
