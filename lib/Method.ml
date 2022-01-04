open ListMonad
open InfixOperators
module F = Format

type kind = UDF of {comp_unit: string; methname: string} | API of {methname: string}
[@@deriving compare, equal, sexp]

type t = NormalMethod of kind | Initializer of kind [@@deriving compare, equal, sexp]

let dummy_kind : kind = UDF {comp_unit= ""; methname= "Dummy Dummy.dummy()"}

let dummy = NormalMethod dummy_kind

let get_kind (method_ : t) : kind =
  match method_ with NormalMethod kind | Initializer kind -> kind


let is_frontend (string : String.t) : bool =
  String.is_substring ~substring:"$Lambda$" string
  || String.is_substring ~substring:"lambda$" string
  || String.is_prefix ~prefix:"__" string


let is_initializer_methname (string : String.t) : bool =
  String.is_substring ~substring:"<init>" string


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

  let get_return_type (methstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      Str.matched_group 1 methstring
    with Assert_failure _ -> failwithf "extract_rtntype_from_normalstring: %s" methstring ()


  let get_class_name (methstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      let match_ = Str.matched_group 2 methstring in
      if String.is_substring ~substring:"$" match_ then
        String.take_while ~f:(fun char -> not @@ Char.equal '$' char) match_
      else match_
    with Assert_failure _ -> failwithf "extract_class_name_from_normalstring: %s" methstring ()


  let get_method_name (methstring : String.t) : string =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      Str.matched_group 3 methstring
    with Assert_failure _ ->
      failwithf "extract_methstringname_from_normalstring: %s" methstring ()
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

let get_return_type_of_methname (methname : String.t) : string =
  if is_initializer_methname methname then "" (* nothing, bro! *)
  else NormalString.get_return_type methname


let get_class_name_of_methname (methname : String.t) : string =
  if is_initializer_methname methname then InitString.get_class_name methname
  else NormalString.get_class_name methname


let get_method_name_of_methname (methname : String.t) : string =
  if is_initializer_methname methname then InitString.get_method_name methname
  else NormalString.get_method_name methname


let get_return_type (method_ : t) : string =
  match method_ with
  | NormalMethod (UDF {methname}) | NormalMethod (API {methname}) ->
      NormalString.get_return_type methname
  | Initializer (UDF {methname}) | Initializer (API {methname}) ->
      ""


let get_class_name (method_ : t) : string =
  match method_ with
  | NormalMethod (UDF {methname}) | NormalMethod (API {methname}) ->
      NormalString.get_class_name methname
  | Initializer (UDF {methname}) | Initializer (API {methname}) ->
      InitString.get_class_name methname


let get_method_name (method_ : t) : string =
  match method_ with
  | NormalMethod (UDF {methname}) | NormalMethod (API {methname}) ->
      NormalString.get_method_name methname
  | Initializer (UDF {methname}) | Initializer (API {methname}) ->
      InitString.get_method_name methname


let string_list_to_string stringlist =
  "[" ^ List.fold stringlist ~f:(fun acc string -> acc ^ string ^ ", ") ~init:"" ^ "]"


let comp_unit_of_methname (methname : String.t) : String.t =
  let root_dir = Deserializer.deserialize_config () in
  let abs_dirs_and_classnames =
    DirectoryManager.Classnames.classnames_by_compilation_unit root_dir
  in
  let out =
    List.find
      ~f:(fun (abs_dir, classnames) ->
        let class_name = get_class_name_of_methname methname in
        List.mem classnames class_name ~equal:String.equal )
      abs_dirs_and_classnames
  in
  match out with
  | None ->
      failwithf "Could not find comp unit for %s" methname ()
  | Some (abs_dir, _) ->
      List.last_exn @@ String.split ~on:'/' abs_dir


let is_api_code_methname (methname : String.t) : bool =
  try
    let classname = get_class_name_of_methname methname
    and methname = get_method_name_of_methname methname in
    let classname_methname = F.asprintf "%s.%s" classname methname in
    List.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methname line)
      (Deserializer.deserialize_skip_func ())
  with _ -> true


let is_udf_code_methname (methname : String.t) : bool =
  try
    let classname = get_class_name_of_methname methname
    and methname = get_method_name_of_methname methname in
    let classname_methname = F.asprintf "%s.%s" classname methname in
    List.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methname line)
      (Deserializer.deserialize_method_txt ())
  with _ -> false


let corner_case (methname : String.t) : bool =
  (not @@ is_api_code_methname methname) && (not @@ is_udf_code_methname methname)


let is_udf_code_of_this_comp_unit (method_ : t) (comp_unit : string) : bool =
  match method_ with
  | NormalMethod (UDF {comp_unit= this_comp_unit}) | Initializer (UDF {comp_unit= this_comp_unit})
    ->
      String.equal this_comp_unit comp_unit
  | _ ->
      false


let to_string (method_ : t) : string =
  match method_ with
  | NormalMethod (UDF {methname})
  | NormalMethod (API {methname})
  | Initializer (UDF {methname})
  | Initializer (API {methname}) ->
      methname


let is_api_method (method_ : t) : bool =
  let methname = to_string method_ in
  let classname = get_class_name_of_methname methname
  and methname = get_method_name_of_methname methname in
  let classname_methname = F.asprintf "%s.%s" classname methname in
  List.exists
    ~f:(fun line -> String.is_substring ~substring:classname_methname line)
    (Deserializer.deserialize_skip_func ())


let of_string (string : String.t) : t =
  let kind : kind =
    if is_udf_code_methname string then
      UDF {comp_unit= comp_unit_of_methname string; methname= string}
    else API {methname= string}
  in
  if is_initializer_methname string then Initializer kind else NormalMethod kind


let find_unique_identifier_of_method (methname : String.t) : string =
  let method_classname = get_class_name_of_methname methname
  and method_method_name = get_method_name_of_methname methname in
  List.find_exn
    ~f:(fun unique_id ->
      String.is_substring
        ~substring:(Format.asprintf "%s.%s" method_classname method_method_name)
        unique_id )
    (Deserializer.deserialize_method_txt () @ Deserializer.deserialize_skip_func ())


let is_initializer = function Initializer _ -> true | _ -> false
