open ListMonad
open GraphRepr
open FeatureMaps

type vertex = G.V.t

exception TODO

module SingleFeature = struct
  type feature = FeatureMaps.NodeWiseFeatureMap.feature

  type t = string -> feature

  let int_of_feature feature =
    match feature with
    | FeatureMaps.NodeWiseFeatureMap.Int int ->
        int
    | _ ->
        failwith "not an int feature"


  let string_of_feature feature =
    match feature with
    | FeatureMaps.NodeWiseFeatureMap.String str ->
        str
    | _ ->
        failwith "not an string feature"


  let bool_of_feature feature =
    match feature with
    | FeatureMaps.NodeWiseFeatureMap.Bool bool ->
        bool
    | _ ->
        failwith "not an bool feature"


  (** Pattern that captures (1) package name, (2) class name, and (3) method name from a unique
      identifier. *)
  let id_regex = Str.regexp "\\(.*\\)\\.?\\([A-Z][a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  (* unique_identifiers are strings of the format {package}.{classname}.{method_name}:{return_type_with_package}
     they are obtained from Procname.pp_unique_id. *)

  let extract_package_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 1 unique_identifier)
    with Assert_failure _ ->
      failwithf "extract_package_name_from_id failed: %s" unique_identifier ()


  let extract_class_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 2 unique_identifier)
    with Assert_failure _ -> failwithf "extract_class_name_from_id: %s" unique_identifier ()


  let extract_method_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 3 unique_identifier)
    with Assert_failure _ -> failwithf "extract_method_name_from_id: %s" unique_identifier ()


  (** Pattern that captures (1) rtntype, (2) class name, and (3) method name from a unique
      identifier. *)
  let normalstring_regex = Str.regexp "\\(.*\\) \\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let extract_rtntype_from_normalstring (methstring : string) : feature =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      String (Str.matched_group 1 methstring)
    with Assert_failure _ -> failwithf "extract_rtntype_from_normalstring: %s" methstring ()


  let extract_class_name_from_normalstring (methstring : string) : feature =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      String (Str.matched_group 2 methstring)
    with Assert_failure _ -> failwithf "extract_class_name_from_normalstring: %s" methstring ()


  let extract_method_name_from_normalstring (methstring : string) : feature =
    try
      assert (Str.string_match normalstring_regex methstring 0) ;
      String (Str.matched_group 3 methstring)
    with Assert_failure _ -> failwithf "extract_method_name_from_normalstring: %s" methstring ()


  let initstring_regex = Str.regexp "\\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let extract_class_name_from_initstring (initstring : string) : feature =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      String (Str.matched_group 1 initstring)
    with Assert_failure _ -> failwithf "extract_class_name_from_initstring: %s" initstring ()


  let extract_method_name_from_initstring (initstring : string) : feature =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      String (Str.matched_group 2 initstring)
    with Assert_failure _ -> failwithf "extract_method_name_from_initstring: %s" initstring ()


  let extract_rtntype_from_methstring (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then String "" (* nothing, bro! *)
    else extract_rtntype_from_normalstring methstring


  let extract_class_name_from_methstring (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then
      extract_class_name_from_initstring methstring
    else extract_class_name_from_normalstring methstring


  let extract_method_name_from_methstring (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then
      extract_method_name_from_initstring methstring
    else extract_method_name_from_normalstring methstring


  let find_unique_identifier_of_methstring (methstring : string) : string =
    let methstring_classname = extract_class_name_from_normalstring methstring
    and methstring_method_name = extract_method_name_from_methstring methstring in
    List.find_exn
      ~f:(fun unique_id ->
        String.is_substring
          ~substring:
            (F.asprintf "%s.%s"
               (string_of_feature methstring_classname)
               (string_of_feature methstring_method_name) )
          unique_id )
      (Deserializer.deserialize_method_txt () @ Deserializer.deserialize_skip_func ())


  let this_project_package_name =
    let skip_methods = Deserializer.deserialize_skip_func ()
    and udf_methods = Deserializer.deserialize_method_txt () in
    string_of_feature @@ extract_package_name_from_id @@ List.hd_exn udf_methods


  let is_this_project_class_initializer (methstring : string) : feature =
    let method_lines = Deserializer.deserialize_method_txt () in
    Bool
      (List.exists
         ~f:(fun line ->
           String.is_substring ~substring:methstring line
           && String.is_prefix ~prefix:this_project_package_name line )
         method_lines )


  let is_this_project_method (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then
      is_this_project_class_initializer methstring
    else
      let this_method_id = find_unique_identifier_of_methstring methstring in
      let methstring_package = extract_package_name_from_id this_method_id in
      Bool (String.equal (string_of_feature methstring_package) this_project_package_name)


  let is_java_builtin_class_initializer (methstring : string) : feature =
    (* first, we read all the skip_func file, *)
    (* find the initializers in it, *)
    (* and see if the methstring is a substring in at least one of them. *)
    let skip_func_lines = Deserializer.deserialize_skip_func () in
    Bool
      (List.exists
         ~f:(fun line ->
           let classname = string_of_feature @@ extract_class_name_from_initstring methstring
           and methodname = string_of_feature @@ extract_method_name_from_initstring methstring in
           String.is_substring ~substring:(F.asprintf "%s.%s" classname methodname) line
           && String.is_prefix ~prefix:"java." line )
         skip_func_lines )


  let is_java_builtin_method (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then
      is_java_builtin_class_initializer methstring
    else
      let this_method_id = find_unique_identifier_of_methstring methstring in
      let methstring_package = extract_package_name_from_id this_method_id in
      Bool (String.is_prefix ~prefix:"java." (string_of_feature methstring_package))


  let is_framework_method (methstring : string) : feature =
    if String.is_substring ~substring:"<init>" methstring then
      Bool
        (not
           ( (bool_of_feature @@ is_this_project_class_initializer methstring)
           || (bool_of_feature @@ is_java_builtin_class_initializer methstring) ) )
    else
      Bool
        (not
           ( (bool_of_feature @@ is_java_builtin_method methstring)
           || (bool_of_feature @@ is_this_project_method methstring) ) )


  let returnval_not_used_in_caller (methstring : string) : feature =
    (* NOTE use partial application on the first two params to type-check *)
    let sexp_loaded = Sexp.parse @@ In_channel.read_all "void_calls.lisp" in
    match sexp_loaded with
    | Done (res, _) ->
        let module LVList = struct
          type t = G.LiteralVertex.t list [@@deriving sexp]
        end in
        let void_call_methods = List.map ~f:fst (LVList.t_of_sexp res) in
        Bool (List.mem ~equal:String.equal void_call_methods methstring)
    | Cont _ ->
        failwith "sexp parsing error"


  let is_library_code (methstring : string) : feature =
    let classname = string_of_feature @@ extract_class_name_from_methstring methstring
    and methname = string_of_feature @@ extract_method_name_from_methstring methstring in
    let classname_methname = F.asprintf "%s.%s" classname methname in
    Bool
      (List.exists
         ~f:(fun line -> String.is_substring ~substring:classname_methname line)
         (Deserializer.deserialize_skip_func ()) )


  let is_initializer (methstring : string) : feature =
    Bool (String.is_substring ~substring:"<init>" methstring)


  let all_features =
    [ extract_rtntype_from_methstring
    ; extract_class_name_from_methstring
    ; extract_method_name_from_methstring
    ; is_framework_method
    ; is_library_code
    ; returnval_not_used_in_caller
    ; is_initializer ]
end

module PairwiseFeature = struct
  (* methname is the result of Procname.to_string *)
  let is_both_framework_code ((method1, method2) : string * string) : bool =
    (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_method method1)
    && (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_method method2)


  let belong_to_same_class ((method1, method2) : string * string) : bool =
    not
    @@ ( (SingleFeature.bool_of_feature @@ SingleFeature.is_this_project_method method1)
       || (SingleFeature.bool_of_feature @@ SingleFeature.is_this_project_method method2) )
    && String.equal
         (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method1))
         (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method2))


  let belong_to_same_package ((method1, method2) : string * string) : bool =
    not
    @@ ( (SingleFeature.bool_of_feature @@ SingleFeature.is_this_project_method method1)
       || (SingleFeature.bool_of_feature @@ SingleFeature.is_this_project_method method2) )
    && String.equal
         (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method1))
         (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method2))


  let return_type_is_another's_class ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_rtntype_from_methstring method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_methstring method2))


  let is_both_java_builtin ((method1, method2) : string * string) : bool =
    let method1_is_init = String.is_substring ~substring:"<init>" method1
    and method2_is_init = String.is_substring ~substring:"<init>" method2 in
    match (method1_is_init, method2_is_init) with
    | true, true | false, false ->
        SingleFeature.bool_of_feature (SingleFeature.is_java_builtin_method method1)
        && SingleFeature.bool_of_feature (SingleFeature.is_java_builtin_method method2)
    | _ ->
        false


  let is_both_initializer ((method1, method2) : string * string) : bool =
    let method1_is_init = String.is_substring ~substring:"<init>" method1
    and method2_is_init = String.is_substring ~substring:"<init>" method2 in
    method1_is_init && method2_is_init


  let all_features =
    [ is_both_framework_code
    ; belong_to_same_class
    ; belong_to_same_package
    ; return_type_is_another's_class
    ; is_both_java_builtin
    ; is_both_initializer ]
end

let run_all_single_features (methname : string) : SingleFeature.feature list =
  (* TODO: change this to a dataframe *)
  List.rev
  @@ List.fold
       ~f:(fun acc feature ->
         let feature_value = feature methname in
         feature_value :: acc )
       SingleFeature.all_features ~init:[]


let init_feature_map (graph : G.t) : FeatureMaps.NodeWiseFeatureMap.t =
  List.fold
    ~f:(fun acc meth ->
      FeatureMaps.NodeWiseFeatureMap.strong_update acc meth (run_all_single_features meth) )
    (G.all_methods_of_graph graph)
    ~init:(FeatureMaps.NodeWiseFeatureMap.init graph)
