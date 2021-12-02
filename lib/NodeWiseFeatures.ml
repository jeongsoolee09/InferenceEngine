open ListMonad
open GraphRepr
open FeatureMaps

exception TODO

type vertex = G.V.t

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

  (* for the record, unique ids and methnames are the same thing here. *)

  let extract_package_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 1 unique_identifier)
    with Assert_failure _ -> failwith unique_identifier


  let extract_class_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 2 unique_identifier)
    with Assert_failure _ -> failwith unique_identifier


  let extract_method_name_from_id (unique_identifier : string) : feature =
    try
      assert (Str.string_match id_regex unique_identifier 0) ;
      String (Str.matched_group 3 unique_identifier)
    with Assert_failure _ -> failwith unique_identifier


  (** Pattern that captures (1) rtntype, (2) class name, and (3) method name from a unique
      identifier. *)
  let methstring_regex = Str.regexp "\\(.*\\) ?\\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let extract_rtntype_from_methstring (methstring : string) : feature =
    try
      assert (Str.string_match methstring_regex methstring 0) ;
      String (Str.matched_group 1 methstring)
    with Assert_failure _ -> failwith methstring


  let extract_class_name_from_methstring (methstring : string) : feature =
    try
      assert (Str.string_match methstring_regex methstring 0) ;
      String (Str.matched_group 2 methstring)
    with Assert_failure _ -> failwith methstring


  let extract_method_name_from_methstring (methstring : string) : feature =
    try
      assert (Str.string_match methstring_regex methstring 0) ;
      String (Str.matched_group 3 methstring)
    with Assert_failure _ -> failwith methstring


  let initstring_regex = Str.regexp "\\([a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  let extract_class_name_from_initstring (initstring : string) : feature =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      String (Str.matched_group 1 initstring)
    with Assert_failure _ -> failwith initstring


  let extract_method_name_from_initstring (initstring : string) : feature =
    try
      assert (Str.string_match initstring_regex initstring 0) ;
      String (Str.matched_group 2 initstring)
    with Assert_failure _ -> failwith initstring


  let find_unique_identifier_of_methstring (methstring : string) : string =
    let methstring_classname = extract_class_name_from_methstring methstring
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


  let is_framework_code (methstring : string) : feature =
    let skip_methods = Deserializer.deserialize_skip_func ()
    and udf_methods = Deserializer.deserialize_method_txt () in
    let this_project_package_name =
      string_of_feature @@ extract_package_name_from_id @@ List.hd_exn udf_methods
    in
    let methstring_package =
      find_unique_identifier_of_methstring methstring |> extract_package_name_from_id
    in
    Bool
      (not
         ( String.is_prefix ~prefix:"java." (string_of_feature methstring_package)
         || String.equal (string_of_feature methstring_package) this_project_package_name ) )


  let all_features =
    [ extract_rtntype_from_methstring
    ; extract_class_name_from_methstring
    ; extract_method_name_from_methstring
    ; is_framework_code ]
end

module PairwiseFeature = struct
  (* methname is the result of Procname.to_string *)
  let is_both_framework_code ((method1, method2) : string * string) : bool =
    (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_code method1)
    && (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_code method2)


  let belong_to_same_class ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method2))


  let belong_to_same_package ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method2))


  let return_type_is_another's_class ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_rtntype_from_methstring method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_methstring method2))
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
  let all_methods = G.all_methods_of_graph graph in
  List.fold
    ~f:(fun acc meth ->
      FeatureMaps.NodeWiseFeatureMap.strong_update acc meth (run_all_single_features meth) )
    all_methods
    ~init:(FeatureMaps.NodeWiseFeatureMap.init graph)
