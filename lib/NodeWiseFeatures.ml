open ListMonad
open GraphRepr
open FeatureMaps

exception TODO

type vertex = G.V.t

module SingleFeature = struct
  type feature = FeatureMaps.NodeWiseFeatureMap.feature

  type t = string -> feature

  let int_of_feature feature =
    match feature with FeatureMaps.NodeWiseFeatureMap.Int int -> int | _ -> failwith "not an int feature"


  let string_of_feature feature =
    match feature with FeatureMaps.NodeWiseFeatureMap.String str -> str | _ -> failwith "not an string feature"


  let bool_of_feature feature =
    match feature with FeatureMaps.NodeWiseFeatureMap.Bool bool -> bool | _ -> failwith "not an bool feature"
  (** Pattern that captures (1) package name, (2) class name, and (3) method name from a unique
      identifier. *)
  let id_regex = Str.regexp "\\(.*\\)\\.?\\([A-Z][a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  (* unique_identifiers are strings of the format {package}.{classname}.{method_name}:{return_type_with_package}
     they are obtained from Procname.pp_unique_id. *)

  (* for the record, unique ids and methnames are the same thing here. *)

  let extract_package_name_from_id (unique_identifier : string) : feature =
    assert (Str.string_match id_regex unique_identifier 0) ;
    String (Str.matched_group 1 unique_identifier)


  let extract_class_name_from_id (unique_identifier : string) : feature =
    assert (Str.string_match id_regex unique_identifier 0) ;
    String (Str.matched_group 2 unique_identifier)


  let extract_method_name_from_id (unique_identifier : string) : feature =
    assert (Str.string_match id_regex unique_identifier 0) ;
    String (Str.matched_group 3 unique_identifier)


  let is_framework_code methname =
    let skip_methods = Deserializer.deserialize_skip_func ()
    and udf_methods = Deserializer.deserialize_method_txt () in
    let this_project_package_name =
      string_of_feature @@ extract_package_name_from_id @@ List.hd_exn udf_methods in
    let not_this_project_methods_and_java_methods =
      List.filter
        ~f:(fun str ->
          not
            ( String.is_prefix ~prefix:"java." str (* should not be a Java builtin *)
            || String.is_substring ~substring:this_project_package_name str
               (* should not belong to this package *) ) )
        skip_methods
    in
    FeatureMaps.NodeWiseFeatureMap.Bool (List.mem ~equal:String.equal
      (not_this_project_methods_and_java_methods >>| extract_method_name_from_id >>| string_of_feature)
      (string_of_feature @@ extract_method_name_from_id methname))
end

module PairwiseFeature = struct
  (* methname is the result of Procname.to_string *)
  let is_both_framework_code ((method1, method2) : string * string) : bool =
    (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_code method1) && (SingleFeature.bool_of_feature @@ SingleFeature.is_framework_code method2)


  let belong_to_same_class ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_class_name_from_id method2))


  let belong_to_same_package ((method1, method2) : string * string) : bool =
    String.equal
      (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method1))
      (SingleFeature.string_of_feature (SingleFeature.extract_package_name_from_id method2))
end

let all_single_features = [
  SingleFeature.extract_package_name_from_id
; SingleFeature.extract_class_name_from_id
; SingleFeature.extract_method_name_from_id
; SingleFeature.is_framework_code
]


let run_all_single_features (methname: string)  : SingleFeature.feature list =
  (* Possible TODO: change this to a dataframe *)
  List.rev
  @@ List.fold
       ~f:(fun acc feature ->
         let feature_value = feature methname in
         feature_value :: acc )
       all_single_features ~init:[]


let init_feature_map (graph : G.t) : FeatureMaps.NodeWiseFeatureMap.t =
  let all_methods = G.all_methods_of_graph graph in
  List.fold
    ~f:(fun acc meth ->
        FeatureMaps.NodeWiseFeatureMap.strong_update acc meth (run_all_single_features meth) )
    all_methods ~init:(FeatureMaps.NodeWiseFeatureMap.init graph)
