open ListMonad
open InfixOperators
open GraphRepr
open FeatureMaps
open Method

exception TODO

module F = Format

module SingleFeature = struct
  type feature = FeatureMaps.NodeWiseFeatureMap.feature

  type t = Method.t -> feature

  (** Pattern that captures (1) package name, (2) class name, and (3) method name from a unique
      identifier. *)
  let id_regex = Str.regexp "\\(.*\\)\\.?\\([A-Z][a-zA-Z$0-9]+\\)\\.\\([a-zA-Z<>$0-9]+\\)(.*)"

  (* unique_identifiers are strings of the format {package}.{classname}.{method_name}:{return_type_with_package}
     they are obtained from Procname.pp_unique_id. *)

  let find_unique_identifier_of_method (method_ : Method.t) : string =
    let method_classname = get_class_name method_
    and method_method_name = get_method_name method_ in
    List.find_exn
      ~f:(fun unique_id ->
        String.is_substring
          ~substring:(Format.asprintf "%s.%s" method_classname method_method_name)
          unique_id )
      (Deserializer.deserialize_method_txt () @ Deserializer.deserialize_skip_func ())


  let this_project_package_name : string =
    let skip_methods = Deserializer.deserialize_skip_func ()
    and udf_methods = Deserializer.deserialize_method_txt () in
    UniqueID.get_package_name @@ List.hd_exn udf_methods


  let is_this_project_class_initializer (method_ : Method.t) : bool =
    let methstring = Method.to_string method_ in
    let method_lines = Deserializer.deserialize_method_txt () in
    List.exists
      ~f:(fun line ->
        String.is_substring ~substring:methstring line
        && String.is_prefix ~prefix:this_project_package_name line )
      method_lines


  let is_this_project_method (method_ : Method.t) : bool =
    let methstring = Method.to_string method_ in
    if String.is_substring ~substring:"<init>" methstring then
      is_this_project_class_initializer method_
    else
      try
        let this_method_id = find_unique_identifier_of_method method_ in
        let methstring_package = UniqueID.get_package_name this_method_id in
        String.equal methstring_package this_project_package_name
      with _ -> (* It's very likely an interface method *)
                true


  (* NOTE do not use this for a pairwise feature *)
  let is_main_method (method_ : Method.t) : bool =
    let methstring = Method.to_string method_ in
    String.is_substring ~substring:"main(" methstring


  let is_java_builtin_class_initializer (method_ : Method.t) : bool =
    (* first, we read all the skip_func file, *)
    (* find the initializers in it, *)
    (* and see if the methstring is a substring in at least one of them. *)
    let skip_func_lines = Deserializer.deserialize_skip_func () in
    List.exists
      ~f:(fun line ->
        let classname = get_class_name method_ and methodname = get_method_name method_ in
        String.is_substring ~substring:(F.asprintf "%s.%s" classname methodname) line
        && String.is_prefix ~prefix:"java." line )
      skip_func_lines


  let is_java_builtin_method (method_ : Method.t) : bool =
    let methstring = Method.to_string method_ in
    if String.is_substring ~substring:"<init>" methstring then
      is_java_builtin_class_initializer method_
    else
      try
        let this_method_id = find_unique_identifier_of_method method_ in
        let methstring_package = UniqueID.get_package_name this_method_id in
        String.is_prefix ~prefix:"java." methstring_package
      with _ -> (* It's very likely an interface method *)
                true


  let is_framework_method (method_ : Method.t) : bool =
    let methstring = Method.to_string method_ in
    if String.is_substring ~substring:"<init>" methstring then
      not (is_this_project_class_initializer method_ || is_java_builtin_class_initializer method_)
    else not (is_java_builtin_method method_ || is_this_project_method method_)


  let returnval_not_used_in_caller (method_ : Method.t) : bool =
    (* NOTE use partial application on the first two params to type-check *)
    let sexp_loaded = Sexp.parse @@ In_channel.read_all "void_calls.lisp" in
    match sexp_loaded with
    | Done (res, _) ->
        let module LVList = struct
          type t = G.LiteralVertex.t list [@@deriving sexp]
        end in
        let void_call_methods = List.map ~f:fst (LVList.t_of_sexp res) in
        List.mem ~equal:Method.equal void_call_methods method_
    | Cont _ ->
        failwith "sexp parsing error"


  let is_library_code (method_ : Method.t) : bool =
    let classname = get_class_name method_ and methname = get_method_name method_ in
    let classname_methname = F.asprintf "%s.%s" classname methname in
    List.exists
      ~f:(fun line -> String.is_substring ~substring:classname_methname line)
      (Deserializer.deserialize_skip_func ())


  let all_features : t list =
    [ (fun m -> String (get_return_type m))
    ; (fun m -> String (get_class_name m))
    ; (fun m -> String (get_method_name m))
    ; (fun m -> Bool (is_framework_method m))
    ; (fun m -> Bool (is_library_code m))
    ; (fun m -> Bool (returnval_not_used_in_caller m))
    ; (fun m -> Bool (is_initializer m)) ]
end

module PairwiseFeature = struct
  (* methname is the result of Procname.to_string *)
  open SingleFeature

  let is_both_framework_code ((method1, method2) : Method.t * Method.t) : bool =
    is_framework_method method1 && is_framework_method method2


  let belong_to_same_class ((method1, method2) : Method.t * Method.t) : bool =
    (not @@ (is_this_project_method method1 || is_this_project_method method2))
    && String.equal (get_class_name method1) (get_class_name method2)


  let belong_to_same_package ((method1, method2) : Method.t * Method.t) : bool =
    (not @@ (is_this_project_method method1 || is_this_project_method method2))
    && String.equal
         (UniqueID.get_package_name (find_unique_identifier_of_method method1))
         (UniqueID.get_package_name (find_unique_identifier_of_method method2))


  let return_type_is_another's_class ((method1, method2) : Method.t * Method.t) : bool =
    String.equal (get_return_type method1) (get_return_type method2)


  let is_both_java_builtin ((method1, method2) : Method.t * Method.t) : bool =
    let method1_is_init = is_initializer method1 and method2_is_init = is_initializer method2 in
    match (method1_is_init, method2_is_init) with
    | true, true | false, false ->
        is_java_builtin_method method1 && is_java_builtin_method method2
    | _ ->
        false


  let is_both_initializer ((method1, method2) : Method.t * Method.t) : bool =
    let method1_is_init = is_initializer method1 and method2_is_init = is_initializer method2 in
    method1_is_init && method2_is_init


  let all_features =
    [ is_both_framework_code
    ; belong_to_same_class
    ; belong_to_same_package
    ; return_type_is_another's_class
    ; is_both_java_builtin
    ; is_both_initializer ]
end

let run_all_single_features (method_ : Method.t) : SingleFeature.feature list =
  (* TODO: change this to a dataframe *)
  List.rev
  @@ List.fold
       ~f:(fun acc feature ->
         let feature_value = feature method_ in
         feature_value :: acc )
       SingleFeature.all_features ~init:[]


let init_feature_map (graph : G.t) : FeatureMaps.NodeWiseFeatureMap.t =
  List.fold
    ~f:(fun acc meth ->
      FeatureMaps.NodeWiseFeatureMap.strong_update acc meth (run_all_single_features meth) )
    (G.all_non_frontend_methods_of_graph graph)
    ~init:(FeatureMaps.NodeWiseFeatureMap.init graph)
