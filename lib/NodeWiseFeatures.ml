open GraphRepr

let ( >>= ) = List.( >>= )

and ( >>| ) = List.( >>| )

and return = List.return

type vertex = G.V.t

module Utils = struct
  (** Pattern that captures (1) package name, (2) class name, and (3) method name. *)
  let regex = Str.regexp "\\(.+\\)\\.\\([A-Z][a-zA-Z$]+\\)\\.\\([a-zA-Z<>$]+\\)(.*)"

  (* unique_identifiers are strings of the format {package}.{classname}.{method_name}:{return_type_with_package}
     they are obtained from Procname.pp_unique_id. *)
  let extract_package_name (unique_identifier : string) : string =
    assert (Str.string_match regex unique_identifier 0) ;
    Str.matched_group 1 unique_identifier


  let extract_class_name (unique_identifier : string) : string =
    assert (Str.string_match regex unique_identifier 0) ;
    Str.matched_group 2 unique_identifier


  let extract_method_name (unique_identifier : string) : string =
    assert (Str.string_match regex unique_identifier 0) ;
    Str.matched_group 3 unique_identifier
end

(* methname is the result of Procname.to_string *)
let is_both_framework_code ((method1, method2) : string * string) : bool =
  let skip_methods = Deserializer.deserialize_method_txt ()
  and udf_methods = Deserializer.deserialize_method_txt () in
  let this_project_package_name = Utils.extract_package_name @@ List.hd_exn udf_methods in
  let not_this_project_methods_and_java_methods =
    List.filter
      ~f:(fun str ->
        not
          ( String.is_substring ~substring:"java." str (* should not be a Java builtin *)
          || String.is_substring ~substring:this_project_package_name str )
        (* should not belong to this package *) )
      skip_methods
  in
  let is_framework_code methname =
    List.mem ~equal:String.equal
      (not_this_project_methods_and_java_methods >>| Utils.extract_method_name)
      (Utils.extract_method_name methname)
  in
  is_framework_code method1 && is_framework_code method2


let belong_to_same_class ((method1, method2) : string * string) : bool =
  String.equal (Utils.extract_class_name method1) (Utils.extract_class_name method2)
