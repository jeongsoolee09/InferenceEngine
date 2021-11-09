open GraphRepr

let ( >>= ) = List.( >>= )

and ( >>| ) = List.( >>| )

and return = List.return

module Utils = struct
  (** Pattern that captures (1) package name, (2) class name, and (3) method name. *)
  let regex = Str.regexp "\\(.+\\)\\.\\([A-Z][a-zA-Z]+\\)\\.\\([a-zA-Z<>]+\\)(.*)"

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
let is_both_framework_code (vertex1 : G.V.t) (vertex2 : G.V.t) : bool =
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
  List.mem ~equal:String.equal
    (not_this_project_methods_and_java_methods >>| Utils.extract_method_name)
    (Utils.extract_method_name methname)


(* methname is the result of Procname.to_string *)
let get_class (methname : string) : string =
  let regex = Str.regexp ".+ \\(.+\\)\\..+(.*)" in
  assert (Str.string_match regex methname 0) ;
  Str.matched_group 1 methname
