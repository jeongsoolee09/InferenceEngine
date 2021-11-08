open GraphRepr

module Utils = struct
  let extract_package_name (unique_identifier : string) : string =
    let regex =
      Str.regexp "\\([a-z]+\\.[a-z]+\\.[a-z]+\\)\\.[a-zA-Z]+\\.[a-zA-Z]+(.*):[a-zA-Z]+\"\\)"
    in
    assert (Str.string_match regex unique_identifier 0) ;
    Str.matched_group 1 unique_identifier
end

module NodeWiseFeatures = struct
  (* methname is the result of Procname.to_string *)
  let is_framework_code (methname : string) : bool =
    let skip_methods =
      Deserializer.deserialize_method_txt
        "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access/skip_func.txt"
    and udf_methods =
      Deserializer.deserialize_method_txt
        "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access/Methods.txt"
    in
    let this_project_package_name = Utils.extract_package_name @@ List.hd_exn udf_methods in
    let not_this_project_methods_and_java_methods =
      List.filter
        ~f:(fun str ->
          not
            ( String.is_substring ~substring:"java." str
            || String.is_substring ~substring:this_project_package_name str ) )
        skip_methods
    in
    (* TODO 밑의 expr는 정답이 아님. 더 코딩하기 싫어서 타입만 맞추고 냅둔 것. *)
    List.mem ~equal:String.equal not_this_project_methods_and_java_methods methname


  (* methname is the result of Procname.to_string *)
  let get_class (methname : string) : string =
    let regex = Str.regexp ".+ \\(.+\\)\\..+(.*)" in
    assert (Str.string_match regex methname 0) ;
    Str.matched_group 1 methname
end

module ContextualFeatures = struct
  let _ = print_endline "hihi"
end
