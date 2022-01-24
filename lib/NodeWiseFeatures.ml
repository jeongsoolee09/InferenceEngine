open ListMonad
open InfixOperators
open GraphRepr
open FeatureMaps
open Method

exception TODO

module F = Format
module Hashtbl = Caml.Hashtbl

type feature = Int of int | String of string | Bool of bool | Annotation of Annotations.t
[@@deriving equal]

module SingleFeature = struct
  type rule = Method.t -> feature

  type t = {label: string; rule: rule}

  let this_project_package_name : string =
    let skip_methods = Deserializer.deserialize_skip_func ()
    and udf_methods = Deserializer.deserialize_method_txt () in
    UniqueID.get_package_name @@ List.hd_exn udf_methods


  let is_this_project_class_initializer =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let methstring = Method.to_string method_ in
            let method_lines = Deserializer.deserialize_method_txt () in
            List.exists
              ~f:(fun line ->
                String.is_substring ~substring:methstring line
                && String.is_prefix ~prefix:this_project_package_name line )
              method_lines
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let is_this_project_method =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let methstring = Method.to_string method_ in
            if String.is_substring ~substring:"<init>" methstring then
              is_this_project_class_initializer method_
            else
              try
                let this_method_id = find_unique_identifier method_ in
                let methstring_package = UniqueID.get_package_name this_method_id in
                String.equal methstring_package this_project_package_name
              with _ -> (* It's very likely an interface method *)
                        true
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  (* NOTE do not use this for a pairwise feature *)
  let is_main_method =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let methstring = Method.to_string method_ in
            String.is_substring ~substring:"main(" methstring
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let is_java_builtin_class_initializer =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      let skip_func_lines = Deserializer.deserialize_skip_func () in
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            List.exists
              ~f:(fun line ->
                let classname = get_class_name method_ and methodname = get_method_name method_ in
                String.is_substring ~substring:(F.asprintf "%s.%s" classname methodname) line
                && String.is_prefix ~prefix:"java." line )
              skip_func_lines
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let is_java_builtin_method =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let skip_func_lines = Deserializer.deserialize_skip_func () in
            let methstring = Method.to_string method_ in
            if String.is_substring ~substring:"<init>" methstring then
              is_java_builtin_class_initializer method_
            else
              try
                let this_method_id = find_unique_identifier method_ in
                let methstring_package = UniqueID.get_package_name this_method_id in
                String.is_prefix ~prefix:"java." methstring_package
              with _ -> (* It's very likely an interface method *)
                        true
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let is_framework_method =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let methstring = Method.to_string method_ in
            if String.is_substring ~substring:"<init>" methstring then
              not
                ( is_this_project_class_initializer method_
                || is_java_builtin_class_initializer method_ )
            else not (is_java_builtin_method method_ || is_this_project_method method_)
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let returnval_not_used_in_caller =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
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
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let is_library_code =
    let cache = Hashtbl.create 777 in
    fun (method_ : Method.t) : bool ->
      match Hashtbl.find_opt cache method_ with
      | None ->
          let out =
            let classname = get_class_name method_ and methname = get_method_name method_ in
            let classname_methname = F.asprintf "%s.%s" classname methname in
            List.exists
              ~f:(fun line -> String.is_substring ~substring:classname_methname line)
              (Deserializer.deserialize_skip_func ())
          in
          Hashtbl.add cache method_ out ;
          out
      | Some res ->
          res


  let all_features : t array =
    [| {label= "return_type"; rule= (fun m -> String (get_return_type m))}
     ; {label= "class_name"; rule= (fun m -> String (get_class_name m))}
     ; {label= "method_name"; rule= (fun m -> String (get_method_name m))}
     ; {label= "package_name"; rule= (fun m -> String (get_package_name m))}
     ; {label= "is_framework_method"; rule= (fun m -> Bool (is_framework_method m))}
     ; {label= "is_java_builtin_method"; rule= (fun m -> Bool (is_java_builtin_method m))}
     ; {label= "is_library_code"; rule= (fun m -> Bool (is_library_code m))}
     ; { label= "returnval_not_used_in_caller"
       ; rule= (fun m -> Bool (returnval_not_used_in_caller m)) }
     ; {label= "is_initializer"; rule= (fun m -> Bool (is_initializer m))}
     ; {label= "annots"; rule= (fun m -> Annotation (Annotations.get_annots m))} |]
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
    && String.equal (Method.get_package_name method1) (Method.get_package_name method2)


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


  let has_same_annots ((method1, method2) : Method.t * Method.t) : bool =
    let method1_annots = Annotations.get_annots method1
    and method2_annots = Annotations.get_annots method2 in
    List.exists
      ~f:(fun method1_annot ->
        List.mem ~equal:Annotations.equal_single_annot method2_annots method1_annot )
      method1_annots


  let all_features =
    [| is_both_framework_code
     ; belong_to_same_class
     ; belong_to_same_package
     ; return_type_is_another's_class
     ; is_both_java_builtin
     ; is_both_initializer
     ; has_same_annots |]
end

let run_all_single_features (method_ : Method.t) : feature list =
  List.rev
  @@ Array.fold
       ~f:(fun acc feature ->
         let feature_value = feature.rule method_ in
         feature_value :: acc )
       SingleFeature.all_features ~init:[]


module NodeWiseFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (Method)
  include WithMethodDomain

  type t = feature list WithMethodDomain.t

  let string_of_feature (feature : feature) : string =
    match feature with
    | Int int ->
        Int.to_string int
    | String string ->
        string
    | Bool bool ->
        Bool.to_string bool
    | Annotation annots ->
        "["
        ^ List.fold
            ~f:(fun acc single_annot -> acc ^ F.asprintf "%s, " single_annot.name)
            annots ~init:""
        ^ "]"


  let init (methods : Method.t list) : t =
    List.fold
      ~f:(fun current_map method_ -> add method_ (run_all_single_features method_) current_map)
      ~init:empty methods


  module CSVSerializer = struct
    let headers : string list =
      "methname"
      :: ( Array.to_list
         @@ Array.map ~f:(fun single_feature -> single_feature.label) SingleFeature.all_features )


    let serialize (featuremap : t) ~(filename : string) : unit =
      let csv_repr =
        headers
        :: ( List.rev
           @@ fold
                (fun method_ features acc -> (method_ :: (features >>| string_of_feature)) :: acc)
                featuremap [] )
      in
      let out_chan = Out_channel.create filename in
      let csv_out_chan = Csv.to_channel ~quote_all:true out_chan in
      Csv.output_all csv_out_chan csv_repr ;
      Out_channel.close out_chan
  end
end
