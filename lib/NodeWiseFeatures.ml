open ListMonad
open InfixOperators
open GraphRepr

(* open FeatureMaps *)
open Method
module F = Format
module Hashtbl = Caml.Hashtbl

type feature_value = Int of int | String of string | Bool of bool | Annotation of Annotations.t
[@@deriving equal]

module SingleFeature = struct
  type feature = Method.t -> feature_value

  type t = {label: string; feature: feature}

  let this_project_package_name : string =
    let udf_methods = Deserializer.deserialize_method_txt () in
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
          let out = List.mem ~equal:Method.equal (Deserializer.deserialize_void_call ()) method_ in
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
    [| {label= "return_type"; feature= (fun m -> String (get_return_type m))}
     ; {label= "class_name"; feature= (fun m -> String (get_class_name m))}
     ; {label= "method_name"; feature= (fun m -> String (get_method_name m))}
     ; {label= "package_name"; feature= (fun m -> String (get_package_name m))}
     ; {label= "is_framework_method"; feature= (fun m -> Bool (is_framework_method m))}
     ; {label= "is_java_builtin_method"; feature= (fun m -> Bool (is_java_builtin_method m))}
     ; {label= "is_library_code"; feature= (fun m -> Bool (is_library_code m))}
     ; { label= "returnval_not_used_in_caller"
       ; feature= (fun m -> Bool (returnval_not_used_in_caller m)) }
     ; {label= "is_initializer"; feature= (fun m -> Bool (is_initializer m))}
     ; {label= "annots"; feature= (fun m -> Annotation (Annotations.get_annots m))} |]
end

let run_all_single_features (method_ : Method.t) : feature_value list =
  List.rev
  @@ Array.fold
       ~f:(fun acc feature ->
         print_endline feature.label ;
         let feature_value = feature.feature method_ in
         feature_value :: acc )
       SingleFeature.all_features ~init:[]


module NodeWiseFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (Method)
  include WithMethodDomain

  type t = feature_value list WithMethodDomain.t

  let string_of_feature (feature : feature_value) : string =
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
      ~f:(fun current_map method_ ->
        print_endline method_ ;
        add method_ (run_all_single_features method_) current_map )
      ~init:empty methods


  module CSVSerializer = struct
    let headers : string list =
      "methname"
      :: ( Array.to_list
         @@ Array.map ~f:(fun single_feature -> single_feature.label) SingleFeature.all_features )


    let serialize (featuremap : t) ~(filename : string) : unit =
      if Sys.is_file_exn filename then
        print_endline @@ F.asprintf "File %s already exists. Skipping." filename
      else
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

  let is_already_serialized filename =
    Array.exists ~f:(fun dir -> String.equal dir filename) @@ Sys.readdir "."


  let serialize_to_bin (featuremap : t) ~(filename : string) : unit =
    let out_chan = Out_channel.create filename in
    Marshal.to_channel out_chan featuremap [] ;
    Out_channel.close out_chan


  let deserialize_bin (filename : string) : t =
    let in_chan = In_channel.create filename in
    let out = Marshal.from_channel in_chan in
    In_channel.close in_chan ;
    out


  let init_for_graph_udfs (graph : G.t) =
    let filename = F.asprintf "NodeWiseFeatures_%s_udfs.bin" graph.comp_unit in
    match is_already_serialized filename with
    | true ->
        deserialize_bin filename
    | false ->
        let unmarked_udfs_of_graph = G.get_unmarked_udfs graph in
        let out = init unmarked_udfs_of_graph in
        serialize_to_bin out ~filename ;
        out


  let init_for_graph_apis (graph : G.t) =
    let filename = F.asprintf "NodeWiseFeatures_%s_apis.bin" graph.comp_unit in
    match is_already_serialized filename with
    | true ->
        deserialize_bin filename
    | false ->
        let unmarked_apis_of_graph = G.get_unmarked_apis graph in
        let out = init unmarked_apis_of_graph in
        serialize_to_bin out ~filename ;
        out


  let init_for_graph (graph : G.t) : t * t = (init_for_graph_apis graph, init_for_graph_udfs graph)

  let merge_two_maps (map1 : t) (map2 : t) : t =
    fold (fun k v current_map -> add k v current_map) map2 map1
end
