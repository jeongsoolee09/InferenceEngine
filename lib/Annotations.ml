open ListMonad
open InfixOperators
module F = Format

exception TODO

module Hashtbl = Caml.Hashtbl

type single_annot = {name: string; params: (string * string) list} [@@deriving equal]

type t = single_annot list [@@deriving equal]

let get_name (single_annot : single_annot) : string = single_annot.name

let get_param (single_annot : single_annot) = single_annot.params

let capture_nonempty_angled_brackets (string : string) =
  let regex = Re2.create_exn "([^()<>\s]+(\([^()<>]*\))?)" in
  Re2.find_all_exn regex string


let split_single_annot_string (string : string) : string list =
  let regex = Re2.create_exn "([^()<>\s]+(\([^()<>]*\))?)" in
  Re2.find_all_exn regex string


let split_up_input_sig (input_sig : string) : (string * string) list =
  let regex = Re2.create_exn "([a-z]+=\\\".+\\\")" in
  let split_on_comma = Re2.find_all_exn regex input_sig in
  split_on_comma
  >>| fun str ->
  (List.nth_exn (String.split ~on:'=' str) 0, List.nth_exn (String.split ~on:'=' str) 1)


let single_annot_of_splitted_string (string_list : string list) : single_annot =
  match string_list with
  | [] ->
      failwith "Invalid input string list (is empty)"
  | [name] ->
      {name; params= []}
  | [name; input_sig] ->
      {name; params= split_up_input_sig input_sig}
  | _ ->
      failwith "Invalid input string list (is too long)"


let single_annot_of_string : string -> single_annot =
  split_single_annot_string >> single_annot_of_splitted_string


let string_of_single_annot (single_annot : single_annot) : string =
  let acc =
    List.fold
      ~f:(fun acc (param, paramval) -> F.asprintf "%s=%s," param paramval)
      ~init:(single_annot.name ^ "(") single_annot.params
  in
  acc ^ ")"


let to_string (annot_list : t) : string =
  let acc =
    List.fold
      ~f:(fun acc annot -> acc ^ F.asprintf "%s," (string_of_single_annot annot))
      ~init:"" annot_list
  in
  "[" ^ acc ^ "]"


let of_string (string : string) : t =
  string |> capture_nonempty_angled_brackets >>| single_annot_of_string


(* REMEMBER: WE DON'T NEED TEST CLASSES/METHODS!!! *)

let make_annot_lookup_table =
  let cache = ref (Hashtbl.create 777) in
  fun () ->
    match Hashtbl.length !cache with
    | 0 ->
        let out = Hashtbl.create 777 in
        List.iter
          ~f:(fun (methname, annot_str) ->
            if not @@ Method.is_testcode methname then (
              print_endline "==================================================" ;
              print_endline methname ;
              print_endline annot_str ;
              Hashtbl.add out methname (of_string annot_str) ) )
          (Deserializer.deserialize_annots ()) ;
        cache := out ;
        out
    | _ ->
        !cache


let get_annots = Hashtbl.find (make_annot_lookup_table ())

(* let has_same_annotation (method1 : Method.t) (method2 : Method.t) : bool = *)
(*   equal (get_annots method1) (get_annots method2) *)
