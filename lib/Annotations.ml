open ListMonad
open InfixOperators
module F = Format

exception TODO

type single_annot = {name: string; params: (string * string) list}

type t = single_annot list

let get_name (single_annot : single_annot) : string = single_annot.name

let get_param (single_annot : single_annot) = single_annot.params

let capture_angled_brackets (string : string) =
  String.fold
    ~f:(fun (is_capturing, smol_acc, big_acc) char ->
      if is_capturing then
        if Char.equal char '>' then (false, [], (char :: smol_acc) :: big_acc)
        else (is_capturing, char :: smol_acc, big_acc)
      else if Char.equal char '<' then (true, char :: smol_acc, big_acc)
      else (is_capturing, smol_acc, big_acc) )
    ~init:(String.is_prefix ~prefix:"<" string, [], [])
    string
  |> trd3 >>| List.rev >>| String.of_char_list |> List.rev


let split_single_annot_string (string : string) : string list =
  let regexp = Str.regexp "<\\([^<^(^)^>]+\\)\\(([^(^)]+)\\)*)?>" in
  assert (Str.string_match regexp string 0) ;
  let acc = ref [] in
  List.iteri
    ~f:(fun index _ -> try acc := Str.matched_group index string :: !acc with _ -> ())
    [0; 1; 2] ;
  List.tl_exn @@ List.rev @@ !acc


let split_up_input_sig (input_sig : string) : (string * string) list =
  String.split ~on:',' input_sig
  >>| fun split ->
  let split_on_equal = String.split ~on:'=' split in
  let filter char = Char.equal '(' char || Char.equal ')' char || Char.equal ' ' char in
  ( String.strip ~drop:filter @@ List.nth_exn split_on_equal 0
  , String.strip ~drop:filter @@ List.nth_exn split_on_equal 1 )


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


let of_string (string : string) : t = string |> capture_angled_brackets >>| single_annot_of_string

(* REMEMBER: WE DON'T NEED TEST CLASSES/METHODS!!! *)
