open ListMonad
open InfixOperators
open Yojson.Basic
open GraphRepr
open Chain
module Json = Yojson.Basic

type json = Json.t

module ChainSliceSet = Set.Make (ChainSlice)

let collect_redefines_for_single_chain (json_assoc : json) : ChainSlice.t list =
  let collected =
    match json_assoc with
    | `List alist ->
        List.fold
          ~f:(fun acc assoc ->
            let alist = Util.to_assoc assoc in
            match List.Assoc.find_exn alist "status" ~equal:String.equal with
            | `String "Redefine" ->
                let current_method =
                  Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
                in
                let location =
                  Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
                in
                let access_path =
                  Util.to_string @@ List.Assoc.find_exn alist "access_path" ~equal:String.equal
                in
                let redefine_slice =
                  ChainSlice.RedefineSlice (current_method, location, access_path)
                in
                redefine_slice :: acc
            | _ ->
                acc )
          ~init:[] alist
    | _ ->
        failwith "Type Error3"
  in
  (* deduping process (elem order is irrelevant) *)
  collected |> ChainSliceSet.of_list |> ChainSliceSet.elements


(** Is this vertex from a redefine slice? *)
let is_redefine_vertex (redefine_slices : ChainSlice.t list) (vertex : G.V.t) : bool =
  (* check if the method name and linum matches *)
  let method_name, linum = (Vertex.get_method vertex, Vertex.get_loc vertex) in
  List.fold
    ~f:(fun acc slice ->
      match slice with
      | ChainSlice.RedefineSlice (slice_method, slice_loc, _) ->
          let is_match =
            Method.equal method_name (Method.of_string slice_method)
            && LocationSet.equal linum (LocationSet.of_string slice_loc)
          in
          is_match || acc
      | _ ->
          acc )
    ~init:false redefine_slices


let collect_redefines (json : json) : ChainSlice.t list =
  match json with
  | `List list ->
      list
      >>| (fun json_assoc -> Util.member "chain" json_assoc)
      >>= collect_redefines_for_single_chain
  | _ ->
      failwith "Type Error4"


(** serialize the list of redefine methods into a txt file. *)
let make_and_output_redefine_dict (json : json) : unit =
  let redefine_methods = collect_redefines json >>| ChainSlice.get_current_method in
  let module MethodSet = Caml.Set.Make (Method) in
  let redefine_methods = redefine_methods |> MethodSet.of_list |> MethodSet.elements in
  let out_chan = Out_channel.create @@ F.asprintf "Redefines.txt" in
  List.iter
    ~f:(fun method_ -> Out_channel.output_string out_chan (F.asprintf "%s\n" method_))
    redefine_methods ;
  Out_channel.close out_chan
