open Yojson.Basic
open GraphRepr
module Json = Yojson.Basic

type json = Json.t

module ChainSliceSet = Set.Make (ChainSlice)

let collect_redefines (json_assoc : json) : ChainSlice.t list =
  let collected =
    match json_assoc with
    | `Assoc alist ->
        List.fold
          ~f:(fun acc chainslice ->
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
            | otherwise ->
                acc )
          ~init:[] alist
    | _ ->
        failwith "Type Error"
  in
  (* deduping process (order is irrelevant) *)
  collected |> ChainSliceSet.of_list |> ChainSliceSet.elements
