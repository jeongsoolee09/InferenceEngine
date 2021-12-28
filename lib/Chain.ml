open Yojson.Basic
open ListMonad
open InfixOperators
module F = Format

type json = Yojson.Basic.t

module ChainSlice = struct
  type t =
    | DefineSlice of (string * string * string * string) (* current_method, access_path, location, using *)
    | CallSlice of (string * string * string * string) (* current_method, callee, location, with *)
    | VoidCallSlice of (string * string * string * string) (* current_method, callee, location, with *)
    | RedefineSlice of (string * string * string) (* current_method, location, access_path *)
    | DeadSlice of string (* current_method *)
    | DeadByCycleSlice of string (* current_method *)
    | Temp of (string * string)
  (* current_method, location *)
  [@@deriving equal, compare]

  let to_string (slice : t) : string =
    match slice with
    | DefineSlice (curr, ap, loc, using) ->
        F.asprintf "DefineSlice (%s, %s, %s, %s)" curr ap loc using
    | CallSlice (curr, callee, loc, with_) ->
        F.asprintf "CallSlice (%s, %s, %s, %s)" curr callee loc with_
    | VoidCallSlice (curr, ap, loc, using) ->
        F.asprintf "VoidCallSlice (%s, %s, %s, %s)" curr ap loc using
    | RedefineSlice (curr, loc, ap) ->
        F.asprintf "RedefineSlice (%s, %s, %s)" curr loc ap
    | DeadSlice curr ->
        F.asprintf "DeadSlice (%s)" curr
    | DeadByCycleSlice curr ->
        F.asprintf "DeadByCycleSlice (%s)" curr
    | Temp (curr, loc) ->
        F.asprintf "Temp (%s, %s)" curr loc


  let list_to_string (slices : t list) : string =
    let contents = List.fold ~f:(fun acc slice -> acc ^ to_string slice ^ ", ") slices ~init:"" in
    "[ " ^ contents ^ " ]"


  let is_define slice = match slice with DefineSlice _ -> true | _ -> false

  let is_call slice = match slice with CallSlice _ -> true | _ -> false

  let is_voidcall slice = match slice with VoidCallSlice _ -> true | _ -> false

  let is_redefine slice = match slice with RedefineSlice _ -> true | _ -> false

  let is_dead slice = match slice with DeadSlice _ -> true | _ -> false

  let is_deadbycycle slice = match slice with DeadByCycleSlice _ -> true | _ -> false

  let chain_slice_of_json_assoc (json_assoc : json) : t =
    match json_assoc with
    | `Assoc alist -> (
      match List.Assoc.find_exn alist "status" ~equal:String.equal with
      | `String "Define" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let access_path =
            Util.to_string @@ List.Assoc.find_exn alist "access_path" ~equal:String.equal
          in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let using = Util.to_string @@ List.Assoc.find_exn alist "using" ~equal:String.equal in
          DefineSlice (current_method, access_path, location, using)
      | `String "Call" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let callee = Util.to_string @@ List.Assoc.find_exn alist "callee" ~equal:String.equal in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let with_ = Util.to_string @@ List.Assoc.find_exn alist "with" ~equal:String.equal in
          CallSlice (current_method, callee, location, with_)
      | `String "VoidCall" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          let callee = Util.to_string @@ List.Assoc.find_exn alist "callee" ~equal:String.equal in
          let location =
            Util.to_string @@ List.Assoc.find_exn alist "location" ~equal:String.equal
          in
          let with_ = Util.to_string @@ List.Assoc.find_exn alist "with" ~equal:String.equal in
          VoidCallSlice (current_method, callee, location, with_)
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
          RedefineSlice (current_method, location, access_path)
      | `String "Dead" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          DeadSlice current_method
      | `String "DeadByCycle" ->
          let current_method =
            Util.to_string @@ List.Assoc.find_exn alist "current_method" ~equal:String.equal
          in
          DeadByCycleSlice current_method
      | otherwise ->
          raise @@ Invalid_argument (Yojson.Basic.to_string otherwise) )
    | _ ->
        failwith "Type Error1"
end

module ChainSliceManager = struct
  let wrapped_chain_list_of_raw_json : json -> json list = Util.to_list

  let chain_slice_list_of_wrapped_chain (json : json) : ChainSlice.t list =
    match Util.member "chain" json with
    | `List json_list ->
        json_list >>| ChainSlice.chain_slice_of_json_assoc
    | _ ->
        failwith "Type Error2"
end
