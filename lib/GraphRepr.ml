open Yojson.Basic

type json = Yojson.Basic.t

exception TODO

let ( >>| ) = List.( >>| )

let ( >>= ) = List.( >>= )

let ( >> ) f g x = g (f x)

module Set = Caml.Set
module F = Format

module Vertex = struct
  type t = string * string [@@deriving compare, equal]

  let hash = Hashtbl.hash

  let pp ((procstring, locstring) : t) : string =
    F.asprintf "\"(\"%s\", \"%s\")\"" procstring locstring
end

module G = struct
  include Graph.Persistent.Digraph.ConcreteBidirectional (Vertex)

  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_name (meth, locset) = F.asprintf "\"(%s, %s)\"" meth locset

  let vertex_attributes (_ : Vertex.t) = [`Shape `Box]

  let get_subgraph _ = None

  let default_edge_attributes _ = []

  let edge_attributes _ = []

  let pp_vertex = vertex_name

  let pp_edge (v1, v2) = F.asprintf "\"(%s, %s)\"" (vertex_name v1) (vertex_name v2)
end

module Dot = Graph.Graphviz.Dot (G)

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

  let pp (slice : t) : string =
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


  let pp_chain (slices : t list) : string =
    let open_bracket = F.asprintf "[ "
    and contents = List.fold ~f:(fun acc slice -> acc ^ pp slice ^ ", ") slices ~init:""
    and close_bracket = F.asprintf "] " in
    open_bracket ^ contents ^ close_bracket


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
        failwith "Type Error"
end

module ChainSliceManager = struct
  let wrapped_chain_list_of_raw_json : json -> json list = Util.to_list

  let chain_slice_list_of_wrapped_chain (json : json) : ChainSlice.t list =
    match Util.member "chain" json with
    | `List json_list ->
        json_list >>| ChainSlice.chain_slice_of_json_assoc
    | _ ->
        failwith "Type Error"
end

module VertexMaker = struct
  let vertex_of_chain_slice (chain_slice : ChainSlice.t) : G.V.t =
    match chain_slice with
    | DefineSlice (current, _, loc, _) ->
        (current, loc)
    | CallSlice (current, callee, loc, _) ->
        (callee, loc)
    | VoidCallSlice (current, callee, loc, _) ->
        (callee, loc)
    | RedefineSlice (current, loc, _) ->
        (current, loc)
    | DeadSlice current ->
        (current, "")
    | DeadByCycleSlice current ->
        (current, "")
    | Temp (current, loc) ->
        (current, loc)


  module VertexSet = Set.Make (Vertex)

  let get_all_vertices (raw_json : json) : G.V.t list =
    let vertices_with_dup =
      ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
      >>= ChainSliceManager.chain_slice_list_of_wrapped_chain
      |> List.filter ~f:(fun slice ->
             (not @@ ChainSlice.is_dead slice) && (not @@ ChainSlice.is_deadbycycle slice) )
      >>| vertex_of_chain_slice
    in
    (* remove duplicates by switching to and from a set *)
    let out = vertices_with_dup |> VertexSet.of_list |> VertexSet.elements in
    out


  let get_all_methods () =
    Deserializer.deserialize_method_txt ()
    |> List.filter ~f:(fun method_str ->
           (not @@ String.is_substring method_str ~substring:"lambda")
           && (not @@ String.is_substring method_str ~substring:"Lambda")
           && (not @@ String.is_substring method_str ~substring:"<init>")
           && (not @@ String.is_substring method_str ~substring:"<clinit>") )
end

module ChainRefiners = struct
  let delete_inner_deads (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    (* print_endline "==================================================" ; *)
    (* print_endline @@ "before chain: " ^ ChainSlice.pp_chain chain_slices ; *)
    let all_but_last = List.drop_last_exn chain_slices in
    let dead_filtered =
      List.filter
        ~f:(fun chain_slice ->
          (* if ChainSlice.is_dead chain_slice || ChainSlice.is_deadbycycle chain_slice then *)
          (*   print_endline @@ ChainSlice.pp chain_slice ; *)
          (not @@ ChainSlice.is_dead chain_slice) && (not @@ ChainSlice.is_deadbycycle chain_slice)
          )
        all_but_last
    in
    (* print_endline @@ "\nafter chain: " *)
    (* ^ ChainSlice.pp_chain (dead_filtered @ [List.last_exn chain_slices]) ; *)
    dead_filtered
end

module EdgeMaker = struct
  let make_bicycle_chain (list : 'a list) : ('a * 'a) list =
    let all_but_last = List.drop_last_exn list and all_but_first = List.tl_exn list in
    List.zip_exn all_but_last all_but_first


  let parse_skip_func (raw_signature : string) : string =
    let pattern = Str.regexp ".*\\.\\(.+\\)(.*)" in
    assert (Str.string_match pattern raw_signature 0) ;
    Str.matched_group 1 raw_signature


  let skip_func_method_names =
    Deserializer.deserialize_skip_func ()
    |> List.filter ~f:(fun str -> not @@ String.is_prefix ~prefix:"__" str)
    >>| parse_skip_func


  let process_head_define (chain_slices : ChainSlice.t list) : ChainSlice.t list =
    let head_define = List.hd_exn chain_slices in
    let define_current_method_field, location_field, define_using_field =
      match head_define with
      | DefineSlice (current_method, _, location, using_method) ->
          (current_method, location, using_method)
      | _ ->
          failwith "ahahahahah"
    in
    if
      (not @@ String.equal define_current_method_field define_using_field)
      && List.mem ~equal:String.equal skip_func_method_names (parse_skip_func define_using_field)
    then Temp (define_using_field, location_field) :: chain_slices
    else chain_slices


  let process_chainslices chainslices = chainslices |> process_head_define

  let edge_list_of_chain_slice_list (chain_slices : ChainSlice.t list) : G.E.t list =
    let processed = process_chainslices chain_slices in
    let vertices = processed >>| VertexMaker.vertex_of_chain_slice in
    make_bicycle_chain vertices


  let get_all_edges (raw_json : json) : G.E.t list =
    ChainSliceManager.wrapped_chain_list_of_raw_json raw_json
    >>| ChainSliceManager.chain_slice_list_of_wrapped_chain >>| ChainRefiners.delete_inner_deads
    >>= edge_list_of_chain_slice_list
end
