open Yojson.Basic
open ListMonad
open InfixOperators

type json = Yojson.Basic.t

module Json = Yojson.Basic
module F = Format

exception TODO
(* NOTE: this module will become useless when integrated, so keep it simple! *)

let make_now_string (gmt_diff : int) : string =
  let open CalendarLib in
  let now_raw = Calendar.now () in
  let year = Calendar.year now_raw in
  let month = Date.int_of_month @@ Calendar.month now_raw in
  let day = Calendar.day_of_month now_raw in
  let hour = Calendar.hour now_raw + gmt_diff in
  let minute = Calendar.minute now_raw in
  let second = Calendar.second now_raw in
  F.asprintf "%d-%d-%d_%d:%d:%d" year month day hour minute second


let deserialize_config () =
  let in_channel = In_channel.create "config.json" in
  let json = Json.from_channel in_channel in
  let out = Util.member "project_root" json |> Util.to_string in
  In_channel.close in_channel ;
  out


let project_root = deserialize_config ()

let yojson_already_serialized () =
  Array.exists ~f:(fun dir -> String.is_substring ~substring:"yojson" dir) (Sys.readdir ".")


let find_serialized_yojson () =
  Array.find_exn ~f:(fun dir -> String.is_substring ~substring:"yojson" dir) (Sys.readdir ".")


let deserialize_json =
  let cache : json ref = ref (`Assoc []) in
  fun () : json ->
    if Yojson.Basic.equal !cache (`Assoc []) then (
      if not @@ yojson_already_serialized () then (
        let in_channel = In_channel.create (project_root ^ "Chain.json") in
        let out = Json.from_channel in_channel in
        In_channel.close in_channel ;
        (* update the cache *)
        cache := out ;
        (* make a fresh marshal *)
        let marshal_out_chan =
          Out_channel.create (F.asprintf "%s_yojson.bin" (make_now_string 9))
        in
        Marshal.to_channel marshal_out_chan out [] ;
        Out_channel.close marshal_out_chan ;
        out )
      else
        let marshal_in_chan = In_channel.create (find_serialized_yojson ()) in
        let from_marshal = Marshal.from_channel marshal_in_chan in
        In_channel.close marshal_in_chan ;
        cache := from_marshal ;
        from_marshal )
    else !cache


let deserialize_method_txt =
  let cache : string list ref = ref [] in
  fun () : string list ->
    if List.is_empty !cache then (
      let out =
        In_channel.read_lines (project_root ^ "Methods.txt")
        |> List.filter ~f:(fun method_str ->
               (not @@ String.is_substring method_str ~substring:"lambda")
               && (not @@ String.is_substring method_str ~substring:"Lambda")
               && (not @@ String.is_substring method_str ~substring:"<clinit>")
               && (not @@ String.is_empty method_str) )
      in
      cache := out ;
      out )
    else !cache


let deserialize_skip_func =
  let cache : string list ref = ref [] in
  fun () : string list ->
    if List.is_empty !cache then (
      let out =
        In_channel.read_lines (project_root ^ "skip_func.txt")
        |> List.filter ~f:(not << String.is_prefix ~prefix:"__")
        |> List.filter ~f:(not << String.is_empty)
      in
      cache := out ;
      out )
    else !cache


let deserialize_graph (filename : string) =
  let in_chan = In_channel.create filename in
  In_channel.set_binary_mode in_chan true ;
  Marshal.from_channel in_chan


let deserialize_callgraph =
  let cache = ref [] in
  fun () : (string * string) list ->
    if List.is_empty !cache then (
      let out =
        let regexp = Str.regexp "\\(.*\\) -> \\(.*\\)" in
        In_channel.read_lines (project_root ^ "Callgraph.txt")
        >>| fun string ->
        assert (Str.string_match regexp string 0) ;
        (Str.matched_group 1 string, Str.matched_group 2 string)
      in
      cache := out ;
      out )
    else !cache


let deserialize_annots =
  let cache = ref [] in
  fun () : (string * string) list ->
    if List.is_empty !cache then (
      let out =
        let fd = In_channel.create "Annotations.json" in
        Json.from_channel fd |> Util.to_assoc >>| fun (string, json) -> (string, Util.to_string json)
      in
      cache := out ;
      out )
    else !cache
