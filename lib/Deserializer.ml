open Yojson.Basic
open ListMonad
open InfixOperators

type json = Yojson.Basic.t

module Json = Yojson.Basic

exception TODO
(* NOTE: this module will become useless when integrated, so keep it simple! *)

let deserialize_config () =
  let in_channel = In_channel.create "config.json" in
  let json = Json.from_channel in_channel in
  let out = Util.member "project_root" json |> Util.to_string in
  In_channel.close in_channel ;
  out


let project_root = deserialize_config ()

let deserialize_json () : json =
  let in_channel = In_channel.create (project_root ^ "Chain.json") in
  let out = Json.from_channel in_channel in
  In_channel.close in_channel ;
  out


let deserialize_method_txt () : string list =
  In_channel.read_lines (project_root ^ "Methods.txt")
  |> List.filter ~f:(not << String.is_empty)
  |> List.filter ~f:(not << String.is_substring "$Lambda$")
  |> List.filter ~f:(not << String.is_substring "$lambda$")


let deserialize_skip_func () : string list =
  In_channel.read_lines (project_root ^ "skip_func.txt")
  |> List.filter ~f:(not << String.is_prefix ~prefix:"__")
  |> List.filter ~f:(not << String.is_empty)


let deserialize_graph (filename : string) =
  let in_chan = In_channel.create filename in
  In_channel.set_binary_mode in_chan true ;
  Marshal.from_channel in_chan
