open Yojson.Basic

type json = Yojson.Basic.t

exception TODO

(* NOTE: this module will become useless when integrated, so keep it simple! *)

let deserialize_json (dir : string) : json =
  let in_channel = In_channel.create dir in
  Yojson.Basic.from_channel in_channel


let deserialize_method_txt : string -> string list = In_channel.read_lines

let deserialize_skip_func : string -> string list = In_channel.read_lines
