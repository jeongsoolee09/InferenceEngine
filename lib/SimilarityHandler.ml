(** module that extracts similar vertex pairs. *)

open ListMonad
open InfixOperators
open GraphRepr
open Chain
open NodeWiseFeatures
open ContextualFeatures

let ns_table_from_csv csv_filename =
  let acc = Hashtbl.create 777 in
  let in_chan = In_channel.create csv_filename in
  let csv_array = Csv.to_array @@ Csv.load_in in_chan in
  In_channel.close in_chan ;
  for i = 1 to Array.length csv_array - 1 do
    let method1 = csv_array.(i).(1)
    and method2 = csv_array.(i).(12)
    and ns_score = int_of_string @@ csv_array.(i).(23) in
    Hashtbl.add acc (method1, method2) ns_score
  done ;
  acc


let cs_table_from_csv csv_filename =
  let acc = Hashtbl.create 777 in
  let in_chan = In_channel.create csv_filename in
  List.iter ~f:(fun array -> raise TODO) (Csv.load_in in_chan) ;
  In_channel.close in_chan ;
  acc


let make_nodewise_sim_edge (graph : G.t) : G.t =
  Out_channel.print_string "spawning python process compute_nodewise_similarity.py..." ;
  SpawnPython.spawn_python ~pyfile:"./lib/python/compute_nodewise_similarity.py" ~args:[] ;
  Out_channel.print_endline "done" ;
  raise TODO


let make_contextual_sim_edge (graph : G.t) : G.t =
  Out_channel.print_string "spawning python process compute_contextual_similarity.py..." ;
  SpawnPython.spawn_python ~pyfile:"./lib/python/compute_contextual_similarity.py" ~args:[] ;
  Out_channel.print_endline "done" ;
  raise TODO
