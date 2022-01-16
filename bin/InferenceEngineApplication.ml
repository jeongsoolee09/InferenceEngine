open ListMonad
open InfixOperators
open GraphMaker
open SimilarityHandler

exception TODO

let main () =
  (* let graph = Deserializer.deserialize_json () |> GraphMaker.init_graph ~debug:true in *)
  let json = Deserializer.deserialize_json () in
  (* let interaction_completed = Loop.loop graph [] nodewise_featuremap 1 in *)
  print_endline "done!"


let _ = main ()
