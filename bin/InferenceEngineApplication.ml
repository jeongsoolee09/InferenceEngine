open GraphMaker

exception TODO

let main () =
  let graph = Deserializer.deserialize_json () |> GraphMaker.init_graph in
  let nodewise_featuremap = NodeWiseFeatures.init_feature_map graph in
  let interaction_completed = Loop.loop graph [] nodewise_featuremap 1 in
  print_endline "hihi"


let _ = main ()
