open MakeGraph

exception TODO

let main () =
  let graph = Deserializer.deserialize_json () |> GraphMaker.init_graph in
  let initial_distmap = Probability.make_map_for_graph graph
  and nodewise_featuremap = NodeWiseFeatures.init_feature_map graph in
  let interaction_completed = Loop.loop initial_distmap [] graph nodewise_featuremap in
  raise TODO


let _ = main ()
