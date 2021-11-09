open MakeGraph

(* main *)
let main () =
  Deserializer.deserialize_json () |> GraphMaker.init_graph
  |> GraphMaker.graph_to_dot ~filename:"initial_graph.dot"


let _ = main ()
