open MakeGraph

(* main *)
let main () =
  Deserializer.deserialize_json
    "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/relational-data-access/Chain.json"
  |> GraphMaker.init_graph
  |> GraphMaker.graph_to_dot ~filename:"initial_graph.dot"


let _ = main ()
