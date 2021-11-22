open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures

module TestGraph = struct
  let sample_graph =
    G.empty
    |> (fun graph -> G.add_edge graph ("nextLine", "26") ("create", "26"))
    |> (fun graph -> G.add_edge graph ("create", "26") ("create", "31"))
    |> (fun graph -> G.add_edge graph ("create", "31") ("run", "31"))
    |> (fun graph -> G.add_edge graph ("run", "31") ("append", "32"))
    |> (fun graph -> G.add_edge graph ("append", "32") ("run", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("append", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("toString", "32"))
    |> (fun graph -> G.add_edge graph ("toString", "32") ("run", "32"))
    |> (fun graph -> G.add_edge graph ("run", "32") ("batchUpdate", "34"))
    |> fun graph -> G.add_edge graph ("run", "33") ("batchUpdate", "34")

  (** How will identify_trunk work on sample_graph? *)

  let _ =
    let trunks = identify_trunks sample_graph in
    List.iter ~f:(fun trunk -> Trunk.pp trunk) trunks
end
