(* behold the power of Emacs... *)

open GraphRepr
open ListMonad
open InfixOperators
open ContextualFeatures
open MakeGraph
open SimilarityHandler
open Probability

module GraphTest = struct
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

  let test () =
    let trunks = identify_trunks sample_graph in
    List.iter ~f:(fun trunk -> Trunk.pp trunk) trunks
end

module GraphMakerTest = struct
  open GraphRepr

  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let out_channel = Out_channel.create "graphmakertest.dot"

  let test () = Dot.output_graph out_channel graph

  (* works well!!! *)
end

module DistMapTest = struct
  let json = Deserializer.deserialize_json ()

  let graph = GraphMaker.init_graph json

  let distmap = make_map_for_graph graph

  let vertices = G.all_vertices_of_graph graph

  let distmap_keys = ProbMap.fold (fun k _ acc -> k :: acc) distmap []

  let test () =
    ProbMap.iter
      (fun k v ->
        print_string @@ Vertex.to_string k ;
        print_string @@ ProbQuadruple.to_string v ;
        print_newline () )
      distmap


  let test2 () =
    List.for_all ~f:(fun elem -> List.mem ~equal:Vertex.equal vertices elem) distmap_keys


  let test3 () =
    List.for_all ~f:(fun elem -> List.mem ~equal:Vertex.equal distmap_keys elem) vertices

  (* works well! *)
end
