(** module that extracts similar vertex pairs. *)

open ListMonad
open GraphRepr
open NodeWiseFeatures
open ContextualFeatures

exception TODO

module StringPair = struct
  type t = string * string [@@deriving compare]
end

module NodeWiseSimilarityScoreMap = struct
  module WithMethodPairDomain = Caml.Map.Make (StringPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t
end

module ContextualSimilarityScoreMap = struct
  module WithMethodPairDomain = Caml.Map.Make (StringPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t
end

module SimilarVertexPairExtractor = struct
  module NodewisePairExtractor = struct
    let threshold = 1 (* TEMP *)

    let get_nodewise_similarity (method_pair : string * string) : int =
      (* execute all extractors. *)
      let extractors_list = [is_both_framework_code; belong_to_same_class] in
      List.fold
        ~f:(fun acc extractor -> if extractor method_pair then acc + 1 else acc)
        ~init:0 extractors_list


    let init_similarity_score_map (all_methods : string list) : NodeWiseSimilarityScoreMap.t =
      let carpro =
        let* method1 = all_methods in
        let* method2 = all_methods in
        return (method1, method2)
      in
      List.fold
        ~f:(fun acc ((m1, m2) as pair) ->
          let nodewise_similarity = get_nodewise_similarity pair in
          if Int.( >= ) nodewise_similarity threshold then
            NodeWiseSimilarityScoreMap.add pair nodewise_similarity acc
          else acc )
        ~init:NodeWiseSimilarityScoreMap.empty carpro


    let get_similar_methods (graph : G.t) : (string * string) list =
      let all_methods =
        let all_vertices = G.fold_vertex List.cons graph [] in
        let module StringSet = Caml.Set.Make (String) in
        all_vertices >>| fst |> StringSet.of_list |> StringSet.elements
      in
      let similarity_score_map = init_similarity_score_map all_methods in
      NodeWiseSimilarityScoreMap.fold
        (fun pair similarity acc -> if Int.( >= ) similarity threshold then pair :: acc else acc)
        similarity_score_map []
  end

  module ContextualPairExtractor = struct
    (* We first work on trunks rather than methods themselves. *)
    let threshold = 1 (* TEMP *)

    let get_trunk_similarity (trunk_pair : trunk * trunk) : int =
      (* execute all extractors. *)
      let extractors_list = [same_callee_in_trunk_count; trunks_share_same_suffixes_length] in
      List.fold ~f:(fun acc extractor -> extractor trunk_pair) ~init:0 extractors_list


    (** find vertices dangling from the trunk with bidirectional edges, e.g. if a -> b -> c <-> d,
        find d *)
    let find_bidirectionals_in_trunk (trunk : trunk) (graph : G.t) : vertex list =
      let with_bidirectional_edges =
        G.fold_vertex
          (fun vertex acc ->
            if
              List.exists
                ~f:(fun succ -> List.mem ~equal:Vertex.equal (G.succ graph succ) vertex)
                (G.succ graph vertex)
            then vertex :: acc
            else acc )
          graph []
      in
      (* now, find the nodes "dangling" from the trunk *)
      let is_dangling node =
        Int.( = ) (List.length (G.succ graph node)) 1
        &&
        let in_edges = GraphUtils.find_in_edges node graph
        and succ = List.hd_exn (G.succ graph node) in
        let edge_shooter, _, _ = List.hd_exn in_edges in
        Int.( = ) (List.length in_edges) 1 && G.V.equal edge_shooter succ
      in
      List.filter ~f:is_dangling with_bidirectional_edges


    let identify_similar_method_from_similar_trunk ((trunk1, trunk2) : trunk * trunk) (graph : G.t)
        : (vertex * vertex) list =
      (* 1. trunk's roots are similar *)
      let trunk1_root = List.hd_exn trunk1 and trunk2_root = List.hd_exn trunk2 in
      (* 2. trunk's leaves are similar *)
      let trunk1_leaf = List.last_exn trunk1 and trunk2_leaf = List.last_exn trunk2 in
      (* 3. trunks's redefines are similar *)
      let all_redefine_slices =
        RedefineHandler.collect_redefines @@ Deserializer.deserialize_json ()
      in
      let trunk1_redefines =
        List.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk1
      and trunk2_redefines =
        List.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk2
      in
      (* what if redefine is missing on one side? *)
      (* 4. trunks's method with bidirectional edges are similar *)
      let trunk1_bidirectional = find_bidirectionals_in_trunk trunk1
      and trunk2_bidirectional = find_bidirectionals_in_trunk trunk2 in
      (* TODO: making a carpro is naive. The logic should be refined *)
      let bidirectional_carpro =
        let* bidirectional1 = trunk1_bidirectional graph in
        let* bidirectional2 = trunk2_bidirectional graph in
        return (bidirectional1, bidirectional2)
      in
      match (trunk1_redefines, trunk2_redefines) with
      | [], [] | [], _ | _, [] ->
          (* do nothing regarding redefines *)
          [(trunk1_root, trunk2_root); (trunk1_leaf, trunk2_leaf)] @ bidirectional_carpro
      | _, _ ->
          (* make a carpro of redefines *)
          (* TODO: making a carpro is naive. The logic should be refined *)
          let redefines_carpro =
            let* redefine1 = trunk1_redefines in
            let* redefine2 = trunk2_redefines in
            return (redefine1, redefine2)
          in
          [(trunk1_root, trunk2_root); (trunk1_leaf, trunk2_leaf)]
          @ bidirectional_carpro @ redefines_carpro


    let init_contextual_score_map (all_trunks : trunk list) : NodeWiseSimilarityScoreMap.t =
      let carpro =
        let* trunk1 = all_trunks in
        let* trunk2 = all_trunks in
        return (trunk1, trunk2)
      in
      (* now, we need to translate the trunk similarity into node similarity. *)
      raise TODO
  end
end

module EstablishSimEdges = struct
  (* since SimlarVertexPairExtractor extracts only pairs of similar methods, we need to find vertices containing those similar methods. *)

  let with_list_index (lst : 'a list) : ('a * int) list =
    List.rev @@ List.foldi ~f:(fun index acc elem -> (index, elem) :: acc) ~init:[] lst


  (** pairup similar vertices, also putting list indices into consideration. e.g. if a and b are
      similar, and if list_a = [a; c; d; a; a; a;] and list_b = [f; g; h; b; b; i; j], then pair up
      second and third occurrences of a's with first and second occurrences of b's. *)
  let smart_pairup_vertices (trunk_a : trunk) (trunk_b : trunk)
      (similar_methods : (string * string) list) =
    let trunk_a_processed = trunk_a |> List.stable_dedup >>| with_list_index
    and trunk_b_processed = trunk_b |> List.stable_dedup >>| with_list_index in
    if Int.( >= ) (List.length trunk_a) (List.length trunk_b) then (* loop on trunk_b *)
      List.fold ~f:(fun acc b_elem -> ) ~init:[] trunk_b
    else (* loop on trunk_a *)
      List.fold ~f:(fun acc a_elem -> ) ~init:[] trunk_a


  let make_nodewise_sim_edge (graph : G.t) : G.t =
    let open SimilarVertexPairExtractor in
    let similar_edges_nodewise = NodewisePairExtractor.get_similar_methods graph in
    raise TODO


  let make_contextual_sim_edge (graph : G.t) : G.t = raise TODO
end
