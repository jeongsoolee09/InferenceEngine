(** module that extracts similar vertex pairs. *)

open ListMonad
open GraphRepr
open NodeWiseFeatures
open ContextualFeatures

module StringPair = struct
  type t = string * string [@@deriving compare]

  let to_string (s1, s2) = F.asprintf "(%s, %s)" s1 s2
end

module NodeWiseSimilarityMap = struct
  module WithMethodPairDomain = Caml.Map.Make (StringPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t

  let threshold = 4 (* TEMP *)

  let init (all_methods : string list) : t =
    let method_pairs =
      let* method1 = all_methods in
      let* method2 = all_methods in
      if not @@ String.equal method1 method2 then return (method1, method2) else []
    in
    List.fold
      ~f:(fun acc pair -> WithMethodPairDomain.add pair 0 acc)
      method_pairs ~init:WithMethodPairDomain.empty


  let is_similar_nodewise (meth1 : string) (meth2 : string) (map : t) : bool =
    Int.( >= ) (find (meth1, meth2) map) threshold
end

module TrunkSimilarityMap = struct
  module TrunkPair = struct
    type t = G.Trunk.t * G.Trunk.t [@@deriving compare, equal]
  end

  module WithTrunkPairDomain = Caml.Map.Make (TrunkPair)
  include WithTrunkPairDomain

  type t = Int.t WithTrunkPairDomain.t

  let threshold = 1 (* TEMP *)

  let init (all_trunks : trunk list) : t =
    let trunk_pairs =
      let* trunk1 = all_trunks in
      let* trunk2 = all_trunks in
      if not @@ G.Trunk.equal trunk1 trunk2 then return (trunk1, trunk2) else []
    in
    List.fold
      ~f:(fun acc pair -> WithTrunkPairDomain.add pair 0 acc)
      trunk_pairs ~init:WithTrunkPairDomain.empty


  let pair_with_max_value (map : t) : TrunkPair.t =
    (* for debugging purposes *)
    fst
    @@ fold
         (fun key value (current_max_key, current_max_value) ->
           if value > current_max_value then (key, value) else (current_max_key, current_max_value)
           )
         map
         (([], []), Int.min_value)
end

module ContextualSimilarityMap = struct
  module WithMethodPairDomain = Caml.Map.Make (StringPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t

  let init (all_methods : string list) : t =
    let method_pairs =
      let* method1 = all_methods in
      let* method2 = all_methods in
      if not @@ String.equal method1 method2 then return (method1, method2) else []
    in
    List.fold
      ~f:(fun acc pair -> WithMethodPairDomain.add pair 0 acc)
      method_pairs ~init:WithMethodPairDomain.empty
end

module SimilarVertexPairExtractor = struct
  (** module that ultimately calculates the nodewise similarity of each method. *)
  module NodewisePairExtractor = struct
    let pairwise_features_and_their_scores =
      let open NodeWiseFeatures.PairwiseFeature in
      [ (is_both_framework_code, 4)
      ; (belong_to_same_class, 2)
      ; (belong_to_same_package, 2)
      ; (return_type_is_another's_class, 3)
      ; (is_both_java_builtin, 3)
      ; (is_both_initializer, 4) ]


    (** Run all extractors for every method pair. *)
    let get_nodewise_similarity (method_pair : string * string) : int =
      (* execute all extractors. *)
      List.fold
        ~f:(fun current_score (feature, score) ->
          if feature method_pair then current_score + score else current_score )
        ~init:0 pairwise_features_and_their_scores


    (** main functionality: calculate the nodewise simliarity of each method and organize those in a
        table. *)
    let update_nodewise_similarity_map (all_methods : string list) : NodeWiseSimilarityMap.t =
      let initial_map = NodeWiseSimilarityMap.init all_methods in
      NodeWiseSimilarityMap.fold
        (fun ((m1, m2) as pair) _ acc ->
          let nodewise_similarity = get_nodewise_similarity pair in
          NodeWiseSimilarityMap.remove pair acc
          |> NodeWiseSimilarityMap.add pair nodewise_similarity )
        initial_map initial_map
  end

  module TrunkPairExtractor = struct
    (** Run all extractors for every trunk pair. *)
    let get_trunk_similarity (trunk_pair : trunk * trunk) : int =
      (* execute all extractors. *)
      let extractors_list =
        [ ContextualFeatures.TrunkFeatures.same_callee_in_trunk_count
        ; ContextualFeatures.TrunkFeatures.trunks_share_same_suffixes_length
        ; ContextualFeatures.TrunkFeatures.trunks_share_same_prefixes_length ]
      in
      List.fold ~f:(fun acc extractor -> acc + extractor trunk_pair) ~init:0 extractors_list


    (** main functionality: calculate the simliarity of each trunk pairs and organize those in a
        table. *)
    let update_trunk_similarity_map (all_trunks : trunk list) : TrunkSimilarityMap.t =
      let initial_map = TrunkSimilarityMap.init all_trunks in
      TrunkSimilarityMap.fold
        (fun ((t1, t2) as pair) _ acc ->
          let trunk_similarity = get_trunk_similarity pair in
          TrunkSimilarityMap.remove pair acc |> TrunkSimilarityMap.add pair trunk_similarity )
        initial_map initial_map
  end

  (** module that ultimately calculates the contextual similarity of each method. *)
  module ContextualPairExtractor = struct
    (* We first work on trunks rather than methods themselves. *)
    let threshold = TrunkSimilarityMap.threshold

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
        let in_edges = G.find_in_edges (G.LiteralVertex.of_vertex node) graph
        and succ = List.hd_exn (G.succ graph node) in
        let edge_shooter, _, _ = List.hd_exn in_edges in
        Int.( = ) (List.length in_edges) 1 && G.V.equal edge_shooter succ
      in
      List.filter ~f:is_dangling with_bidirectional_edges


    (** given a trunk pair, get the pairs of methods that are contextually similar to each other. *)
    let identify_similar_method_from_similar_trunk ((trunk1, trunk2) : trunk * trunk) (graph : G.t)
        : (string * string) list =
      (* 1. trunk's roots are similar *)
      let trunk1_root = List.hd_exn trunk1 and trunk2_root = List.hd_exn trunk2 in
      (* 2. trunk's leaves are similar *)
      let trunk1_leaf = List.last_exn trunk1 and trunk2_leaf = List.last_exn trunk2 in
      let root_pair_list =
        if
          (not @@ Vertex.equal trunk1_root trunk2_root)
          && not
               ( String.is_substring (fst3 trunk1_root) ~substring:"<init>"
               || String.is_substring (fst3 trunk2_root) ~substring:"<init>" )
        then [(fst3 trunk1_root, fst3 trunk2_root); (fst3 trunk2_root, fst3 trunk1_root)]
        else []
      and leaf_pair_list =
        if
          (not @@ Vertex.equal trunk1_leaf trunk2_leaf)
          && not
               ( String.is_substring (fst3 trunk1_leaf) ~substring:"<init>"
               || String.is_substring (fst3 trunk2_leaf) ~substring:"<init>" )
        then [(fst3 trunk1_leaf, fst3 trunk2_leaf); (fst3 trunk2_leaf, fst3 trunk1_leaf)]
        else []
      in
      (* 3. trunks's redefines are similar *)
      let all_redefine_slices =
        RedefineHandler.collect_redefines @@ Deserializer.deserialize_json ()
      in
      let trunk1_redefines =
        List.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk1
      and trunk2_redefines =
        List.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk2
      in
      (* we can make redefine pairs: make a carpro of redefines *)
      (* TODO: making a carpro is naive. The logic should be refined *)
      let redefines_carpro =
        let* redefine1 = trunk1_redefines in
        let* redefine2 = trunk2_redefines in
        if not @@ Vertex.equal redefine1 redefine2 then return (fst3 redefine1, fst3 redefine2)
        else []
      in
      (* what if redefine is missing on one side? *)
      (* 4. trunks's method with bidirectional edges are similar *)
      let trunk1_bidirectional = find_bidirectionals_in_trunk trunk1
      and trunk2_bidirectional = find_bidirectionals_in_trunk trunk2 in
      (* TODO: making a carpro is naive. The logic should be refined *)
      (* let bidirectional_carpro = *)
      (*   let* bidirectional1 = trunk1_bidirectional graph in *)
      (*   let* bidirectional2 = trunk2_bidirectional graph in *)
      (*   if *)
      (*     Vertex.equal bidirectional1 bidirectional2 *)
      (*     && G.is_pointing_to_each_other *)
      (*          (G.LiteralVertex.of_vertex trunk1_root) *)
      (*          (G.LiteralVertex.of_vertex trunk1_leaf) *)
      (*          graph ~label:EdgeLabel.NodeWiseSimilarity *)
      (*   then return (fst3 bidirectional1, fst3 bidirectional2) *)
      (*   else [] *)
      (* in *)
      root_pair_list @ leaf_pair_list (* @ bidirectional_carpro *) @ redefines_carpro


    (** pairup similar vertices, also putting list indices into consideration. e.g. if a and b are
        similar, and if list_a = [a; c; d; a; a; a;] and list_b = [f; g; h; b; b; i; j], then pair
        up second and third occurrences of a's with first and second occurrences of b's. *)
    let smart_pairup_vertices (trunk_a : trunk) (trunk_b : trunk)
        ((trunk_a_similar, trunk_b_similar) : string * string) : (vertex * vertex) list =
      let with_list_index (lst : 'a list) : (int * 'a) list =
        List.rev @@ List.foldi ~f:(fun index acc elem -> (index, elem) :: acc) ~init:[] lst
      in
      let trunk_a_processed =
        trunk_a
        |> List.filter ~f:(fun vertex -> String.equal trunk_a_similar (fst3 vertex))
        |> List.stable_dedup |> with_list_index
      and trunk_b_processed =
        trunk_b
        |> List.filter ~f:(fun vertex -> String.equal trunk_b_similar (fst3 vertex))
        |> List.stable_dedup |> with_list_index
      in
      if Int.( >= ) (List.length trunk_a) (List.length trunk_b) then
        (* loop on trunk_b *)
        List.fold
          ~f:(fun acc ((b_index, b_elem) as b_tuple) ->
            (* find the a_tuple in trunk_a with the minimum difference in terms of index *)
            let a_elem_with_smallest_diff =
              List.fold
                ~f:(fun ((current_min, _) as current_min_tuple) ((a_index, a_elem) as a_tuple) ->
                  let diff = Int.abs (Int.( - ) a_index b_index) in
                  if diff <= current_min then a_tuple else current_min_tuple )
                ~init:(Int.max_value, ("", "", ProbQuadruple.initial))
                trunk_a_processed
            in
            (b_elem, snd a_elem_with_smallest_diff) :: acc )
          ~init:[] trunk_b_processed
      else
        (* loop on trunk_a *)
        List.fold
          ~f:(fun acc ((a_index, a_elem) as a_tuple) ->
            (* find the b_tuple in trunk_b with the minimum difference in terms of index *)
            let b_elem_with_smallest_diff =
              List.fold
                ~f:(fun ((current_min, _) as current_min_tuple) ((b_index, b_elem) as b_tuple) ->
                  let diff = Int.abs (Int.( - ) b_index a_index) in
                  if diff <= current_min then b_tuple else current_min_tuple )
                ~init:(Int.max_value, ("", "", ProbQuadruple.initial))
                trunk_b_processed
            in
            (a_elem, snd b_elem_with_smallest_diff) :: acc )
          ~init:[] trunk_a_processed
  end
end

module EstablishSimEdges = struct
  let make_nodewise_sim_edge (graph : G.t) : G.t =
    let open SimilarVertexPairExtractor in
    let all_vertices = G.all_vertices_of_graph graph in
    let contextual_similarity_map =
      NodewisePairExtractor.update_nodewise_similarity_map (G.all_methods_of_graph graph)
    in
    let above_threshold_entries =
      NodeWiseSimilarityMap.filter
        (fun _ similarity -> similarity >= NodeWiseSimilarityMap.threshold)
        contextual_similarity_map
    in
    NodeWiseSimilarityMap.fold
      (fun (method1, method2) _ acc ->
        let method1_vertices =
          List.filter ~f:(fun (meth, _, _) -> String.equal meth method1) all_vertices
        and method2_vertices =
          List.filter ~f:(fun (meth, _, _) -> String.equal meth method2) all_vertices
        in
        (* we'll use smart_pairup_vertices to ensure we don't connect two distant vertices. *)
        let smart_pairedup : (Vertex.t * Vertex.t) list =
          (* ContextualPairExtractor.smart_pairup_vertices method1_vertices method2_vertices *)
          (*   (method1, method2) *)
          let* meth1 = method1_vertices in
          let* meth2 = method2_vertices in
          if not @@ Vertex.equal meth1 meth2 then return (meth1, meth2) else []
        in
        List.fold
          ~f:(fun smol_acc (v1, v2) ->
            G.add_edge_e smol_acc (v1, EdgeLabel.NodeWiseSimilarity, v2)
            |> fun graph -> G.add_edge_e graph (v2, EdgeLabel.NodeWiseSimilarity, v1) )
          ~init:acc smart_pairedup )
      above_threshold_entries graph


  let make_contextual_sim_edge (graph : G.t) : G.t =
    (* we use smart_pairup here, to translate method simliarity to vertex similarity. *)
    let open SimilarVertexPairExtractor in
    let all_trunks = identify_trunks graph in
    let trunk_similarity_map = TrunkPairExtractor.update_trunk_similarity_map all_trunks in
    TrunkSimilarityMap.fold
      (fun ((trunk1, trunk2) as trunk_pair) similarity acc ->
        if similarity >= TrunkSimilarityMap.threshold then
          let contextually_similar_methods =
            ContextualPairExtractor.identify_similar_method_from_similar_trunk trunk_pair graph
          in
          let smart_pairedup =
            contextually_similar_methods
            >>= fun (method1, method2) ->
            ContextualPairExtractor.smart_pairup_vertices trunk1 trunk2 (method1, method2)
          in
          List.fold
            ~f:(fun smol_acc (v1, v2) ->
              G.add_edge_e smol_acc (v1, EdgeLabel.ContextualSimilarity, v2)
              |> fun graph -> G.add_edge_e graph (v2, EdgeLabel.ContextualSimilarity, v1) )
            ~init:acc smart_pairedup
        else acc )
      trunk_similarity_map graph
end
