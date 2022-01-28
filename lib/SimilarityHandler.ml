(** module that extracts similar vertex pairs. *)

open Yojson.Basic
open ListMonad
open InfixOperators
open GraphRepr
open Chain
open NodeWiseFeatures
open ContextualFeatures

module MethodPair = struct
  type t = Method.t * Method.t [@@deriving compare]

  let to_string (m1, m2) = F.asprintf "(%s, %s)" (Method.to_string m1) (Method.to_string m2)
end

module NodeWiseSimilarityMap = struct
  module WithMethodPairDomain = Caml.Map.Make (MethodPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t

  let threshold = 5 (* TEMP *)

  let make_empty (all_methods : Method.t list) : t =
    let method_pairs =
      let* method1 = all_methods in
      let* method2 = all_methods in
      if not @@ Method.equal method1 method2 then return (method1, method2) else []
    in
    List.fold
      ~f:(fun acc pair -> WithMethodPairDomain.add pair 0 acc)
      method_pairs ~init:WithMethodPairDomain.empty


  let is_similar_nodewise (meth1 : Method.t) (meth2 : Method.t) (map : t) : bool =
    Int.( >= ) (find (meth1, meth2) map) threshold


  let to_alist (map : t) = fold (fun k v acc -> (k, v) :: acc) map []

  let of_alist alist = List.fold ~f:(fun acc (k, v) -> add k v acc) alist ~init:empty
end

module TrunkSimilarityMap = struct
  module TrunkPair = struct
    type t = Trunk.t * Trunk.t [@@deriving compare, equal]
  end

  module WithTrunkPairDomain = Caml.Map.Make (TrunkPair)
  include WithTrunkPairDomain

  type t = Int.t WithTrunkPairDomain.t

  let threshold = 1 (* TEMP *)

  let make_empty (all_trunks : trunk array) : t =
    let trunk_pairs =
      let raw_carpro = Array.cartesian_product all_trunks all_trunks in
      Array.filter ~f:(fun (trunk1, trunk2) -> not @@ Trunk.equal trunk1 trunk2) raw_carpro
    in
    Array.fold
      ~f:(fun acc pair -> WithTrunkPairDomain.add pair 0 acc)
      trunk_pairs ~init:WithTrunkPairDomain.empty
end

module ContextualSimilarityMap = struct
  module WithMethodPairDomain = Caml.Map.Make (MethodPair)
  include WithMethodPairDomain

  type t = Int.t WithMethodPairDomain.t

  let make_empty (all_methods : Method.t list) : t =
    let method_pairs =
      let* method1 = all_methods in
      let* method2 = all_methods in
      if not @@ Method.equal method1 method2 then return (method1, method2) else []
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
      [| (is_both_framework_code, 4)
       ; (belong_to_same_class, 2)
       ; (belong_to_same_package, 2)
       ; (return_type_is_another's_class, 3)
       ; (is_both_java_builtin, 2)
       ; (is_both_initializer, 4)
       ; (has_same_annots, 6) |]


    (** Run all extractors for every method pair. *)
    let get_nodewise_similarity (method_pair : Method.t * Method.t) : int =
      (* execute all extractors. *)
      Array.fold
        ~f:(fun current_score (feature, score) ->
          if feature method_pair then current_score + score else current_score )
        ~init:0 pairwise_features_and_their_scores


    let project_root = Deserializer.deserialize_config ()

    (* TEMP: Sys.readdir "." should be replaced with Deserializer.deserialize_config (). *)
    let ns_map_already_serialized (comp_unit : string) : bool =
      Array.exists
        ~f:(fun dir -> String.is_substring ~substring:(F.asprintf "ns_map_%s" comp_unit) dir)
        (Sys.readdir ".")


    (* TEMP: Sys.readdir "." should be replaced with Deserializer.deserialize_config (). *)
    let find_serialized_ns_map (comp_unit : string) : string =
      Array.find_exn
        ~f:(fun dir -> String.is_substring ~substring:(F.asprintf "ns_map_%s" comp_unit) dir)
        (Sys.readdir ".")


    (** main functionality: calculate the nodewise simliarity of each method and organize those in a
        table. *)
    let init_nodewise_similarity_map =
      let cache = Hashtbl.create 777 in
      fun (comp_unit : string) (all_methods : Method.t list) : NodeWiseSimilarityMap.t ->
        match Hashtbl.find_opt cache all_methods with
        | None ->
            if not @@ ns_map_already_serialized comp_unit then (
              let out =
                let map_array =
                  NodeWiseSimilarityMap.make_empty all_methods
                  |> fun map ->
                  NodeWiseSimilarityMap.fold (fun k _ acc -> k :: acc) map []
                  |> List.filter ~f:(fun (m1, m2) ->
                         (not << Method.is_frontend) m1 && (not << Method.is_frontend) m2 )
                  |> Array.of_list
                in
                Out_channel.print_endline
                @@ Format.asprintf "domain size of nodewise sim map is %d\n"
                     (Array.length map_array) ;
                let mapped =
                  Array.map ~f:(fun pair -> (pair, get_nodewise_similarity pair)) map_array
                in
                mapped |> Array.to_list |> NodeWiseSimilarityMap.of_alist
              in
              Hashtbl.add cache all_methods out ;
              let marshal_out_chan =
                Out_channel.create (F.asprintf "%s_ns_map_%s.bin" (make_now_string 9) comp_unit)
              in
              Marshal.to_channel marshal_out_chan out [] ;
              Out_channel.close marshal_out_chan ;
              out )
            else
              let marshal_in_chan = In_channel.create (find_serialized_ns_map comp_unit) in
              let from_marshal = Marshal.from_channel marshal_in_chan in
              In_channel.close marshal_in_chan ;
              Hashtbl.add cache all_methods from_marshal ;
              from_marshal
        | Some res ->
            res
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
    let init_trunk_similarity_map (all_trunks : trunk array) : TrunkSimilarityMap.t =
      let initial_map = TrunkSimilarityMap.make_empty all_trunks in
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
    let find_bidirectionals_in_trunk (trunk : trunk) (graph : G.t) : G.V.t list =
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
        : (Method.t * Method.t) list =
      (* 1. trunk's roots are similar *)
      let trunk1_root = trunk1.(0) and trunk2_root = trunk2.(0) in
      (* 2. trunk's leaves are similar *)
      let trunk1_leaf = Array.last trunk1 and trunk2_leaf = Array.last trunk2 in
      let open NodeWiseFeatures.SingleFeature in
      let root_pair_list =
        if
          (not @@ Vertex.equal trunk1_root trunk2_root)
          && not
               ( (Method.is_initializer @@ Vertex.get_method trunk1_root)
               || (Method.is_initializer @@ Vertex.get_method trunk2_root) )
        then
          [ (Vertex.get_method trunk1_root, Vertex.get_method trunk2_root)
          ; (Vertex.get_method trunk2_root, Vertex.get_method trunk1_root) ]
        else []
      and leaf_pair_list =
        if
          (not @@ Vertex.equal trunk1_leaf trunk2_leaf)
          && not
               ( (Method.is_initializer @@ Vertex.get_method trunk1_leaf)
               || (Method.is_initializer @@ Vertex.get_method trunk2_leaf) )
        then [(fst3 trunk1_leaf, fst3 trunk2_leaf); (fst3 trunk2_leaf, fst3 trunk1_leaf)]
        else []
      in
      (* 3. trunks's redefines are similar *)
      let all_redefine_slices =
        RedefineHandler.collect_redefines @@ Deserializer.deserialize_json ()
      in
      let trunk1_redefines =
        Array.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk1
      and trunk2_redefines =
        Array.filter ~f:(RedefineHandler.is_redefine_vertex all_redefine_slices) trunk2
      in
      (* we can make redefine pairs: make a carpro of redefines *)
      (* TODO: making a carpro is naive. The logic should be refined *)
      let redefines_carpro =
        let raw_carpro =
          Array.cartesian_product
            (Array.map ~f:Vertex.get_method trunk1_redefines)
            (Array.map ~f:Vertex.get_method trunk2_redefines)
        in
        Array.filter ~f:(fun (v1, v2) -> not @@ Method.equal v1 v2) raw_carpro |> Array.to_list
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
        ((trunk_a_similar, trunk_b_similar) : Method.t * Method.t) : (G.V.t * G.V.t) list =
      let with_list_index (lst : 'a list) : (int * 'a) list =
        List.rev @@ List.foldi ~f:(fun index acc elem -> (index, elem) :: acc) ~init:[] lst
      in
      let trunk_a_processed =
        Array.to_list trunk_a
        |> List.filter ~f:(fun vertex -> Method.equal trunk_a_similar (Vertex.get_method vertex))
        |> List.stable_dedup |> with_list_index
      and trunk_b_processed =
        Array.to_list trunk_b
        |> List.filter ~f:(fun vertex -> Method.equal trunk_b_similar (Vertex.get_method vertex))
        |> List.stable_dedup |> with_list_index
      in
      if Int.( >= ) (Array.length trunk_a) (Array.length trunk_b) then
        (* loop on trunk_b *)
        List.fold
          ~f:(fun acc ((b_index, b_elem) as b_tuple) ->
            (* find the a_tuple in trunk_a with the minimum difference in terms of index *)
            let a_elem_with_smallest_diff =
              List.fold
                ~f:(fun ((current_min, _) as current_min_tuple) ((a_index, a_elem) as a_tuple) ->
                  let diff = Int.abs (Int.( - ) a_index b_index) in
                  if diff <= current_min then a_tuple else current_min_tuple )
                ~init:(Int.max_value, Vertex.dummy) trunk_a_processed
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
                ~init:(Int.max_value, Vertex.dummy) trunk_b_processed
            in
            (a_elem, snd b_elem_with_smallest_diff) :: acc )
          ~init:[] trunk_a_processed
  end
end

module EstablishSimEdges = struct
  let make_nodewise_sim_edge (graph : G.t) : G.t =
    let open SimilarVertexPairExtractor in
    (* use only unmarked vertices; only they are relevant *)
    let unmarked_vertices =
      G.fold_vertex
        (fun vertex acc ->
          if ProbQuadruple.is_indeterminate (Vertex.get_dist vertex) then vertex :: acc else acc )
        graph []
    in
    let unmarked_methods = unmarked_vertices >>| Vertex.get_method |> List.stable_dedup in
    print_endline
    @@ F.asprintf "There are %d vertices, of which %d are unmarked \n"
         (List.length (G.all_vertices_of_graph graph))
         (List.length unmarked_vertices) ;
    let nodewise_similarity_map =
      NodewisePairExtractor.init_nodewise_similarity_map graph.comp_unit unmarked_methods
    in
    let above_threshold_entries =
      NodeWiseSimilarityMap.filter
        (fun _ similarity -> similarity >= NodeWiseSimilarityMap.threshold)
        nodewise_similarity_map
    in
    NodeWiseSimilarityMap.fold
      (fun (method1, method2) _ acc ->
        let method1_vertices =
          List.filter ~f:(fun (meth, _, _) -> Method.equal meth method1) unmarked_vertices
        and method2_vertices =
          List.filter ~f:(fun (meth, _, _) -> Method.equal meth method2) unmarked_vertices
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
    (* use only relevant trunks, which contains unmarked vertices *)
    let all_relevant_trunks =
      let all_trunks = Trunk.identify_longest_trunks graph in
      Array.filter
        ~f:(fun trunk ->
          Array.exists
            ~f:(fun vertex -> ProbQuadruple.is_indeterminate (Vertex.get_dist vertex))
            trunk )
        all_trunks
    in
    let trunk_similarity_map = TrunkPairExtractor.init_trunk_similarity_map all_relevant_trunks in
    TrunkSimilarityMap.fold
      (fun ((trunk1, trunk2) as trunk_pair) similarity acc ->
        if similarity >= TrunkSimilarityMap.threshold then
          let contextually_similar_methods =
            ContextualPairExtractor.identify_similar_method_from_similar_trunk trunk_pair graph
          in
          let smart_pairedup =
            contextually_similar_methods
            >>= ContextualPairExtractor.smart_pairup_vertices trunk1 trunk2
          in
          List.fold
            ~f:(fun smol_acc (v1, v2) ->
              G.add_edge_e smol_acc (v1, EdgeLabel.ContextualSimilarity, v2)
              |> fun graph -> G.add_edge_e graph (v2, EdgeLabel.ContextualSimilarity, v1) )
            ~init:acc smart_pairedup
        else acc )
      trunk_similarity_map graph
end
