open GraphRepr
open InfixOperators
open ListMonad

type trunk = Trunk.t

module TrunkFeatures = struct
  (** Do the two trunks share the same callee? **)
  let same_callee_in_trunk_count ((trunk1, trunk2) : trunk * trunk) : int =
    Array.fold
      ~f:(fun acc vertex ->
        let matching =
          Array.mem
            ~equal:(fun v1 v2 -> Method.equal (Vertex.get_method v1) (Vertex.get_method v2))
            trunk2 vertex
        in
        if matching then acc + 1 else acc )
      ~init:0 trunk1


  (** Do the two trunks share the same suffixes? If it does, what is its length? *)
  let trunks_share_same_suffixes_length ((trunk1, trunk2) : trunk * trunk) : int =
    (* if the two trunks are not equal in size, prepends some fillers *)
    let trunk1_only_methods = Array.map ~f:fst3 trunk1
    and trunk2_only_methods = Array.map ~f:fst3 trunk2 in
    let trunk1_length = Array.length trunk1 and trunk2_length = Array.length trunk2 in
    let trunk1_revised, trunk2_revised =
      match Int.compare trunk1_length trunk2_length with
      | -1 ->
          (* trunk1 is shorter: prepend some fillers *)
          let fillers =
            Array.init ~f:(fun _ -> Method.of_string "filler") (trunk2_length - trunk1_length)
          in
          (Array.append fillers trunk1_only_methods, trunk2_only_methods)
      | 0 ->
          (trunk1_only_methods, trunk2_only_methods)
      | 1 ->
          (* trunk2 is shorter: prepend some fillers *)
          let fillers =
            Array.init ~f:(fun _ -> Method.of_string "filler") (trunk1_length - trunk2_length)
          in
          (trunk1_only_methods, Array.append fillers trunk2_only_methods)
      | _ ->
          failwith "this is impossible"
    in
    let acc = ref 0 in
    for i = Array.length trunk1_revised - 1 downto 0 do
      if Method.equal trunk1_revised.(i) trunk2_revised.(i) then Int.incr acc
    done ;
    !acc


  (** Do the two trunks share the same prefixes? If it does, what is its length? *)
  let trunks_share_same_prefixes_length ((trunk1, trunk2) : trunk * trunk) : int =
    (* if the two trunks are not equal in size, prepends some fillers *)
    let trunk1_only_methods = Array.map ~f:fst3 trunk1
    and trunk2_only_methods = Array.map ~f:fst3 trunk2 in
    let trunk1_length = Array.length trunk1 and trunk2_length = Array.length trunk2 in
    let trunk1_revised, trunk2_revised =
      match Int.compare trunk1_length trunk2_length with
      | -1 ->
          (* trunk1 is shorter: append some fillers *)
          let fillers =
            Array.init ~f:(fun _ -> Method.of_string "filler") (trunk2_length - trunk1_length)
          in
          (Array.append trunk1_only_methods fillers, trunk2_only_methods)
      | 0 ->
          (trunk1_only_methods, trunk2_only_methods)
      | 1 ->
          (* trunk2 is shorter: append some fillers *)
          let fillers =
            Array.init ~f:(fun _ -> Method.of_string "filler") (trunk1_length - trunk2_length)
          in
          (trunk1_only_methods, Array.append trunk2_only_methods fillers)
      | _ ->
          failwith "this is impossible"
    in
    let acc = ref 0 in
    for i = Array.length trunk1_revised - 1 downto 0 do
      if Method.equal trunk1_revised.(i) trunk2_revised.(i) then Int.incr acc
    done ;
    !acc
end
