open GraphRepr
open InfixOperators
open ListMonad

type trunk = G.Trunk.t

module TrunkFeatures = struct
  (** Do the two trunks share the same callee? **)
  let same_callee_in_trunk_count ((trunk1, trunk2) : trunk * trunk) : int =
    let trunk1_only_methods = trunk1 >>| fst3 and trunk2_only_methods = trunk2 >>| fst3 in
    List.fold
      ~f:(fun acc vertex ->
        let matching =
          List.mem
            ~equal:(fun (meth1, _, _) (meth2, _, _) -> Method.equal meth1 meth2)
            trunk2 vertex
        in
        if matching then acc + 1 else acc )
      ~init:0 trunk1


  (** Do the two trunks share the same suffixes? If it does, what is its length? *)
  let trunks_share_same_suffixes_length ((trunk1, trunk2) : trunk * trunk) : int =
    (* if the two trunks are not equal in size, prepends some fillers *)
    let trunk1_only_methods = trunk1 >>| fst3 and trunk2_only_methods = trunk2 >>| fst3 in
    let trunk1_length = List.length trunk1 and trunk2_length = List.length trunk2 in
    let trunk1_revised, trunk2_revised =
      match Int.compare trunk1_length trunk2_length with
      | -1 ->
          (* trunk1 is shorter: prepend some fillers *)
          let fillers =
            List.init ~f:(fun _ -> Method.of_string "filler") (trunk2_length - trunk1_length)
          in
          (fillers @ trunk1_only_methods, trunk2_only_methods)
      | 0 ->
          (trunk1_only_methods, trunk2_only_methods)
      | 1 ->
          (* trunk2 is shorter: prepend some fillers *)
          let fillers =
            List.init ~f:(fun _ -> Method.of_string "filler") (trunk1_length - trunk2_length)
          in
          (trunk1_only_methods, fillers @ trunk2_only_methods)
      | _ ->
          failwith "this is impossible"
    in
    let zipped = List.zip_exn (List.rev trunk1_revised) (List.rev trunk2_revised) in
    let suffix = List.take_while ~f:(fun (m1, m2) -> Method.equal m1 m2) zipped in
    List.length suffix


  (** Do the two trunks share the same prefixes? If it does, what is its length? *)
  let trunks_share_same_prefixes_length ((trunk1, trunk2) : trunk * trunk) : int =
    (* if the two trunks are not equal in size, prepends some fillers *)
    let trunk1_only_methods = trunk1 >>| fst3 and trunk2_only_methods = trunk2 >>| fst3 in
    let trunk1_length = List.length trunk1 and trunk2_length = List.length trunk2 in
    let trunk1_revised, trunk2_revised =
      match Int.compare trunk1_length trunk2_length with
      | -1 ->
          (* trunk1 is shorter: append some fillers *)
          let fillers =
            List.init ~f:(fun _ -> Method.of_string "filler") (trunk2_length - trunk1_length)
          in
          (trunk1_only_methods @ fillers, trunk2_only_methods)
      | 0 ->
          (trunk1_only_methods, trunk2_only_methods)
      | 1 ->
          (* trunk2 is shorter: append some fillers *)
          let fillers =
            List.init ~f:(fun _ -> Method.of_string "filler") (trunk1_length - trunk2_length)
          in
          (trunk1_only_methods, trunk2_only_methods @ fillers)
      | _ ->
          failwith "this is impossible"
    in
    let zipped = List.zip_exn trunk1_revised trunk2_revised in
    let prefix = List.take_while ~f:(fun (m1, m2) -> Method.equal m1 m2) zipped in
    List.length prefix
end
