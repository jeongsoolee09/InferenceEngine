open GraphRepr

module Hashtbl = Caml.Hashtbl

module NSClusters = struct
  let ns_cluster : (Vertex.t list list) ref = ref []

  let set_ns_cluster (new_val: Vertex.t list list) =
    ns_cluster := new_val

  let get_ns_cluster () ~(debug: bool) =
    if debug then
      if List.is_empty !ns_cluster then print_endline "Cache miss (NSClusters)\n" else
        if not @@ List.is_empty !ns_cluster then print_endline "Cache hit (NSClusters)\n";
    !ns_cluster
end
