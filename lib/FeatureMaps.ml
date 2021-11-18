open InfixOperators
open GraphRepr
open ListMonad

module NodeWiseFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (String)
  include WithMethodDomain

  type t = Int.t WithMethodDomain.t

  (** Make a default map. *)
  let init (graph : G.t) : t =
    List.fold ~f:(fun acc meth -> add meth 0 acc) (G.all_methods_of_graph graph) ~init:empty


  let strong_update (map : t) (meth_ : string) (new_ : int) = remove meth_ map |> add meth_ new_
end

module ContextualFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (String)
  include WithMethodDomain

  type t = Int.t WithMethodDomain.t

  (** Make a default map. *)
  let init (graph : G.t) : t =
    List.fold ~f:(fun acc meth -> add meth 0 acc) (G.all_methods_of_graph graph) ~init:empty


  let strong_update (map : t) (meth_ : string) (new_ : int) = remove meth_ map |> add meth_ new_
end
