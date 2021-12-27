open InfixOperators
open GraphRepr
open ListMonad

module NodeWiseFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (Method)
  include WithMethodDomain

  type feature = Int of int | String of string | Bool of bool [@@deriving equal]

  type t = feature list WithMethodDomain.t

  (** Make a default map. *)
  let init (graph : G.t) : t =
    List.fold (G.all_non_frontend_methods_of_graph graph) ~f:(fun acc meth -> add meth [Int 0] acc) ~init:empty


  let strong_update (map : t) (meth_ : Method.t) (new_ : feature list) =
    remove meth_ map |> add meth_ new_
end
