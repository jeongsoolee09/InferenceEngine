open InfixOperators
open GraphRepr
open ListMonad

module NodeWiseFeatureMap = struct
  module WithMethodDomain = Caml.Map.Make (String)
  include WithMethodDomain

  type feature = Int of int | String of string | Bool of bool [@@deriving equal]

  type t = feature list WithMethodDomain.t

  (** Make a default map. *)
  let init (graph : G.t) : t =
    List.fold ~f:(fun acc meth -> add meth [Int 0] acc) (G.all_methods_of_graph graph) ~init:empty


  let strong_update (map : t) (meth_ : string) (new_ : feature list) =
    remove meth_ map |> add meth_ new_
end
