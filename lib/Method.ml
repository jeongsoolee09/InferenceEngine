open ListMonad
open InfixOperators
module F = Format

type t = NormalMethod of String.t | Initializer of String.t [@@deriving compare, equal, sexp]

let of_string (string : String.t) : t =
  if String.is_substring ~substring:"<init>" string then Initializer string else NormalMethod string


let to_string (method_ : t) : string =
  match method_ with NormalMethod str -> str | Initializer str -> str


let is_initializer = function Initializer _ -> true | _ -> false

let dummy = of_string "Dummy Dummy.dummy()"
