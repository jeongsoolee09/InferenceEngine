open ListMonad
open InfixOperators
open GraphRepr

exception TODO

let record_snapshot (graph : G.t) (distmap : Probability.ProbMap.t) =
  (* if confident, then draw it differently *)
  raise TODO
