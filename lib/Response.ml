open InfixOperators
open ListMonad
open GraphRepr
open EdgeLabel

type t = ForLabel of (Method.t * TaintLabel.t) | ForYesOrNo of (Method.t * TaintLabel.t * bool)

let to_string (response : t) : string =
  match response with
  | ForLabel (meth_, label) ->
      F.asprintf "ForLabel (%s, %s)" (Method.to_string meth_) (TaintLabel.to_string label)
  | ForYesOrNo (meth_, label, bool) ->
      F.asprintf "ForYesOrNo (%s, %s, %s)" (Method.to_string meth_) (TaintLabel.to_string label)
        (Bool.to_string bool)


let list_to_string (responses : t list) : string =
  let contents =
    List.fold responses ~f:(fun acc response -> acc ^ to_string response ^ ", ") ~init:""
  in
  F.asprintf "[%s]" contents


let get_method : t -> Method.t = function
  | ForLabel (meth, _) ->
      meth
  | ForYesOrNo (meth, _, _) ->
      meth


let get_label : t -> TaintLabel.t = function
  | ForLabel (_, label) ->
      label
  | ForYesOrNo (_, label, _) ->
      label


let get_yesorno : t -> bool = function
  | ForYesOrNo (_, _, bool) ->
      bool
  | _ ->
      raise @@ Invalid_argument "this is not a response of a question asking for label"


let response_of_dist (method_ : Method.t) (dist : ProbQuadruple.t) : t =
  let label = ProbQuadruple.determine_label dist in
  ForLabel (method_, label)


let response_of_string_forlabel (method_ : Method.t) (response_str : string) : t =
  match response_str with
  | "src" | "source" ->
      ForLabel (method_, TaintLabel.Source)
  | "sin" | "sink" ->
      ForLabel (method_, TaintLabel.Sink)
  | "san" | "sanitizer" ->
      ForLabel (method_, TaintLabel.Sanitizer)
  | "non" | "none" ->
      ForLabel (method_, TaintLabel.None)
  | otherwise ->
      raise @@ Invalid_argument otherwise


let response_of_string_foryesorno (method_ : Method.t) (label : TaintLabel.t) (response_str : string)
    : t =
  match response_str with
  | "yes" | "y" ->
      ForYesOrNo (method_, label, true)
  | "no" | "n" ->
      ForYesOrNo (method_, label, false)
  | otherwise ->
      raise @@ Invalid_argument otherwise


let is_forlabel = function ForLabel _ -> true | ForYesOrNo _ -> false

let is_foryesorno = function ForLabel _ -> false | ForYesOrNo _ -> true
