open InfixOperators
open ListMonad
open GraphRepr

type t = AskingForLabel of Method.t | AskingForConfirmation of (Method.t * TaintLabel.t)

(** make a prompt message out of a question term. *)
let make_prompt (question : t) : string =
  match question with
  | AskingForLabel meth ->
      F.asprintf "What label does %s bear? [src|sin|san|non]: " (Method.to_string meth)
  | AskingForConfirmation (meth, label) ->
      F.asprintf "Method %s is a %s, right? [yes|no]: " (Method.to_string meth)
        (TaintLabel.to_string label)


let get_method (question : t) : Method.t =
  match question with AskingForLabel meth -> meth | AskingForConfirmation (meth, _) -> meth


let get_label (question : t) : TaintLabel.t =
  match question with
  | AskingForConfirmation (_, label) ->
      label
  | _ ->
      raise @@ Invalid_argument "hahaha"


let is_askingforlabel = function AskingForLabel _ -> true | _ -> false

let is_askingforconfirmation = function AskingForConfirmation _ -> true | _ -> false
