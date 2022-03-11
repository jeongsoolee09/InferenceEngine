open ListMonad
open InfixOperators
open GraphRepr
open TaintLabel

exception TODO

let bump (old_dist : ProbQuadruple.t) (to_bump_labels : TaintLabel.t list) ~(inc_delta : Float.t)
    ~(dec_delta : Float.t) : ProbQuadruple.t =
  let module TaintLabelSet = Caml.Set.Make (TaintLabel) in
  let bump_label_set = TaintLabelSet.of_list to_bump_labels in
  let increased =
    TaintLabelSet.fold
      (fun label (acc : ProbQuadruple.t) ->
        match label with
        | Source ->
            {acc with src= acc.src +. inc_delta}
        | Sink ->
            {acc with sin= acc.sin +. inc_delta}
        | Sanitizer ->
            {acc with san= acc.san +. inc_delta}
        | None ->
            {acc with non= acc.non +. inc_delta}
        | Indeterminate ->
            acc )
      bump_label_set old_dist
  in
  let labels_to_decrease =
    let all_labels = TaintLabelSet.of_list [Source; Sink; Sanitizer; None] in
    TaintLabelSet.diff all_labels bump_label_set
  in
  let decreased =
    TaintLabelSet.fold
      (fun label (acc : ProbQuadruple.t) ->
        match label with
        | Source ->
            {acc with src= acc.src -. dec_delta}
        | Sink ->
            {acc with sin= acc.sin -. dec_delta}
        | Sanitizer ->
            {acc with san= acc.san -. dec_delta}
        | None ->
            {acc with non= acc.non -. dec_delta}
        | Indeterminate ->
            acc )
      labels_to_decrease increased
  in
  decreased


let overwrite ~(src : Float.t) ~(sin : Float.t) ~(san : Float.t) ~(non : Float.t) : ProbQuadruple.t
    =
  {src; sin; san; non}


let oracle_overwrite (label : TaintLabel.t) : ProbQuadruple.t =
  let inf = Float.infinity and neg = Float.neg_infinity in
  match label with
  | Source ->
      {src= inf; sin= neg; san= neg; non= neg}
  | Sink ->
      {src= neg; sin= inf; san= neg; non= neg}
  | Sanitizer ->
      {src= neg; sin= neg; san= inf; non= neg}
  | None ->
      {src= neg; sin= neg; san= neg; non= inf}
  | Indeterminate ->
      {src= 0.25; sin= 0.25; san= 0.25; non= 0.25}
