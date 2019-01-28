
type t =
  | Goto     of Pdfdest.t
  | GotoName of string
  | Uri      of string


let pdfobject_of_action act =
  let pdfact =
    match act with
    | Goto(dest)         -> Pdfaction.Goto(dest)
    | GotoName(destname) -> Pdfaction.GotoName(destname)
    | Uri(uri)           -> Pdfaction.Uri(uri)
  in
  Pdfaction.pdfobject_of_action pdfact
