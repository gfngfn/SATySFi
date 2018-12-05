
type t =
  | Goto of Pdfdest.t
  | GotoName of string
  | Uri  of string


val pdfobject_of_action : t -> Pdf.pdfobject
