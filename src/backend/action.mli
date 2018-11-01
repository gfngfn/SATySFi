
type t =
  | Goto of Pdfdest.t
  | GotoName of string
  | Uri  of string
  | Rendition of Rendition.t


val pdfobject_of_action : Pdf.t -> t -> Pdf.pdfobject
