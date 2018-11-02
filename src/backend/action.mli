
type link =
  | Goto      of Pdfdest.t
  | GotoName  of string
  | Uri       of string

type screen =
  | Rendition of int * Rendition.t

val pdfobject_of_link_action : link -> Pdf.pdfobject

val pdfobject_of_screen_action : Pdf.t -> int -> screen -> Pdf.pdfobject
