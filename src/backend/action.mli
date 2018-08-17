
type t =
  | Goto of Pdfdest.t
  | Uri  of string


val pdfobject_of_action : t -> Pdf.pdfobject
 
