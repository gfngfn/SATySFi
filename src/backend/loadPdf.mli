
type bbox = float * float * float * float

val make_xobject : Pdf.t -> Pdf.t -> Pdfpage.t -> Pdf.pdfobject

val get_page : Pdf.t -> int -> (bbox * Pdfpage.t) option
