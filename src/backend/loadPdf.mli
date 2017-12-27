
type bbox = float * float * float * float

val make_xobject : Pdf.t -> int -> (bbox * Pdf.pdfobject) option
