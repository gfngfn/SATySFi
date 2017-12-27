
open HorzBox

exception CannotLoadPdf of file_path * int

val initialize : unit -> unit

val get_xobject_dictionary : unit -> Pdf.pdfobject
