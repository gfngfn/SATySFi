
type file_path = string

type bbox = float * float * float * float

type key

exception CannotLoadPdf of file_path * int

val initialize : unit -> unit

val add_pdf : file_path -> int -> key

val get_bounding_box : key -> bbox

val get_xobject_dictionary : unit -> Pdf.pdfobject

val get_tag : key -> string
