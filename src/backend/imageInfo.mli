
type file_path = string

type bbox = float * float * float * float

type key

exception CannotLoadPdf          of file_path * int
exception ImageOfWrongFileType   of file_path
exception UnsupportedColorModel  of Images.colormodel

val initialize : unit -> unit

val add_pdf : file_path -> int -> key

val add_image : file_path -> key

val get_bounding_box : key -> bbox

val get_xobject_dictionary : Pdf.t -> Pdf.pdfobject

val get_tag : key -> string

val get_color_space : key -> Pdf.pdfobject option
