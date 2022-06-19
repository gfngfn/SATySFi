
open MyUtil
open LengthInterface

type bbox = float * float * float * float

type key

val initialize : unit -> unit

val add_pdf : abs_path -> int -> key

val add_image : abs_path -> key

val get_height_from_width : key -> length -> length

val get_size : key -> Length.t * Length.t

val get_ratio : key -> length -> length -> float * float

val get_xobject_dictionary : Pdf.t -> Pdf.pdfobject

val get_tag : key -> string

val get_color_space : key -> Pdf.pdfobject option
