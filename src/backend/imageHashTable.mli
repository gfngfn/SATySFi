
type file_path = string

type tag = string

type bbox = float * float * float * float

type key

type value_main =
  | PDFImage   of Pdf.t * Pdfpage.t
  | OtherImage of Images.format * Pdf.pdfobject * int * int * file_path

type value = tag * bbox * value_main

val initialize : unit -> unit

val add_pdf : file_path -> int -> key

val add_image : file_path -> key

val find : key -> value

val fold : (key -> value -> 'a -> 'a) -> 'a -> 'a
