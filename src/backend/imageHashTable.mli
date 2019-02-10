
open MyUtil

type tag = string

type bbox = float * float * float * float

type key

type value_main =
  | PDFImage   of Pdf.t * Pdfpage.t
  | OtherImage of Images.format * Pdf.pdfobject * int * int * abs_path

type value = tag * bbox * value_main

exception CannotLoadPdf          of string * abs_path * int
exception CannotLoadImage        of string * abs_path
exception ImageOfWrongFileType   of abs_path
exception UnsupportedColorModel  of Images.colormodel * abs_path

val initialize : unit -> unit

val add_pdf : abs_path -> int -> key

val add_image : abs_path -> key

val find : key -> value

val fold : (key -> value -> 'a -> 'a) -> 'a -> 'a
