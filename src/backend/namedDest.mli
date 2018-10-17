
open LengthInterface

val register : string -> point -> unit

val get : string -> string

val notify_pagebreak : int -> unit

val add_to_pdf : Pdf.t -> Pdf.t
