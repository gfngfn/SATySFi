
open LengthInterface

val register_location : string -> point -> unit
val notify_new_page : int -> unit
val add_locations : Pdf.t -> Pdf.t
val get_location : string -> string
