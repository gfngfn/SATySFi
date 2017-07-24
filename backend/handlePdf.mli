open HorzBox

type t

val create_empty_pdf : string -> unit -> t

val write_page : t -> Pdfpaper.t -> evaled_vert_box list -> unit -> t

val write_to_file : t -> unit -> unit
