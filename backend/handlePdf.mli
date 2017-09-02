open HorzBox

type t

val create_empty_pdf : string -> t

val write_page : Pdfpaper.t -> evaled_vert_box list -> t -> t

val write_to_file : t -> unit
