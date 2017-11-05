open HorzBox

type t

val create_empty_pdf : string -> t

val write_page : page_scheme -> evaled_vert_box list -> t -> t

val write_to_file : t -> unit

val pdfops_of_evaled_horz_box : point -> evaled_horz_box list -> Pdfops.t list
