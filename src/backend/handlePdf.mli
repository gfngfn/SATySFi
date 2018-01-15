
open LengthInterface
open HorzBox

type t

val create_empty_pdf : string -> t

val write_page : page_size -> page -> page_parts_scheme_func -> t -> t

val write_to_file : t -> unit
