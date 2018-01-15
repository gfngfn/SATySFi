
open LengthInterface
open HorzBox

type t

val create_empty_pdf : string -> t

val write_page : page_scheme -> page -> header_or_footer -> header_or_footer -> t -> t

val write_to_file : t -> unit
