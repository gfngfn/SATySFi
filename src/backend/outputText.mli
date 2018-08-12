
type t

val empty_hex_style : t

val append_kern : t -> int -> t

val append_glyph_synthesis : t -> FontFormat.per_mille -> FontFormat.glyph_synthesis -> t

val to_TJ_arguments : t -> (FontFormat.per_mille option * Pdf.pdfobject list) list

val pp : Format.formatter -> t -> unit
