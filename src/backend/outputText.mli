
type t

val empty_hex_style : t

val append_kern : t -> int -> t

val append_glyph_id : t -> FontFormat.glyph_id -> t

val to_TJ_argument : t -> Pdf.pdfobject

val pp : Format.formatter -> t -> unit
