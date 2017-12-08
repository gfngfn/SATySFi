open HorzBox

exception InvalidFontAbbrev of font_abbrev

type tag = string

val initialize : string -> unit

val get_metrics_of_word : horz_string_info -> Uchar.t list -> OutputText.t * length * length * length

val get_math_char_info : math_context -> bool -> Uchar.t -> FontFormat.glyph_id * length * length * length * length * FontFormat.math_kern_info option

val get_font_tag : font_abbrev -> tag option

val get_math_string_info : math_context -> math_string_info

val get_math_tag : math_font_abbrev -> tag option

type math_kern_scheme

val no_math_kern : math_kern_scheme

val make_discrete_math_kern : FontFormat.math_kern -> math_kern_scheme

val make_dense_math_kern : (length -> length) -> math_kern_scheme

val get_math_kern : math_context -> math_kern_scheme -> length -> length

val actual_math_font_size : math_context -> length

val get_math_constants : math_context -> FontFormat.math_constants

val get_font_dictionary : Pdf.t -> Pdf.pdfobject
