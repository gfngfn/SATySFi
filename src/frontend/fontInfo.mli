
open MyUtil
open LengthInterface
open HorzBox
open CharBasis

exception InvalidFontAbbrev     of font_abbrev
exception InvalidMathFontAbbrev of math_font_abbrev
exception NotASingleFont        of font_abbrev * abs_path
exception NotATTCElement        of font_abbrev * abs_path * int
exception NotASingleMathFont    of math_font_abbrev * abs_path
exception NotATTCMathFont       of math_font_abbrev * abs_path * int

type tag = string

val initialize : unit -> unit

val get_metrics_of_word : horz_string_info -> uchar_segment list -> OutputText.t * length * length * length

val get_math_char_info : math_font_abbrev -> is_in_base_level:bool -> is_in_display:bool -> is_big:bool -> font_size:length -> Uchar.t list -> OutputText.t * length * length * length * length * FontFormat.math_kern_info option

val get_font_tag : font_abbrev -> tag

val get_math_tag : math_font_abbrev -> tag

val get_math_constants : math_font_abbrev -> FontFormat.math_constants

val get_math_kern_ratio : math_font_abbrev -> FontFormat.math_kern -> float -> float

val get_font_dictionary : Pdf.t -> Pdf.pdfobject
