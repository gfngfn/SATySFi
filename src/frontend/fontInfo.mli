
open MyUtil
open LengthInterface
open HorzBox
open CharBasis
open Types

exception InvalidFontAbbrev     of font_abbrev
exception InvalidMathFontAbbrev of math_font_abbrev
exception NotASingleFont        of font_abbrev * abs_path
exception NotATTCElement        of font_abbrev * abs_path * int
exception NotASingleMathFont    of math_font_abbrev * abs_path
exception NotATTCMathFont       of math_font_abbrev * abs_path * int

type tag = string

val initialize : unit -> unit

val get_metrics_of_word : horz_string_info -> uchar_segment list -> OutputText.t * length * length * length

val get_math_char_info : math_context -> bool -> bool -> Uchar.t list -> OutputText.t * length * length * length * length * FontFormat.math_kern_info option

val get_font_tag : font_abbrev -> tag

val get_math_string_info : math_context -> math_string_info

val get_math_tag : math_font_abbrev -> tag

type math_kern_scheme

val no_math_kern : math_kern_scheme

val make_discrete_math_kern : FontFormat.math_kern -> math_kern_scheme

val make_dense_math_kern : math_kern_func -> math_kern_scheme

val get_math_kern : math_context -> math_kern_scheme -> length -> length

val get_axis_height : math_font_abbrev -> length -> length

val actual_math_font_size : math_context -> length

val get_math_constants : math_context -> FontFormat.math_constants

val get_font_dictionary : Pdf.t -> Pdf.pdfobject
