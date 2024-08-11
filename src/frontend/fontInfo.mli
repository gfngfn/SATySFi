
open MyUtil
open FontError
open LengthInterface
open HorzBox
open CharBasis

exception FontInfoError of font_error

type key = FontKey.t

type math_key = FontKey.t

type tag = string

val initialize : unit -> unit

val add_single : abs_path -> key

val add_ttc : abs_path -> int -> key

val add_math_single : abs_path -> math_key

val add_math_ttc : abs_path -> int -> math_key

val get_metrics_of_word : horz_string_info -> uchar_segment list -> OutputText.t * length * length * length

val get_math_char_info :
  math_key ->
  is_in_base_level:bool ->
  is_in_display:bool ->
  is_big:bool ->
  font_size:length ->
  Uchar.t list ->
  OutputText.t * length * length * length * length * FontFormat.math_kern_info option

val get_font_tag : key -> tag

val get_math_tag : math_key -> tag

val get_math_constants : math_key -> FontFormat.math_constants

val get_math_kern_ratio : math_key -> FontFormat.math_kern -> float -> float

val get_font_dictionary : Pdf.t -> Pdf.pdfobject
