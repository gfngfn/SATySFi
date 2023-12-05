
open Types
open LengthInterface
open GraphicBase

type t = input_context

val convert_math_variant_char : input_context -> Uchar.t -> HorzBox.math_kind * Uchar.t

val color : t -> color

val enter_script : t -> t

val math_char_class : t -> HorzBox.math_char_class

val set_math_char_class : HorzBox.math_char_class -> t -> t

val is_in_base_level : t -> bool

val font_size : t -> length

val math_font_key_exn : t -> FontKey.t

val get_math_constants : t -> FontFormat.math_constants

val get_math_string_info : t -> HorzBox.math_string_info
