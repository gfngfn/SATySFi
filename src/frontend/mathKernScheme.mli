
open Types
open LengthInterface

type t

val zero : t

val make_discrete : FontFormat.math_kern -> t

val make_dense : HorzBox.math_kern_func -> t

(** Given a correction height, this function returns
    a kerning length (negative value stands for being closer to the previous glyph). *)
val calculate : input_context -> t -> length -> length
