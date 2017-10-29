open Types

val default_context : input_context
val default_paddings : HorzBox.paddings
(*
val frame_deco_S : HorzBox.decoration
val frame_deco_H : HorzBox.decoration
val frame_deco_M : HorzBox.decoration
val frame_deco_T : HorzBox.decoration
*)
val frame_deco_VS : HorzBox.decoration
val frame_deco_VH : HorzBox.decoration
val frame_deco_VM : HorzBox.decoration
val frame_deco_VT : HorzBox.decoration

val make_environments : unit -> Typeenv.t * environment
