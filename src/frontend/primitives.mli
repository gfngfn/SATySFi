open Types

val get_initial_context : HorzBox.page_scheme -> input_context

val make_environments : unit -> Typeenv.t * environment

val default_math_context : HorzBox.math_context  (* temporary *)

val default_math_left_paren : HorzBox.paren

val default_math_right_paren : HorzBox.paren

val default_radical : HorzBox.radical
