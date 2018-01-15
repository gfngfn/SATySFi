
open Types
open LengthInterface

val get_initial_context : length -> EvalVarID.t -> HorzBox.input_context

val make_environments : unit -> Typeenv.t * environment
(*
val default_math_left_paren : HorzBox.paren

val default_math_right_paren : HorzBox.paren
*)
val default_radical : HorzBox.radical
