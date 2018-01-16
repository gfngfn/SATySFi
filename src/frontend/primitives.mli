
open Types
open LengthInterface

val get_initial_context : length -> EvalVarID.t -> HorzBox.input_context

val make_environments : string -> Typeenv.t * environment

val default_radical : HorzBox.radical
