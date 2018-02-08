
open Types
open LengthInterface

val option_type : mono_type -> mono_type

val get_initial_context : length -> HorzBox.context_main

val make_environments : string -> Typeenv.t * environment

val default_radical : HorzBox.radical
