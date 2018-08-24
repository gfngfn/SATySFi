
open Types
open LengthInterface

val option_type : ('a, 'b) typ -> ('a, 'b) typ

val itemize_type : unit -> ('a, 'b) typ

val get_initial_context : length -> HorzBox.context_main

val make_environments : unit -> Typeenv.t * environment

val default_radical : HorzBox.radical
