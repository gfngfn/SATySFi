
module Types = Types_
open Types
open LengthInterface

val option_type : 'a typ -> 'a typ

val itemize_type : unit -> 'a typ

val get_initial_context : length -> HorzBox.context_main

val make_environments : unit -> Typeenv.t * environment

val default_radical : HorzBox.radical
