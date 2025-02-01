
open Types
open LengthInterface
open StaticEnv

val option_type : ('a, 'b) typ -> ('a, 'b) typ

val itemize_type : unit -> ('a, 'b) typ

val get_pdf_mode_initial_context : length -> HorzBox.context_main

val make_pdf_mode_environments : unit -> Typeenv.t * environment

val make_text_mode_environments : unit -> Typeenv.t * environment

val default_radical : HorzBox.radical
