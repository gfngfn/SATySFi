
open Types
open StaticEnv
open ConfigError
open LengthInterface

val option_type : ('a, 'b) typ -> ('a, 'b) typ

val itemize_type : unit -> ('a, 'b) typ

val get_pdf_mode_initial_context : length -> HorzBox.context_main

val make_pdf_mode_environments : unit -> (Typeenv.t * environment, config_error) result

val make_text_mode_environments : unit -> (Typeenv.t * environment, config_error) result

val default_radical : HorzBox.radical
