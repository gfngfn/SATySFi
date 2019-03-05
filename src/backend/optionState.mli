
open MyUtil

type input_kind =
  | SATySFi
  | Markdown of string

val set_input_kind : input_kind -> unit
val get_input_kind : unit -> input_kind

val set_input_file : abs_path -> unit
val input_file : unit -> abs_path option

val job_directory : unit -> string

val set_output_file : abs_path -> unit
val output_file : unit -> abs_path option

val set_type_check_only : unit -> unit
val type_check_only : unit -> bool

val set_bytecomp_mode : unit -> unit
val bytecomp_mode : unit -> bool

val set_show_full_path : unit -> unit
val show_full_path : unit -> bool

val set_show_fonts : unit -> unit
val show_fonts : unit -> bool

val set_debug_show_bbox : unit -> unit
val debug_show_bbox : unit -> bool

val set_debug_show_space : unit -> unit
val debug_show_space : unit -> bool

val set_debug_show_block_bbox : unit -> unit
val debug_show_block_bbox : unit -> bool

val set_text_mode : string list -> unit
val get_mode : unit -> (string list) option
val is_text_mode : unit -> bool

val set_extra_config_paths : string list -> unit
val get_extra_config_paths : unit -> string list option
