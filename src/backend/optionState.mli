
open MyUtil

type input_kind =
  | SATySFi
  | Markdown of string

type output_mode =
  | PdfMode
  | TextMode of string list

type state = {
  input_file             : abs_path;
  output_file            : abs_path option;
  extra_config_paths     : (string list) option;
  output_mode            : output_mode;
  input_kind             : input_kind;
  page_number_limit      : int;
  show_full_path         : bool;
  debug_show_bbox        : bool;
  debug_show_space       : bool;
  debug_show_block_bbox  : bool;
  debug_show_block_space : bool;
  debug_show_overfull    : bool;
  type_check_only        : bool;
  bytecomp               : bool;
  show_fonts             : bool;
  no_default_config      : bool;
}

val set : state -> unit

val get_input_kind : unit -> input_kind

val get_input_file : unit -> abs_path

val get_output_file : unit -> abs_path option

val is_type_check_only : unit -> bool

val is_bytecomp_mode : unit -> bool

val does_show_full_path : unit -> bool

val does_show_fonts : unit -> bool

val does_debug_show_bbox : unit -> bool

val does_debug_show_space : unit -> bool

val does_debug_show_block_bbox : unit -> bool

val does_debug_show_block_space : unit -> bool

val does_debug_show_overfull : unit -> bool

val get_mode : unit -> output_mode

val get_extra_config_paths : unit -> string list option

val get_no_default_config_paths : unit -> bool

val get_page_number_limit : unit -> int

val job_directory : unit -> string

val is_text_mode : unit -> bool
