
type input_kind =
  | SATySFi
  | Markdown of string

val set_input_kind : input_kind -> unit
val get_input_kind : unit -> input_kind

val set_input_file : string -> unit
val input_file : unit -> string option

val set_output_file : string -> unit
val output_file : unit -> string option

val set_type_check_only : unit -> unit
val type_check_only : unit -> bool

val set_bytecomp_mode : unit -> unit
val bytecomp_mode : unit -> bool

val set_show_full_path : unit -> unit
val show_full_path : unit -> bool

val set_debug_show_bbox : unit -> unit
val debug_show_bbox : unit -> bool

val set_debug_show_space : unit -> unit
val debug_show_space : unit -> bool

val set_text_mode : string list -> unit
val get_mode : unit -> (string list) option
val is_text_mode : unit -> bool
