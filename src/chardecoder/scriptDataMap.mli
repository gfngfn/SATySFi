
open CharBasis

val set_from_file : string -> unit

val divide_by_script : (Uchar.t * line_break_class * break_opportunity ref) list -> (script * (Uchar.t * line_break_class * break_opportunity ref) list) list
