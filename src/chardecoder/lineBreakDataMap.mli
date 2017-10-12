
open CharBasis

val set_from_file : string -> unit

val find : Uchar.t -> line_break_class

val append_break_opportunity : Uchar.t list -> (Uchar.t * line_break_class * break_opportunity ref) list

val print_trilist : (Uchar.t * line_break_class * break_opportunity ref) list -> unit
