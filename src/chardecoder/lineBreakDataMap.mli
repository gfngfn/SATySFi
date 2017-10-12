
open CharBasis

val set_from_file : string -> unit

val find : Uchar.t -> line_break_class
(*
val append_property : Uchar.t list -> (Uchar.t * line_break_class) list
*)
val append_break_opportunity : Uchar.t list -> (Uchar.t * line_break_class * break_opportunity ref) list
