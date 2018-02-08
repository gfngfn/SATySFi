
open CharBasis

val set_from_file : string -> unit

val find : Uchar.t -> line_break_class

val append_break_opportunity : Uchar.t list -> line_break_element list

(*
val print_trilist : line_break_element list -> unit
*)
