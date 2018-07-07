
open CharBasis

val set_from_file : string -> unit

val find : Uchar.t -> line_break_class

val append_break_opportunity : Uchar.t list -> break_opportunity -> break_opportunity * line_break_element list
