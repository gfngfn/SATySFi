
open MyUtil
open CharBasis

type t

val empty : t

val make_from_file : abs_path -> t

val find : Uchar.t -> t -> line_break_class

val append_break_opportunity : t -> Uchar.t list -> break_opportunity -> break_opportunity * line_break_element list
