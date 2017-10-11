
open CharBasis

val set_from_file : string -> unit

val find_opt : Uchar.t -> script

val divide_by_script : Uchar.t list -> (script * Uchar.t list) list
