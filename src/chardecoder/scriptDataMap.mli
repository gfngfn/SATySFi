
open MyUtil
open CharBasis

type t

val empty : t

val make_from_file : script:abs_path -> east_asian_width:abs_path -> t

val find : Uchar.t -> t -> script option
