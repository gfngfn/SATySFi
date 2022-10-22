
type t

val of_utf8 : string -> t

val of_utf16be : string -> t

val to_utf8 : t -> string

val to_utf16be_hex : t -> string

val to_utf16be : t -> string

val to_uchar_list : t -> Uchar.t list

val of_uchar : Uchar.t -> t

val of_uchar_list : Uchar.t list -> t
