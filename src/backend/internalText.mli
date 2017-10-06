
type t

exception NotEncodableToUTF16BE of t

val of_utf_8 : string -> t

val to_utf8 : t -> string

val to_utf16be_hex : t -> string

val to_uchar_list : t -> Uchar.t list

val of_uchar : Uchar.t -> t
