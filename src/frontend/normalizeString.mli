type t

val of_utf8_nfd : string -> t

val of_utf8_nfc : string -> t

val of_utf16be_nfd : string -> t

val of_utf16be_nfc : string -> t

val to_utf8 : t -> string

val to_utf16be_hex : t -> string

val to_utf16be : t -> string
