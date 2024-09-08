
val is_uppercased_identifier : string -> bool

val is_lowercased_identifier : string -> bool

val parse_long_command : prefix:string -> string -> (string list * string) option

val parse_long_identifier : string -> (string list * string) option
