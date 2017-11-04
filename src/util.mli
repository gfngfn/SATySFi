
val list_some : ('a option) list -> 'a list

val option_map : ('a -> 'b) -> 'a option -> 'b option

val ( += ) : int ref -> int -> unit

val ( @|> ) : 'a -> ('a -> 'b) -> 'b
