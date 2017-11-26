
val list_some : ('a option) list -> 'a list

val option_map : ('a -> 'b) -> 'a option -> 'b option

val pickup : 'a list -> ('a -> bool) -> 'b -> ('a, 'b) result

val ( += ) : int ref -> int -> unit

val ( @|> ) : 'a -> ('a -> 'b) -> 'b
