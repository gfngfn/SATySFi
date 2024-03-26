val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val return : 'a -> 'a option
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val foldM : ('a -> 'b -> 'a option) -> 'a -> 'b list -> 'a option
val mapM : ('a -> 'b option) -> 'a list -> ('b list) option
