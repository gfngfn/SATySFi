val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val return : 'a -> ('a, 'e) result
val err : 'e -> ('a, 'e) result
val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val foldM : ('a -> 'b -> ('a, 'e) result) -> 'a -> 'b list -> ('a, 'e) result
val mapM : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
val optionM : ('a -> ('b, 'e) result) -> 'a option -> ('b option, 'e) result
