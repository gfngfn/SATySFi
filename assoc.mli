
type ('a, 'b) t


val empty : ('a, 'b) t

val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

val find : ('a, 'b) t -> 'a -> 'b

val map_value : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold_value : ('c -> 'b -> 'c) -> 'c -> ('a, 'b) t -> 'c

val to_value_list : ('a, 'b) t -> 'b list
