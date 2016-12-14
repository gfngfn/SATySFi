
type ('a, 'b) t


val empty : ('a, 'b) t

val add : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

val find : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> 'a -> 'b

val to_list : ('a, 'b) t -> ('a * 'b) list

val of_list : ?eq:('a -> 'a -> bool) -> ('a * 'b) list -> ('a, 'b) t

val map_value : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold_value : ('c -> 'b -> 'c) -> 'c -> ('a, 'b) t -> 'c

val to_value_list : ('a, 'b) t -> 'b list

val fold : ('c -> ('a * 'b) -> 'c) -> 'c -> ('a, 'b) t -> 'c
