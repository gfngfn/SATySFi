
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

val mem : ?eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> bool

val domain_included : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool

val domain_same : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool

val combine_value : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'c) t -> ('b * 'c) list

val intersection : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> ('b * 'b) list

val union : ?eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
