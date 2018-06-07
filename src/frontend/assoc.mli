
type 'a t

type key = string

val empty : 'v t

val add : 'v t -> key -> 'v -> 'v t

val find_opt : 'v t -> key -> 'v option

val to_list : 'v t -> (key * 'v) list

val of_list : (key * 'v) list -> 'v t

val map_value : ('v -> 'w) -> 'v t -> 'w t

val iter_value : ('v -> unit) -> 'v t -> unit

val iter : (key -> 'v -> unit) -> 'v t -> unit

val fold_value : ('a -> 'v -> 'a) -> 'a -> 'v t -> 'a

val to_value_list : 'v t -> 'v list

val fold : ('a -> key -> 'v -> 'a) -> 'a -> 'v t -> 'a

val mem : key -> 'v t -> bool

val domain_included : 'v t -> 'w t -> bool

val domain_same : 'v t -> 'w t -> bool

val intersection : 'v t -> 'v t -> ('v * 'v) list

val combine_value : 'v t -> 'w t -> ('v * 'w) list

val intersection : 'v t -> 'v t -> ('v * 'v) list

val union : 'v t -> 'v t -> 'v t

val cardinal : 'v t -> int

