
type 'a t

val empty : 'a t

val extend : 'a t -> 'a -> 'a t

val append : 'a t -> 'a list -> 'a t

val to_list : 'a t -> 'a list

val chop_last : 'a t -> ('a t * 'a) option

val map : ('a -> 'b) -> 'a t -> 'b t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
