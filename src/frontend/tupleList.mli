
type 'a t

val make : 'a -> 'a -> 'a list -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val to_list : 'a t -> 'a list

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
