
type 'a t

val make : 'a -> 'a -> 'a list -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val mapM : ('a -> ('b, 'e) result) -> 'a t -> ('b t, 'e) result

val to_list : 'a t -> 'a list

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
