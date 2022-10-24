
type error

val pp_error : Format.formatter -> error -> unit

type 'a t

val run : 'a t -> string -> ('a, error) result

val succeed : 'a -> 'a t

val failure : string -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val get : string -> 'a t -> 'a t

val get_opt : string -> 'a t -> ('a option) t

val get_or_else : string -> 'a t -> 'a -> 'a t

val number : float t

val string : string t

val bool : bool t

val list : 'a t -> ('a list) t

type 'a branch

val branch : string -> ('a branch) list -> on_error:(string -> string) -> 'a t

val ( ==> ) : string -> 'a t -> 'a branch

val map : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a1 -> 'a2 -> 'b) -> 'a1 t -> 'a2 t -> 'b t

val map3 : ('a1 -> 'a2 -> 'a3 -> 'b) -> 'a1 t -> 'a2 t -> 'a3 t -> 'b t
