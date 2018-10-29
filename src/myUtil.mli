
val uchar_of_char : char -> Uchar.t

val ascii_capital_of_index : int -> Uchar.t

val ascii_small_of_index : int -> Uchar.t

val range : int -> int -> int list

val list_make : int -> 'a -> 'a list

val list_fold_left_index : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val list_some : ('a option) list -> 'a list

val list_fold_adjacent : ('a -> 'b -> 'b option -> 'b option -> 'a) -> 'a -> 'b list -> 'a

val option_map : ('a -> 'b) -> 'a option -> 'b option

val pickup : 'a list -> ('a -> bool) -> 'b -> ('a, 'b) result

module OptionMonad : sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
  val return : 'a -> 'a option
end

module ResultMonad : sig
  val ( >>= ) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result
  val return : 'a -> ('a, 'b) result
  val err : 'b -> ('a, 'b) result
end


val ( += ) : int ref -> int -> unit

val ( @|> ) : 'a -> ('a -> 'b) -> 'b

val first_some : ('a -> 'b option) -> 'a list -> 'b option
