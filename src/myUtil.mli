
exception RemainsToBeImplemented of string

type abs_path

type lib_path

val remains_to_be_implemented : string -> 'a

val uchar_of_char : char -> Uchar.t

val ascii_capital_of_index : int -> Uchar.t

val ascii_small_of_index : int -> Uchar.t

val ascii_digit_of_index : int -> Uchar.t

val string_of_uchlst : Uchar.t list -> string

val range : int -> int -> int list

val list_make : int -> 'a -> 'a list

val list_fold_left_index : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val list_some : ('a option) list -> 'a list

val list_fold_adjacent : ('a -> 'b -> 'b option -> 'b option -> 'a) -> 'a -> 'b list -> 'a

val pickup : 'a list -> ('a -> bool) -> 'b -> ('a, 'b) result

module OptionMonad : sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
  val return : 'a -> 'a option
end

module ResultMonad : sig
  val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val return : 'a -> ('a, 'e) result
  val err : 'e -> ('a, 'e) result
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val foldM : ('a -> 'b -> ('a, 'e) result) -> 'a -> 'b list -> ('a, 'e) result
  val mapM : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
  val optionM : ('a -> ('b, 'e) result) -> 'a option -> ('b option, 'e) result
end

module EscapeMonad : sig
  val ( >>= ) : [< `Continue of 'a | `Escape of 'b ] -> ('a -> ([> `Escape of 'b ] as 'c)) -> 'c
  val continue : 'a -> [> `Continue of 'a ]
  val escape : 'a -> [> `Escape of 'a ]
  val force : [< `Escape of 'a ] -> 'a
end

val ( += ) : int ref -> int -> unit

val ( @|> ) : 'a -> ('a -> 'b) -> 'b

val first_some : ('a -> 'b option) -> 'a list -> 'b option

val open_in_abs : abs_path -> in_channel

val open_in_bin_abs : abs_path -> in_channel

val open_out_abs : abs_path -> out_channel

val dirname_abs : abs_path -> string

val basename_abs : abs_path -> string

val string_of_file : abs_path -> (string, string) result

val make_abs_path : string -> abs_path

val make_lib_path : string -> lib_path

val get_abs_path_string : abs_path -> string

val get_lib_path_string : lib_path -> string

val get_abs_path_extension : abs_path -> string
