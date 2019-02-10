
exception RemainsToBeImplemented of string

type file_path = string

type abs_path

type lib_path

val remains_to_be_implemented : string -> 'a

val uchar_of_char : char -> Uchar.t

val ascii_capital_of_index : int -> Uchar.t

val ascii_small_of_index : int -> Uchar.t

val string_of_uchlst : Uchar.t list -> string

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
