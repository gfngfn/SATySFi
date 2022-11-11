
exception RemainsToBeImplemented of string

type abs_path
[@@deriving show]

type lib_path

val remains_to_be_implemented : string -> 'a

val string_of_uchar_list : Uchar.t list -> string

val range : int -> int -> int list

val list_fold_adjacent : ('a -> 'b -> 'b option -> 'b option -> 'a) -> 'a -> 'b list -> 'a

val ( @|> ) : 'a -> ('a -> 'b) -> 'b

val open_in_abs : abs_path -> in_channel

val basename_abs : abs_path -> string

val make_abs_path : string -> abs_path

val make_lib_path : string -> lib_path

val get_abs_path_string : abs_path -> string

val get_lib_path_string : lib_path -> string

module AbsPath : sig
  type t = abs_path

  val compare : t -> t -> int
end

val read_file : abs_path -> (string, string) result
