
exception RemainsToBeImplemented of string

type abs_path = AbsPath.t
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

val make_absolute_if_relative : origin:string -> string -> abs_path

val append_to_abs_directory : abs_path -> string -> abs_path

val dirname : abs_path -> abs_path

val basename : abs_path -> string

val readdir : abs_path -> (string list, string) result

val is_directory : abs_path -> bool

val file_exists : abs_path -> bool

val encode_yaml : Yaml.value -> string

val read_file : abs_path -> (string, string) result

val write_file : abs_path -> string -> (unit, string) result

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t
[@@deriving show]

val map_cycle : ('a -> 'b) -> 'a cycle -> 'b cycle
