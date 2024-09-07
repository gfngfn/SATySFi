
type abs_path = AbsPath.t
[@@deriving show]

val string_of_uchar_list : Uchar.t list -> string

val range : int -> int -> int list

val list_fold_adjacent : ('a -> 'b -> 'b option -> 'b option -> 'a) -> 'a -> 'b list -> 'a

val ( @|> ) : 'a -> ('a -> 'b) -> 'b

(* TODO: remove the following (due to migration to `AbsPath`): *)
val make_abs_path : string -> abs_path
val get_abs_path_string : abs_path -> string
val make_absolute_if_relative : origin:abs_path -> string -> abs_path
val append_to_abs_directory : abs_path -> string -> abs_path
val dirname : abs_path -> abs_path
val basename : abs_path -> string

val encode_yaml : Yaml.value -> string

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t
[@@deriving show]

val map_cycle : ('a -> 'b) -> 'a cycle -> 'b cycle
