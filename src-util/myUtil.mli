
type abs_path = AbsPath.t
[@@deriving show]

val string_of_uchar_list : Uchar.t list -> string

val range : int -> int -> int list

val list_fold_adjacent : ('a -> 'b -> 'b option -> 'b option -> 'a) -> 'a -> 'b list -> 'a

val ( @|> ) : 'a -> ('a -> 'b) -> 'b

val encode_yaml : Yaml.value -> string

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t
[@@deriving show]

val map_cycle : ('a -> 'b) -> 'a cycle -> 'b cycle
