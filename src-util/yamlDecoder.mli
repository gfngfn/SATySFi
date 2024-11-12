
type context_element =
  | Field of string
  | Index of int
[@@deriving show]

type context =
  context_element list
[@@deriving show]

val show_yaml_context : context -> string

module type ErrorType = sig
  type t

  val parse_error : string -> t

  val field_not_found : context -> string -> t

  val not_a_float : context -> t

  val not_a_string : context -> t

  val not_a_bool : context -> t

  val not_an_array : context -> t

  val not_an_object : context -> t

  val branch_not_found : context -> string list -> string list -> t

  val more_than_one_branch_found : context -> string list -> string list -> t
end

module Make (Err : ErrorType) : sig
  type 'a t

  val run : 'a t -> string -> ('a, Err.t) result

  val succeed : 'a -> 'a t

  val failure : (context -> Err.t) -> 'a t

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

  val branch : ('a branch) list -> 'a t

  val ( ==> ) : string -> 'a t -> 'a branch

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a1 -> 'a2 -> 'b) -> 'a1 t -> 'a2 t -> 'b t

  val map3 : ('a1 -> 'a2 -> 'a3 -> 'b) -> 'a1 t -> 'a2 t -> 'a3 t -> 'b t

  val foldM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
end
