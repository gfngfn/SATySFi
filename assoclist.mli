(* signature for Assoclist *)
  open Types

  type ('a, 'b) t

  val empty : ('a, 'b) t
  val add : 'a -> 'b -> (('a, 'b) t) -> (('a, 'b) t)
  val add_list : ('a list) -> ('b list) -> (('a, 'b) t) -> (('a, 'b) t)
  val get_value : (('a, 'b) t) -> 'a -> 'b
