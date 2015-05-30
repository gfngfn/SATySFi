(* signature for Stacklist *)
  open Types

  type 'a t

  val empty : 'a t
  val pop : ('a t ref) -> 'a
  val delete_top : ('a t ref) -> unit
  val top : ('a t ref) -> 'a
  val push : ('a t ref) -> 'a -> unit
  val to_list : ('a t) -> ('a list)
