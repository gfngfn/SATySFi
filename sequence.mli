(* signature for Sequence *)
  type 'a t

  val empty : 'a t
  val append : 'a t ref -> 'a -> unit
  val omit_last : 'a t ref -> unit
  val get_last : 'a t ref -> 'a
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
