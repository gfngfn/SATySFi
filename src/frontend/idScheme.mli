
module type S = sig
  type t

  val show : int -> t -> string
end

module Make : functor(X : S) -> sig
  type t

  val initialize : unit -> unit

  val generate : X.t -> t

  val get_supplement : t -> X.t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val show : t -> string
end
