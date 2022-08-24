
open Types


module type ElementType = sig
  type t

  val compare : t -> t -> int
end


module Make (Element : ElementType) : sig
  type element = Element.t

  type vertex

  type 'a t

  val empty : 'a t

  val add_vertex : element -> 'a -> 'a t -> 'a t * vertex

  val get_vertex : element -> 'a t -> vertex option

  val add_edge : from:vertex -> to_:vertex -> 'a t -> 'a t

  val topological_sort : 'a t -> ((element * 'a) list, (element * 'a) cycle) result
end
