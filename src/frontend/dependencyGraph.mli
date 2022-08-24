
open Types


module type ElementType = sig
  type t

  val compare : t -> t -> int
end


(** [Make(Element)] returns a module for directed graphs
    that do not have multiple edges but possibly have loops. *)
module Make (Element : ElementType) : sig
  (** The type for keys standing for vertices. *)
  type element = Element.t

  (** The module for “vertex tokens.” *)
  module Vertex : sig
    type t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val hash : t -> int
  end

  (** The type for graphs. *)
  type 'a t

  (** The empty graph. *)
  val empty : 'a t

  (** [add_vertex elem data g] adds to the graph [g] a vertex associated with [(elem, data)].
      Returns [(g', vertex)] where [g'] is the updated graph
      and [vertex] is the vertex token generated for [elem]. *)
  val add_vertex : element -> 'a -> 'a t -> 'a t * Vertex.t

  (** [get_vertex elem g] returns:
      {ul
        {- [Some vertex] if [g] has [elem] as its vertex and the corresponding token is [vertex],}
        {- or returns [None] otherwise.}} *)
  val get_vertex : element -> 'a t -> Vertex.t option

  (** [add_edge ~from:v1 ~to_:v2 g] adds to the graph [g] an edge from [v1] to [v2]. *)
  val add_edge : from:Vertex.t -> to_:Vertex.t -> 'a t -> 'a t

  (** [topological_sort g] performs a topological sort on vertices and returns:
      {ul
        {- [Error cycle] if [g] has a cycle or a loop, or}
        {- [Ok sorted_vertices] if the sorting succeeds.}} *)
  val topological_sort : 'a t -> ((element * 'a) list, (element * 'a) cycle) result
end
