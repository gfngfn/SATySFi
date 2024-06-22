
open Types


module type ElementType = sig
  type t

  val compare : t -> t -> int
end


(** [Make(Element)] returns a module [G] for directed graphs
    that do not have multiple edges but possibly have loops.
    Each vertex of graphs of type ['a G.t] has its distinct key of type [Element.t] and
    an additional value [data] of type ['a].
    This module is used for dependency resolution on directed acyclic graphs. *)
module Make (Element : ElementType) : sig
  (** The type for keys each of which uniquely determines a vertice. *)
  type element = Element.t

  (** The module for “vertex tokens.” *)
  module Vertex : sig
    type t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val hash : t -> int

    val label : t -> element
  end

  (** Equals [Set.Make(Vertex)]. *)
  module VertexSet : Set.S with type elt = Vertex.t

  (** The type for graphs. *)
  type 'a t

  (** The empty graph. *)
  val empty : 'a t

  (** [add_vertex elem data g] adds to the graph [g] a fresh vertex [v] associated with
      the key [elem] and the additional value [data], and
      returns [Ok (g', v)] where [g'] is the updated graph if [elem] is not present in [g];
      or returns [Error (data, v)] if [g] already has a vertex [v] associated with the key [elem]. *)
  val add_vertex : element -> 'a -> 'a t -> ('a t * Vertex.t, 'a * Vertex.t) result

  (** [get_vertex elem g] returns:
      {ul
        {- [Some v] if [g] has [elem] as its corresponding vertex is [v],}
        {- or returns [None] otherwise.}} *)
  val get_vertex : element -> 'a t -> Vertex.t option

  (** [add_edge ~from:v1 ~to_:v2 g] adds to the graph [g] an edge from [v1] to [v2]. *)
  val add_edge : from:Vertex.t -> to_:Vertex.t -> 'a t -> 'a t

  (** [topological_sort g] performs a topological sort on vertices and returns:
      {ul
        {- [Error cycle] if [g] has a cycle or a loop, or}
        {- [Ok sorted_vertices] if the sorting succeeds.}} *)
  val topological_sort : 'a t -> ((element * 'a) list, (element * 'a) cycle) result

  (** [reachability_closure g vertices] computes the set of vertices reachable
      from at least one vertex contained in [vertices]. *)
  val reachability_closure : 'a t -> VertexSet.t -> VertexSet.t
end
