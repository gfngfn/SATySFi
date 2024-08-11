
open Types

type data = {
  position        : Range.t;
  type_variables  : (type_variable_name ranged) list;
  definition_body : manual_type;
}

module Vertex : sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int
end

module VertexSet : Set.S with type elt = Vertex.t

type t

val empty : t

val add_vertex : type_name -> data -> t -> (t * Vertex.t, data * Vertex.t) result

val get_vertex : type_name -> t -> Vertex.t option

val add_edge : from:Vertex.t -> to_:Vertex.t -> t -> t

val topological_sort : t -> ((type_name * data) list, (type_name * data) cycle) result
