
open Types

type data = {
  position        : Range.t;
  type_variables  : (type_variable_name ranged) list;
  definition_body : manual_type;
}

type vertex

type t

val empty : t

val add_vertex : type_name -> data -> t -> t * vertex

val get_vertex : type_name -> t -> vertex option

val add_edge : from:vertex -> to_:vertex -> t -> t

val topological_sort : t -> ((type_name * data) list, (type_name * data) cycle) result
