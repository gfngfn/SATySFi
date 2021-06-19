
open Types

type data = {
  position        : Range.t;
  type_variables  : (type_variable_name ranged * poly_kind) list;
  definition_body : manual_type;
}

type t

val empty : t

val add_vertex : type_name -> data -> t -> t

val add_edge : type_name -> type_name -> t -> t

val topological_sort : t -> ((type_name * data) list, (type_name * data) cycle) result
