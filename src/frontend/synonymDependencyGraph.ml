
open Types

module Impl = DependencyGraph.Make(String)

type data = {
  position        : Range.t;
  type_variables  : (type_variable_name ranged) list;
  definition_body : manual_type;
}

type vertex = Impl.vertex

type t = data Impl.t


let empty = Impl.empty


let add_vertex (tynm : type_name) (data : data) (graph : t) : t * vertex =
  Impl.add_vertex tynm data graph


let get_vertex (tynm : type_name) (graph : t) : vertex option =
  Impl.get_vertex tynm graph


let add_edge ~(from : vertex) ~(to_ : vertex) (graph : t) : t =
  Impl.add_edge ~from ~to_ graph


let topological_sort (graph : t) : ((type_name * data) list, (type_name * data) cycle) result =
  Impl.topological_sort graph
