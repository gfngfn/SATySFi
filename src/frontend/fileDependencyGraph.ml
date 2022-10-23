
open MyUtil
open Types


module Impl = DependencyGraph.Make(AbsPath)

type vertex = Impl.Vertex.t

type t = untyped_library_file Impl.t


let empty = Impl.empty


let add_vertex (abspath : abs_path) (data : untyped_library_file) (graph : t) : (t * vertex, untyped_library_file * vertex) result =
  Impl.add_vertex abspath data graph


let get_vertex (abspath : abs_path) (graph : t) : vertex option =
  Impl.get_vertex abspath graph


let add_edge ~(from : vertex) ~(to_ : vertex) (graph : t) : t =
  Impl.add_edge ~from ~to_ graph


let topological_sort (graph : t) : ((abs_path * untyped_library_file) list, (abs_path * untyped_library_file) cycle) result =
  Impl.topological_sort graph
