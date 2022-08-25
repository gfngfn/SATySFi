
open MyUtil
open Types

module AbsPath = struct
  type t = abs_path

  let compare ap1 ap2 = String.compare (get_abs_path_string ap1) (get_abs_path_string ap2)
end

module Impl = DependencyGraph.Make(AbsPath)

type vertex = Impl.Vertex.t

type t = file_info Impl.t


let empty = Impl.empty


let add_vertex (abspath : abs_path) (data : file_info) (graph : t) : (t * vertex, file_info * vertex) result =
  Impl.add_vertex abspath data graph


let get_vertex (abspath : abs_path) (graph : t) : vertex option =
  Impl.get_vertex abspath graph


let add_edge ~(from : vertex) ~(to_ : vertex) (graph : t) : t =
  Impl.add_edge ~from ~to_ graph


let topological_sort (graph : t) : ((abs_path * file_info) list, (abs_path * file_info) cycle) result =
  Impl.topological_sort graph
