
open MyUtil
open Types

type vertex

type t

val empty : t

val add_vertex : abs_path -> untyped_library_file -> t -> (t * vertex, untyped_library_file * vertex) result

val get_vertex : abs_path -> t -> vertex option

val add_edge : from:vertex -> to_:vertex -> t -> t

val topological_sort : t -> ((abs_path * untyped_library_file) list, (abs_path * untyped_library_file) cycle) result
