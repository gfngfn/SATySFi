
open MyUtil
open Types

type vertex

type t

val empty : t

val add_vertex : abs_path -> untyped_source_file -> t -> (t * vertex, untyped_source_file * vertex) result

val get_vertex : abs_path -> t -> vertex option

val add_edge : from:vertex -> to_:vertex -> t -> t

val topological_sort : t -> ((abs_path * untyped_source_file) list, (abs_path * untyped_source_file) cycle) result
