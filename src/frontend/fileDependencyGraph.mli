
open MyUtil
open Types

type vertex

type t

val empty : t

val add_vertex : abs_path -> file_info -> t -> (t * vertex) option

val get_vertex : abs_path -> t -> vertex option

val add_edge : from:vertex -> to_:vertex -> t -> t

val topological_sort : t -> ((abs_path * file_info) list, (abs_path * file_info) cycle) result
