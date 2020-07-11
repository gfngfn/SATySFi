
open Types

type t

val empty : t

val add_vertex : TypeID.Synonym.t -> type_name ranged -> t -> t

val add_edge : TypeID.Synonym.t -> TypeID.Synonym.t -> t -> t

val has_cycle : t -> bool
