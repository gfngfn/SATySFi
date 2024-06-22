
open SyntaxBase
open Types

type t

val empty : t

val add_free_id : FreeID.t -> t -> t * string

val add_free_row_id : FreeRowID.t -> LabelSet.t -> t -> t * string

val add_bound_id : BoundID.t -> t -> t

val add_bound_row_id : BoundRowID.t -> LabelSet.t -> t -> t

val find_free_id : FreeID.t -> t -> string

val find_free_row_id : FreeRowID.t -> t -> string

val find_bound_id : BoundID.t -> t -> string

val find_bound_row_id : BoundRowID.t -> t -> string

val make_free_id_hash_set : t -> unit FreeIDHashTable.t

val make_free_row_id_hash_set : t -> LabelSet.t FreeRowIDHashTable.t

val make_bound_id_hash_set : t -> unit BoundIDHashTable.t

val make_bound_row_id_hash_set : t -> LabelSet.t BoundRowIDHashTable.t

val fold_free_id : (FreeID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a

val fold_free_row_id : (FreeRowID.t -> string * LabelSet.t -> 'a -> 'a) -> 'a -> t -> 'a

val fold_bound_id : (BoundID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a

val fold_bound_row_id : (BoundRowID.t -> string * LabelSet.t -> 'a -> 'a) -> 'a -> t -> 'a
