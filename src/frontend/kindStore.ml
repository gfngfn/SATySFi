
open Types

type free_id_entry = {
  level           : Level.t;
  quantifiability : quantifiability;
  mono_kind       : kind;
}

type bound_id_entry = {
  poly_kind : kind;
}


let free_id_table : free_id_entry FreeIDHashTable.t =
  FreeIDHashTable.create 1024


let bound_id_table : bound_id_entry BoundIDHashTable.t =
  BoundIDHashTable.create 1024


let set_free_id (fid : FreeID.t) (fentry : free_id_entry) =
  FreeIDHashTable.replace free_id_table fid fentry


let get_free_id (fid : FreeID.t) =
  match FreeIDHashTable.find_opt free_id_table fid with
  | None         -> assert false
  | Some(fentry) -> fentry


let is_quantifiable (fid : FreeID.t) =
  let fentry = get_free_id fid in
  match fentry.quantifiability with
  | Quantifiable   -> true
  | Unquantifiable -> false


let set_bound_id (bid : BoundID.t) (bentry : bound_id_entry) =
  BoundIDHashTable.replace bound_id_table bid bentry


let get_bound_id (bid : BoundID.t) =
  match BoundIDHashTable.find_opt bound_id_table bid with
  | None         -> assert false
  | Some(bentry) -> bentry


let get_free_row_id (orv : FreeRowID.t) : LabelSet.t =
  failwith "TODO: get_free_row_id"


let register_free_row_id (orv : FreeRowID.t) (labset : LabelSet.t) : unit =
  failwith "TODO: register_free_row_id"
