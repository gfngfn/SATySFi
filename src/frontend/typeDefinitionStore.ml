
open Types


module SynonymHashTable = Hashtbl.Make(TypeID.Synonym)

module VariantHashTable = Hashtbl.Make(TypeID.Variant)


let synonym_hash_table = SynonymHashTable.create 128

let variant_hash_table = VariantHashTable.create 128


let initialize () =
  SynonymHashTable.clear synonym_hash_table;
  VariantHashTable.clear variant_hash_table


let add_synonym_type (sid : TypeID.Synonym.t) (bids : BoundID.t list) (pty : poly_type) =
  SynonymHashTable.add synonym_hash_table sid (bids, pty)


let find_synonym_type (sid : TypeID.Synonym.t) : BoundID.t list * poly_type =
  match SynonymHashTable.find_opt synonym_hash_table sid with
  | None       -> assert false
  | Some(pair) -> pair


let add_variant_type (vid : TypeID.Variant.t) (bids : BoundID.t list) (ctormap : constructor_branch_map) =
  VariantHashTable.add variant_hash_table vid (bids, ctormap)


let find_variant_type (vid : TypeID.Variant.t) : BoundID.t list * constructor_branch_map =
  match VariantHashTable.find_opt variant_hash_table vid with
  | None       -> assert false
  | Some(pair) -> pair
