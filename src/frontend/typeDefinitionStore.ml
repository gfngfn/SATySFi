
open Types


module SynonymHashTable = Hashtbl.Make(TypeID.Synonym)


let synonym_hash_table = SynonymHashTable.create 128


let initialize () =
  SynonymHashTable.clear synonym_hash_table


let add_synonym_type (sid : TypeID.Synonym.t) (bids : BoundID.t list) (pty : poly_type) =
  SynonymHashTable.add synonym_hash_table sid (bids, pty)
