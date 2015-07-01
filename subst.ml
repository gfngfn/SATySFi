open Types

type t = (type_variable_id * type_struct) list

(* t -> type_variable_id -> type_struct -> t *)
let rec add_sub asclst key value =
	match asclst with
	| [] -> [(key, value)]
	| (k, v) :: tail ->
	    if k == key then (key, value) :: tail else (k, v) :: (add_sub tail key value)

(* ref t -> type_variable_id -> type_struct -> unit *)
let add rfasclst key value =
	rfasclst := add_sub !rfasclst key value
