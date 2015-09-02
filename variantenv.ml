open Types

type t = (constructor_name * variant_type_name * type_struct) list


(* t *)
let empty = []

(* t -> constructor_name -> variant_type_name -> type_struct -> t *)
let rec add varntenv constrnm varntnm tystr =
	match varntenv with
	| []                -> [(constrnm, varntnm, tystr)]
	| (c, v, t) :: tail ->
	    if c = constrnm then
	      (constrnm, varntnm, tystr) :: tail
	    else
	      (c, v, t) :: (add tail constrnm varntnm tystr)


(* t -> variant_type_name -> untyped_abstract_tree -> t *)
let rec add_cons varntenv varntnm (rng, utastcons) =
	match utastcons with
	| UTEndOfVariant                           -> varntenv
	| UTVariantCons(constrnm, tystr, tailcons) ->
	    add_cons (add varntenv constrnm varntnm tystr) varntnm tailcons
	| _ -> raise (TypeCheckError("this cannot happen: illegal variant cons"))


(* t -> constructor_name -> (variant_type_name * type_struct) *)
let rec find varntenv constrnm =
	match varntenv with
	| []                -> raise Not_found
	| (c, v, t) :: tail -> if c = constrnm then (v, t) else find tail constrnm
