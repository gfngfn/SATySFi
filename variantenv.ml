open Types
open Display

type t = (variant_type_name list) * ((constructor_name * variant_type_name * type_struct) list)


(* t *)
let empty = ([], [])

(* t -> constructor_name -> variant_type_name -> type_struct -> t *)
let rec add varntenv constrnm varntnm tystr =
  let (defedtylst, varntenvmain) = varntenv in
    (defedtylst, add_main varntenvmain constrnm varntnm tystr)

and add_main varntenvmain constrnm varntnm tystr =
  match varntenvmain with
  | []                -> [(constrnm, varntnm, tystr)]
  | (c, v, t) :: tail ->
      if c = constrnm then
        (constrnm, varntnm, tystr) :: tail
      else
        (c, v, t) :: (add_main tail constrnm varntnm tystr)


(* t -> type_struct -> unit *)
let rec check_type_defined varntenv tystr =
	let (defedtylst, varntenvmain) = varntenv in
  	match tystr with
  	| FuncType(_, tydom, tycod) -> ( check_type_defined varntenv tydom ; check_type_defined varntenv tycod )
  	| ListType(_, tycont)       -> check_type_defined varntenv tycont
  	| RefType(_, tycont)        -> check_type_defined varntenv tycont
  	| ProductType(_, tylist)    -> check_type_list_defined varntenv tylist
  	| VariantType(rng, varntnm) ->
  	    if is_in_list defedtylst varntnm then () else
  	      raise (TypeCheckError("at " ^ (describe_position rng) ^ ":\n    " ^ "undefined type '" ^ varntnm ^ "'"))
  	| _                         -> ()

and check_type_list_defined varntenv tylist =
  match tylist with
  | []         -> ()
  | ty :: tail -> ( check_type_defined varntenv ty ; check_type_list_defined varntenv tail )

and is_in_list lst elm =
  match lst with
  | []           -> false
  | head :: tail -> if head = elm then true else is_in_list tail elm


(* t -> variant_type_name -> untyped_abstract_tree -> t *)
let rec add_cons varntenv varntnm utast =
	let (defedtypelist, varntenvmain) = varntenv in
	let varntenv_new = (varntnm :: defedtypelist, varntenvmain) in
	  add_cons_main varntenv_new varntnm utast

and add_cons_main varntenv varntnm (rng, utastcons) =
  match utastcons with
  | UTEndOfVariant                           -> varntenv
  | UTVariantCons(constrnm, tystr, tailcons) ->
      ( check_type_defined varntenv tystr ;
        add_cons_main (add varntenv constrnm varntnm tystr) varntnm tailcons
      )
  | _ -> raise (TypeCheckError("this cannot happen: illegal variant cons"))


(* t -> constructor_name -> (variant_type_name * type_struct) *)
let rec find varntenv constrnm =
	let (_, varntenvmain) = varntenv in find_main varntenvmain constrnm

and find_main varntenvmain constrnm =
    match varntenvmain with
    | []                -> raise Not_found
    | (c, v, t) :: tail -> if c = constrnm then (v, t) else find_main tail constrnm

