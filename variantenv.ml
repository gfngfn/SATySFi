open Types

type definition_kind = Data | Synonym of type_struct

type defined_type_list = (type_name * definition_kind) list
type constructor_list = (constructor_name * type_name * type_struct) list
type t = defined_type_list * constructor_list


(* t *)
let empty = ([], [])


(* t -> constructor_name -> type_name -> type_struct -> t *)
let rec add varntenv constrnm varntnm tystr =
  let (defedtylst, varntenvmain) = varntenv in
    (defedtylst, add_main varntenvmain constrnm varntnm tystr)

(* constructor_list -> constructor_name -> type_name -> type_struct *)
and add_main varntenvmain constrnm varntnm tystr =
  match varntenvmain with
  | []                -> [(constrnm, varntnm, tystr)]
  | (c, v, t) :: tail ->
      if c = constrnm then
        (constrnm, varntnm, tystr) :: tail
      else
        (c, v, t) :: (add_main tail constrnm varntnm tystr)


(* defined_type_list -> type_name -> definition_kind *)
let rec find_definition_kind defedtylst tynm =
  match defedtylst with
  | []                            -> raise Not_found
  | (tn, ts) :: tl when tn = tynm -> ts
  | _ :: tl                       -> find_definition_kind tl tynm


(* t -> type_struct -> type_struct *)
let rec check_type_defined varntenv tystr =
	let (defedtylst, varntenvmain) = varntenv in
  	match tystr with
    | VariantType(rng, varntnm) ->
        begin try
          match find_definition_kind defedtylst varntnm with
          | Data           -> VariantType(rng, varntnm)
          | Synonym(tystr) -> TypeSynonym(rng, varntnm, tystr)
        with
        | Not_found ->
            raise (TypeCheckError(
                "at " ^ (Display.describe_position rng) ^ ":\n"
              ^ "    undefined type '" ^ varntnm ^ "'"
            ))
        end
  	| FuncType(rng, tydom, tycod) -> FuncType(rng, check_type_defined varntenv tydom, check_type_defined varntenv tycod)
  	| ListType(rng, tycont)       -> ListType(rng, check_type_defined varntenv tycont)
  	| RefType(rng, tycont)        -> RefType(rng, check_type_defined varntenv tycont)
  	| ProductType(rng, tylist)    -> ProductType(rng, List.map (check_type_defined varntenv) tylist)
  	| other                       -> other


(* t -> type_name -> t *)
let add_variant varntenv tynm =
  let (defedtypelist, varntenvmain) = varntenv in
    ((tynm, Data) :: defedtypelist, varntenvmain)


(* t -> type_name -> type_struct -> t *)
let add_type_synonym varntenv tynm tystr =
  let (defedtypelist, varntenvmain) = varntenv in
  let tystr_new = check_type_defined varntenv tystr in
    ((tynm, Synonym(tystr_new)) :: defedtypelist, varntenvmain)


(* t -> type_name -> untyped_variant_cons -> t *)
let rec add_cons varntenv varntnm utvc =
	  add_cons_main (add_variant varntenv varntnm) varntnm utvc

and add_cons_main varntenv varntnm (rng, utvcmain) =
  match utvcmain with
  | UTEndOfVariant                           -> varntenv
  | UTVariantCons(constrnm, tystr, tailcons) ->
      let tystr_new = check_type_defined varntenv tystr in
        add_cons_main (add varntenv constrnm varntnm tystr_new) varntnm tailcons


(* t -> untyped_mutual_variant_cons -> t *)
let rec add_mutual_cons varntenv mutvarntcons =
  let varntenv_new = add_mutual_variant_type varntenv mutvarntcons in
    match mutvarntcons with
    | UTEndOfMutualVariant                         -> varntenv_new
    | UTMutualVariantCons(varntnm, utvc, tailcons) ->
        add_mutual_cons (add_cons varntenv_new varntnm utvc) tailcons

and add_mutual_variant_type varntenv mutvarntcons =
  match mutvarntcons with
  | UTEndOfMutualVariant                      -> varntenv
  | UTMutualVariantCons(varntnm, _, tailcons) ->
      add_mutual_variant_type (add_variant varntenv varntnm) tailcons


(* t -> constructor_name -> (type_name * type_struct) *)
let rec find varntenv constrnm =
	let (_, varntenvmain) = varntenv in find_main varntenvmain constrnm

and find_main varntenvmain constrnm =
    match varntenvmain with
    | []                -> raise Not_found
    | (c, v, t) :: tail -> if c = constrnm then (v, t) else find_main tail constrnm
