open Types
open Typeenv

type t = (type_variable_id * type_struct) list

let empty = []

(* t -> type_variable_id -> type_struct -> t *)
let add theta key value = (key, value) :: theta

let rec find theta key =
  match theta with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k = key then v else find tail key

let rec overwrite_type_struct tystr key value =
  match tystr with
  | TypeVariable(rng, k)    -> if k = key then value else TypeVariable(rng, k)
  | FuncType(rng, dom, cod) -> FuncType(rng, overwrite_type_struct dom key value, overwrite_type_struct cod key value)
  | ListType(rng, cont)     -> ListType(rng, overwrite_type_struct cont key value)
  | RefType(rng, cont)      -> RefType(rng, overwrite_type_struct cont key value)
  | other                   -> other

let rec overwrite theta key value =
  match theta with
  | []             -> add theta key value
  | (k, v) :: tail ->
      if k = key then (key, value) :: tail else (k, (overwrite_type_struct v key value)) :: (overwrite tail key value)

let rec compose theta2 theta1 =
  match theta2 with
  | []              -> theta1
  | (k, v) :: tail2 -> compose tail2 (overwrite theta1 k v)

(* Subst.t -> type_struct -> type_struct *)
let rec apply_to_type_struct theta tystr =
  match tystr with
  | FuncType(rng, tydom, tycod) -> FuncType(rng, apply_to_type_struct theta tydom, apply_to_type_struct theta tycod)
  | ListType(rng, tycont)       -> ListType(rng, apply_to_type_struct theta tycont)
  | RefType(rng, tycont)        -> RefType(rng, apply_to_type_struct theta tycont)
  | TypeVariable(rng, tv)       -> ( try find theta tv with Not_found -> TypeVariable(rng, tv) )
  | other                       -> other

(* Subst.t -> type_environment -> type_environment *)
let rec apply_to_type_environment theta tyenv =
  match tyenv with
  | []                     -> tyenv
  | (varnm, tystr) :: tail ->
      (varnm, apply_to_type_struct theta tystr) :: (apply_to_type_environment theta tail)

let rec apply_to_term theta ast = ast (* need writing *)

let rec emerge_in tvid tystr =
  match tystr with
  | TypeVariable(_, tvidx) -> tvidx == tvid
  | FuncType(_, dom, cod)  -> (emerge_in tvid dom) || (emerge_in tvid cod)
  | ListType(_, cont)      -> emerge_in tvid cont
  | RefType(_, cont)       -> emerge_in tvid cont
  | _                      -> false

let rec compose_special theta2 theta1 =
	match theta2 with
	| [] -> theta1
	| (tvid, tystr2) :: tail ->
	    ( try
	    	  let tystr1 = find theta1 tvid in
	    	  ( print_string "$1\n" ;
	    	    compose_special tail (unify_sub tystr1 tystr2)
	    	  )
	      with
	      | Not_found ->
	        ( print_string "$2\n" ; compose_special tail (add theta1 tvid tystr2) )
	    )
and unify_sub tystr1 tystr2 =
  match (tystr1, tystr2) with
  | (IntType(_), IntType(_))       -> empty
  | (StringType(_), StringType(_)) -> empty
  | (BoolType(_), BoolType(_))     -> empty
  | (UnitType(_), UnitType(_))     -> empty
  | (TypeEnvironmentType(_, _), TypeEnvironmentType(_, _)) -> empty

  | (FuncType(_, dom1, cod1), FuncType(_, dom2, cod2)) ->
      compose_special (unify_sub dom1 dom2) (unify_sub cod1 cod2)

  | (ListType(_, cont1), ListType(_, cont2)) -> unify_sub cont1 cont2

  | (RefType(_, cont1), RefType(_, cont2))   -> unify_sub cont1 cont2

  | (TypeVariable(rng1, tvid1), tystr) ->
      ( match tystr with
        | TypeVariable(rng2, tvid2) ->
            if tvid1 == tvid2 then
              empty
            else if tvid1 < tvid2 then
            ( print_string ("*A '" ^ (string_of_int tvid1) ^ " = '" ^ (string_of_int tvid2) ^ "\n") ;
              [(tvid1, TypeVariable(rng2, tvid2))]
            )
            else
            ( print_string ("*B '" ^ (string_of_int tvid2) ^ " = '" ^ (string_of_int tvid1) ^ "\n") ;
              [(tvid2, TypeVariable(rng1, tvid1))]
            )

        | other ->
          ( print_string ("*C '" ^ (string_of_int tvid1) ^ " = " ^ (string_of_type_struct tystr) ^ "\n") ;
            if emerge_in tvid1 tystr then
              raise (TypeCheckError(error_reporting rng1
                ("this expression has type <" ^ (string_of_type_struct (TypeVariable(rng1, tvid1))) ^ ">\n"
                  ^ "    and <" ^ (string_of_type_struct tystr) ^ "> at the same time,\n"
                  ^ "    but the former type is in the latter one")))
            else
              [(tvid1, tystr)]
          )
      )
  | (tystr, TypeVariable(rng, tvid)) -> unify_sub (TypeVariable(rng, tvid)) tystr

  | (tystr1, tystr2) ->
      let (sttln1, sttpos1, endln1, endpos1) = get_range_from_type tystr1 in
      let (sttln2, sttpos2, endln2, endpos2) = get_range_from_type tystr2 in
      let strty1 = string_of_type_struct tystr1 in
      let strty2 = string_of_type_struct tystr2 in
      let msg1 = describe_position (sttln1, sttpos1, endln1, endpos1) in
      let msg2 = describe_position (sttln2, sttpos2, endln2, endpos2) in
        if (sttln1 > 0) then
          if (sttln2 > 0) then
            raise (TypeCheckError("at " ^ msg1 ^ " and " ^ msg2 ^ ":\n"
              ^ "    these expressions have type <" ^ strty1 ^ "> and <" ^ strty2 ^ "> respectively,\n"
              ^ "    but they should be the same"))
          else
            raise (TypeCheckError(" at " ^ msg1 ^ ":\n"
              ^ "    this expression has type <" ^ strty1 ^ ">,\n"
              ^ "    but is expected of type <" ^ strty2 ^ ">"))
        else
          if (sttln2 > 0) then
            raise (TypeCheckError("at " ^ msg2 ^ ":\n"
              ^ "    this expression has type <" ^ strty2 ^ ">,\n"
              ^ "    but is expected of type <" ^ strty1 ^ ">"))
          else
            raise (TypeCheckError("something is wrong; (" ^ (string_of_int sttln1) ^ ", " ^ (string_of_int sttln2) ^ ")"))

let rec fix_unification theta thetaconstr =
	match theta with
	| [] -> thetaconstr
	| (tvid1, TypeVariable(rng2, tvid2)) :: tail ->
	  ( print_string ("*D '" ^ (string_of_int tvid1) ^ " = '" ^ (string_of_int tvid2) ^ "\n") ;
	    let tystr_new = ( try find theta tvid2 with Not_found -> TypeVariable(rng2, tvid2) ) in
	      fix_unification tail ((tvid1, tystr_new) :: thetaconstr)
	  )

	| (tvid1, tystr) :: tail -> fix_unification tail ((tvid1, tystr) :: thetaconstr)


let unify tystr1 tystr2 = fix_unification (unify_sub tystr1 tystr2) empty

