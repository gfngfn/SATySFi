open Types
open Typeenv

type t = (type_variable_id * type_struct) list

let empty = []

(* t -> type_variable_id -> type_struct -> t *)
let add theta key value = (key, value) :: theta

let rec find theta key =
  match theta with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k == key then v else find tail key


let rec eliminate theta key =
	eliminate_sub theta key []

and eliminate_sub theta key constr =
  match theta with
  | [] -> raise Not_found
  | (k, v) :: tail ->
      if k == key then
        constr @ tail
      else
        eliminate_sub tail key ((k, v) :: constr)


let rec overwrite_type_struct tystr key value =
  match tystr with
  | TypeVariable(rng, k)    -> if k = key then value else TypeVariable(rng, k)
  | FuncType(rng, dom, cod) -> FuncType(rng, overwrite_type_struct dom key value, overwrite_type_struct cod key value)
  | ListType(rng, cont)     -> ListType(rng, overwrite_type_struct cont key value)
  | RefType(rng, cont)      -> RefType(rng, overwrite_type_struct cont key value)
  | other                   -> other

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

(* type_variable_id -> type_struct -> (bool * code_range) *)
let rec emerge_in tvid tystr =
  let dummy = (-2049, 0, 0, 0) in
    match tystr with
    | TypeVariable(rng, tvidx) -> (tvidx == tvid, rng)
    | FuncType(_, dom, cod)    ->
        let (bdom, rngdom) = emerge_in tvid dom in
        let (bcod, rngcod) = emerge_in tvid cod in
          if bdom then (bdom, rngdom) else if bcod then (bcod, rngcod) else (false, dummy)
    | ListType(_, cont)        -> emerge_in tvid cont
    | RefType(_, cont)         -> emerge_in tvid cont
    | _                        -> (false, dummy)

let rec overwrite theta key value =
  match theta with
  | []             -> []
  | (k, v) :: tail ->
      if k = key then
        (key, value) :: (overwrite tail key value)
      else (k, (overwrite_type_struct v key value)) :: (overwrite tail key value)

let overwrite_or_add theta key value =
  overwrite (add theta key value) key value


(* Subst.t -> Subst.t *)
let rec fix_subst theta = fix_subst_sub theta theta

and fix_subst_sub rest from =
  match rest with
  | []                    -> ( check_emergence from ; from )
  | (tvid, tystr) :: tail -> fix_subst_sub tail (overwrite from tvid tystr)

and check_emergence theta =
  match theta with
  | []                    -> ()
  | (tvid, tystr) :: tail ->
      let (b, rng) = emerge_in tvid tystr in
        if b then
          let (strty1, strty2) = string_of_type_struct_double (TypeVariable((-259, 0, 0, 0), tvid)) tystr in
          raise (TypeCheckError(error_reporting rng
            ("this expression has types <" ^ strty1 ^ ">\n"
              ^ "    and <" ^ strty2 ^ "> at the same time,\n"
              ^ "    but the former type emerges in the latter one")))
        else
          check_emergence tail


(* Subst.t -> Subst.t -> Subst.t *)
let rec compose theta2 theta1 = fix_subst (compose_prim theta2 theta1)
and compose_prim theta2 theta1 =
  match theta2 with
  | []                     -> theta1
  | (tvid, tystr2) :: tail ->
      ( try
          let tystr1 = find theta1 tvid in
          ( print_for_debug ("$1: '" ^ (string_of_int tvid) ^ " = "
              ^ (string_of_type_struct tystr2) ^ " = " ^ (string_of_type_struct tystr1) ^ "\n") ;
              (tvid, tystr1) :: (compose_prim (eliminate theta1 tvid) (compose_prim tail (unify tystr1 tystr2)))
          )
        with
        | Not_found ->
          ( compose_prim tail (overwrite_or_add theta1 tvid tystr2) )
      )

(* type_struct -> type_struct -> Subst.t *)
and unify tystr1 tystr2 =
  print_for_debug ("  [" ^ (string_of_type_struct_basic tystr1) ^ "] = ["
                     ^ (string_of_type_struct_basic tystr2) ^ "]\n") ; (* for test *)
  match (tystr1, tystr2) with
  | (IntType(_), IntType(_))       -> empty
  | (StringType(_), StringType(_)) -> empty
  | (BoolType(_), BoolType(_))     -> empty
  | (UnitType(_), UnitType(_))     -> empty
  | (TypeEnvironmentType(_, _), TypeEnvironmentType(_, _)) -> empty

  | (FuncType(_, dom1, cod1), FuncType(_, dom2, cod2)) ->
      compose (unify dom1 dom2) (unify cod1 cod2)

  | (ListType(_, cont1), ListType(_, cont2)) -> unify cont1 cont2

  | (RefType(_, cont1), RefType(_, cont2))   -> unify cont1 cont2

  | (TypeVariable(rng1, tvid1), tystr) ->
      ( match tystr with
        | TypeVariable(rng2, tvid2) ->
            if tvid1 == tvid2 then
              empty
            else if tvid1 < tvid2 then
              if is_invalid_range rng2 then
                [(tvid1, TypeVariable(rng1, tvid2))]
              else
                [(tvid1, TypeVariable(rng2, tvid2))]
            else
              if is_invalid_range rng1 then
                [(tvid2, TypeVariable(rng2, tvid1))]
              else
                [(tvid2, TypeVariable(rng1, tvid1))]

        | other ->
            let (b, _) = emerge_in tvid1 tystr in
              if b then
                raise (TypeCheckError(error_reporting rng1
                  ("this expression has type <" ^ (string_of_type_struct (TypeVariable(rng1, tvid1))) ^ ">\n"
                    ^ "    and <" ^ (string_of_type_struct tystr) ^ "> at the same time,\n"
                    ^ "    but the former type emerges in the latter one")))
              else
                [(tvid1, overwrite_range_of_type tystr rng1)]
      )
  | (tystr, TypeVariable(rng, tvid)) -> unify (TypeVariable(rng, tvid)) tystr

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
              ^ "    but they should correspond with each other"))
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


(* for test *)
let rec string_of_subst theta =
      " +-------------------------------\n"
    ^ (string_of_subst_sub theta)
    ^ " +-------------------------------\n"
and string_of_subst_sub theta =
  match theta with
  | []                    -> ""
  | (tvid, tystr) :: tail ->
      " | '" ^ (string_of_int tvid) ^ " := " ^ (string_of_type_struct_basic tystr) ^ "\n"
        ^ (string_of_subst_sub tail)