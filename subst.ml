open Types
open Display

exception InclusionError
exception ContradictionError

type t = (type_variable_id * type_struct) list


let empty = []


(* t -> type_variable_id -> type_struct -> t *)
let add theta key value = (key, value) :: theta


(* t -> type_variable_id -> type_struct *)
let rec find theta key =
  match theta with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k == key then v else find tail key


(* t -> type_variable_id -> t *)
let rec eliminate theta key = eliminate_sub [] theta key
and eliminate_sub constr theta key =
  match theta with
  | [] -> raise Not_found
  | (k, v) :: tail ->
      if k == key then
        constr @ tail
      else
        eliminate_sub ((k, v) :: constr) tail key


(* type_struct -> type_variable_id -> type_struct -> type_variable *)
let rec overwrite_type_struct tystr key value =
  match tystr with
  | FuncType(rng, dom, cod) -> FuncType(rng, overwrite_type_struct dom key value, overwrite_type_struct cod key value)
  | ListType(rng, cont)     -> ListType(rng, overwrite_type_struct cont key value)
  | RefType(rng, cont)      -> RefType(rng, overwrite_type_struct cont key value)
  | ProductType(rng, lst)   -> ProductType(rng, List.map (fun ty -> overwrite_type_struct ty key value) lst)
  | TypeVariable(rng, k)    -> if k = key then value else TypeVariable(rng, k)
  | other                   -> other


(* t -> type_struct -> type_struct *)
let rec apply_to_type_struct theta tystr =
  match tystr with
  | FuncType(rng, tydom, tycod) -> FuncType(rng, apply_to_type_struct theta tydom, apply_to_type_struct theta tycod)
  | ListType(rng, tycont)       -> ListType(rng, apply_to_type_struct theta tycont)
  | RefType(rng, tycont)        -> RefType(rng, apply_to_type_struct theta tycont)
  | ProductType(rng, tylist)    -> ProductType(rng, List.map (apply_to_type_struct theta) tylist)
  | TypeVariable(rng, tv)       -> ( try find theta tv with Not_found -> TypeVariable(rng, tv) )
  | other                       -> other


(* t -> type_environment -> type_environment *)
let rec apply_to_type_environment theta tyenv =
  match tyenv with
  | []                     -> tyenv
  | (varnm, tystr) :: tail ->
      (varnm, apply_to_type_struct theta tystr) :: (apply_to_type_environment theta tail)


(* type_variable_id -> type_struct -> (bool * code_range) *)
let rec emerge_in tvid tystr =
  let dummy = (-2049, 0, 0, 0) in
    match tystr with
    | FuncType(_, dom, cod)    ->
        let (bdom, rngdom) = emerge_in tvid dom in
        let (bcod, rngcod) = emerge_in tvid cod in
          if bdom then (bdom, rngdom) else if bcod then (bcod, rngcod) else (false, dummy)
    | ListType(_, cont)        -> emerge_in tvid cont
    | RefType(_, cont)         -> emerge_in tvid cont
    | ProductType(_, lst)      -> emerge_in_list tvid lst
    | TypeVariable(rng, tvidx) -> (tvidx = tvid, rng)
    | _                        -> (false, dummy)
and emerge_in_list tvid tylist =
  let dummy = (-2049, 0, 0, 0) in
    match tylist with
    | []           -> (false, dummy)
    | tyhd :: tytl ->
        let (bhd, rnghd) = emerge_in tvid tyhd in
        let (btl, rngtl) = emerge_in_list tvid tytl in
          if bhd then (bhd, rnghd) else if btl then (btl, rngtl) else (false, dummy)


(* t -> type_variable_id -> type_struct -> t *)
let rec overwrite theta key value =
  match theta with
  | []             -> []
  | (k, v) :: tail ->
      if k = key then
        (key, value) :: (overwrite tail key value)
      else (k, (overwrite_type_struct v key value)) :: (overwrite tail key value)


(* t -> type_variable_id -> type_struct -> t *)
let overwrite_or_add theta key value =
  overwrite (add theta key value) key value


(* type_struct -> type_struct -> 'a *)
let report_inclusion_error tystr1 tystr2 =
  let rng1 = Typeenv.get_range_from_type tystr1 in
  let rng2 = Typeenv.get_range_from_type tystr2 in
  let (strty1, strty2) = string_of_type_struct_double tystr1 tystr2 in
  let msg =
  ( if is_invalid_range rng1 then
      if is_invalid_range rng2 then
        let (sttln1, _, _, _) = rng1 in
        let (sttln2, _, _, _) = rng2 in
          "something is wrong; (" ^ (string_of_int sttln1) ^ ", " ^ (string_of_int sttln2) ^ ")"
      else
        "at " ^ (describe_position rng2)
    else
      "at " ^ (describe_position rng1)
  ) in
    raise (TypeCheckError(
        msg ^ ":\n"
      ^ "    this expression has types\n"
      ^ "      " ^ strty1 ^ "\n"
      ^ "    and\n"
      ^ "      " ^ strty2 ^ "\n"
      ^ "    at the same time,\n"
      ^ "    but these are incompatible with each other"
    ))

(* type_struct -> type_struct -> 'a *)
let report_contradiction_error tystr1 tystr2 =
  let rng1 = Typeenv.get_range_from_type tystr1 in
  let rng2 = Typeenv.get_range_from_type tystr2 in
  let strty1 = string_of_type_struct tystr1 in
  let strty2 = string_of_type_struct tystr2 in
  let msg1 = describe_position rng1 in
  let msg2 = describe_position rng2 in
    if is_invalid_range rng1 then
      if is_invalid_range rng2 then
        raise ContradictionError
      else
        raise (TypeCheckError(
            "at " ^ msg2 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty2 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty1))
    else
      if is_invalid_range rng2 then
        raise (TypeCheckError(
            "at " ^ msg1 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty1 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty2))
      else
        raise (TypeCheckError(
            "at " ^ msg1 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty1 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty2 ^ ";\n"
          ^ "    this constraint is required by the expression\n"
          ^ "    at " ^ msg2))

(* t -> t *)
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
          if is_invalid_range rng then
            ( print_for_debug "*1\n" ; raise InclusionError )
          else
            report_inclusion_error (TypeVariable(rng, tvid)) tystr
        else
          check_emergence tail


(* t -> t -> t *)
let rec compose theta2 theta1 = fix_subst (compose_prim theta2 theta1)
and compose_prim theta2 theta1 =
  match theta2 with
  | []                     -> theta1
  | (tvid, tystr2) :: tail ->
      ( try
          let tystr1 = find theta1 tvid in
            (tvid, tystr1) :: (compose_prim (eliminate theta1 tvid) (compose_prim tail (unify tystr1 tystr2)))
        with
        | Not_found ->
          ( compose_prim tail (overwrite_or_add theta1 tvid tystr2) )
      )

(* type_struct -> type_struct -> t *)
and unify tystr1 tystr2 =
  try unify_sub tystr1 tystr2 with
  | InclusionError     -> report_inclusion_error tystr1 tystr2
  | ContradictionError ->
       let rng1 = Typeenv.get_range_from_type tystr1 in
       let rng2 = Typeenv.get_range_from_type tystr2 in
         if (is_invalid_range rng1) && (is_invalid_range rng2) then
           let (sttln1, _, _, _) = rng1 in
           let (sttln2, _, _, _) = rng2 in
             raise (TypeCheckError(
                "something is wrong; (" ^ (string_of_int sttln1) ^ ", " ^ (string_of_int sttln2) ^ ")\n"
              ^ "      " ^ (string_of_type_struct tystr1) ^ "\n"
              ^ "    and\n"
              ^ "      " ^ (string_of_type_struct tystr2)))
         else
           report_contradiction_error tystr1 tystr2

(* type_struct -> type_struct -> t *)
and unify_sub tystr1 tystr2 =
  print_for_debug ("  [" ^ (string_of_type_struct_basic tystr1) ^ "] = ["  (* for debug *)
                     ^ (string_of_type_struct_basic tystr2) ^ "]\n") ;     (* for debug *)
  match (tystr1, tystr2) with
  | (IntType(_), IntType(_))                               -> empty
  | (StringType(_), StringType(_))                         -> empty
  | (BoolType(_), BoolType(_))                             -> empty
  | (UnitType(_), UnitType(_))                             -> empty
  | (TypeEnvironmentType(_, _), TypeEnvironmentType(_, _)) -> empty
  | (VariantType(_, varntnm1), VariantType(_, varntnm2))
                                  when varntnm1 = varntnm2 -> empty
  | (FuncType(_, dom1, cod1), FuncType(_, dom2, cod2))     -> compose (unify_sub dom1 dom2) (unify_sub cod1 cod2)
  | (ListType(_, cont1), ListType(_, cont2))               -> unify_sub cont1 cont2
  | (RefType(_, cont1), RefType(_, cont2))                 -> unify_sub cont1 cont2
  | (ProductType(_, tylist1), ProductType(_, tylist2))     -> unify_sub_list tylist1 tylist2
  | (TypeWithRestriction(rng1, restrlst1, cont1), _)       -> unify_sub_restriction rng1 restrlst1 cont1 tystr2
  | (_, TypeWithRestriction(rng2, restrlst2, cont2))       -> unify_sub_restriction rng2 restrlst2 cont2 tystr1
                                                                (* reversed *)
  | (TypeVariable(rng1, tvid1), _)                         -> unify_sub_variable rng1 tvid1 tystr2
  | (_, TypeVariable(rng2, tvid2))                         -> unify_sub_variable rng2 tvid2 tystr1
                                                                (* reversed *)
  | (_, _)                                                 -> report_contradiction_error tystr1 tystr2

and unify_sub_list tylist1 tylist2 =
  match (tylist1, tylist2) with
  | ([], [])                 -> empty
  | (hd1 :: tl1, hd2 :: tl2) -> compose (unify_sub hd1 hd2) (unify_sub_list tl1 tl2)
  | _                        -> raise ContradictionError

and unify_sub_variable rng1 tvid1 tystr2 =
  match tystr2 with
  | TypeVariable(rng2, tvid2) ->
      if tvid1 = tvid2 then
        empty
      else if tvid1 < tvid2 then
        if is_invalid_range rng2  then [(tvid1, TypeVariable(rng1, tvid2))]
                                  else [(tvid1, TypeVariable(rng2, tvid2))]
      else
        if is_invalid_range rng1  then [(tvid2, TypeVariable(rng2, tvid1))]
                                  else [(tvid2, TypeVariable(rng1, tvid1))]
  | other ->
      let (b, _) = emerge_in tvid1 tystr2 in
        if b  then report_inclusion_error (TypeVariable(rng1, tvid1)) tystr2
              else [(tvid1, Typeenv.overwrite_range_of_type tystr2 rng1)]

(* code_range -> type_restriction -> type_struct -> type_struct -> t *)
and unify_sub_restriction rng1 restrlst1 tycont1 tystr2 =
  match tycont1 with
  | FuncType(r, dom, cod) ->
      let newdom = TypeWithRestriction(rng1, restrlst1, dom) in
      let newcod = TypeWithRestriction(rng1, restrlst1, cod) in
        unify_sub (FuncType(r, newdom, newcod)) tystr2

  | ListType(r, cont) ->
      let newcont = TypeWithRestriction(rng1, restrlst1, cont) in
        unify_sub (ListType(r, newcont)) tystr2

  | RefType(r, cont) ->
      let newcont = TypeWithRestriction(rng1, restrlst1, cont) in
        unify_sub (RefType(r, newcont)) tystr2

  | ProductType(r, lst) ->
      let newlst = List.map (fun ty -> TypeWithRestriction(rng1, restrlst1, ty)) lst in
        unify_sub (ProductType(r, newlst)) tystr2

  | TypeVariable(r1, tvid) ->
      ( match tystr2 with
        | TypeWithRestriction(r2, restrlst2, tycont2) ->
            if is_stricter tvid restrlst1 restrlst2 then [(tvid, tycont2)]
                                                    else raise ContradictionError
        | _ ->
            if satisfy tystr2 tvid restrlst1 then [(tvid, tystr2)]
                                        else raise ContradictionError
      )
  | _ -> unify_sub tycont1 tystr2

(* type_variable_id -> type_restriction -> type_restriction -> bool *)
and is_stricter tvid restrlst1 restrlst2 =
  match restrlst2 with
  | []                                          -> true
  | (TypeVariable(rng2, tvid2), tycls2) :: tail ->
      if tvid2 = tvid then
        if is_included (TypeVariable(rng2, tvid2)) tycls2 restrlst1 then is_stricter tvid restrlst1 tail
                                                                    else false
      else
        is_stricter tvid restrlst1 tail
  | _ :: tail                                   -> is_stricter tvid restrlst1 tail

(* type_variable_id -> type_class_name -> type_restriction -> bool *)
and is_included tystr tycls restrlst =
  match restrlst with
  | []                       -> false
  | (tystrx, tyclsx) :: tail ->
      if is_equal_type tystrx tystr && tyclsx = tycls then true else is_included tystr tycls tail

and is_equal_type tystr1 tystr2 =
  match (tystr1, tystr2) with
  | (IntType(_), IntType(_))                               -> true
  | (StringType(_), StringType(_))                         -> true
  | (BoolType(_), BoolType(_))                             -> true
  | (UnitType(_), UnitType(_))                             -> true
  | (TypeEnvironmentType(_, _), TypeEnvironmentType(_, _)) -> true
  | (VariantType(_, varntnm1), VariantType(_, varntnm2))   -> varntnm1 = varntnm2
  | (FuncType(_, dom1, cod1), FuncType(_, dom2, cod2))     -> (is_equal_type dom1 dom2) && (is_equal_type cod1 cod2)
  | (ListType(_, cont1), ListType(_, cont2))               -> is_equal_type cont1 cont2
  | (RefType(_, cont1), RefType(_, cont2))                 -> is_equal_type cont1 cont2
  | (ProductType(_, tylist1), ProductType(_, tylist2))     -> is_equal_type_list tylist1 tylist2
  | (TypeVariable(_, tvid1), TypeVariable(_, tvid2))       -> tvid1 = tvid2
  | _ -> false

and is_equal_type_list tylist1 tylist2 =
  match (tylist1, tylist2) with
  | ([], [])                 -> true
  | (hd1 :: tl1, hd2 :: tl2) -> (is_equal_type hd1 hd2) && (is_equal_type_list tl1 tl2)
  | _                        -> false

and satisfy tystr tvid restrlst =
  match restrlst with
  | []                                       -> true
  | (TypeVariable(_, tvidx), tyclsx) :: tail ->
      if tvidx = tvid then
        if is_included tystr tyclsx restrlst then satisfy tystr tvid tail else false
      else satisfy tystr tvid tail
  | _ :: tail                                -> satisfy tystr tvid tail

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
