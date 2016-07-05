open Types
open Display

exception InternalInclusionError
exception InternalContradictionError
exception InclusionError     of string
exception ContradictionError of string


type t = (Tyvarid.t * type_struct) list


let print_for_debug_subst msg =
(*
  print_string msg ;
*)
  ()


let empty = []


let add (theta : t) (key : Tyvarid.t) (value : type_struct) = (key, value) :: theta


let rec find (theta : t) (key : Tyvarid.t) =
  match theta with
  | []             -> raise Not_found
  | (k, v) :: tail -> if Tyvarid.same k key then v else find tail key


let eliminate (theta : t) (key : Tyvarid.t) =
  let rec sub constr theta key =
    match theta with
    | []             -> raise Not_found
    | (k, v) :: tail ->
        if Tyvarid.same k key then constr @ tail else sub ((k, v) :: constr) tail key
  in
    sub [] theta key



let rec overwrite_type_struct (tystr : type_struct) (key : Tyvarid.t) (value : type_struct) =
  let iter = fun ty -> overwrite_type_struct ty key value in
    match tystr with
    | FuncType(rng, dom, cod)                    -> FuncType(rng, iter dom, iter cod)
    | ListType(rng, cont)                        -> ListType(rng, iter cont)
    | RefType(rng, cont)                         -> RefType(rng, iter cont)
    | ProductType(rng, lst)                      -> ProductType(rng, List.map iter lst)
    | TypeVariable(rng, k)                       -> if k = key then value else TypeVariable(rng, k)
    | VariantType(rng, tyarglist, varntnm)       -> VariantType(rng, List.map iter tyarglist, varntnm)
    | TypeSynonym(rng, tyarglist, tysynnm, cont) -> TypeSynonym(rng, List.map iter tyarglist, tysynnm, iter cont)
    | other                                      -> other


let rec apply_to_type_struct (theta : t) (tystr : type_struct) =
  let f = apply_to_type_struct theta in
    match tystr with
    | FuncType(rng, tydom, tycod)                  -> FuncType(rng, f tydom, f tycod)
    | ListType(rng, tycont)                        -> ListType(rng, f tycont)
    | RefType(rng, tycont)                         -> RefType(rng, f tycont)
    | ProductType(rng, tylist)                     -> ProductType(rng, List.map f tylist)
    | TypeVariable(rng, tv)                        -> ( try find theta tv with Not_found -> TypeVariable(rng, tv) )
    | VariantType(rng, tyarglist, varntnm)         -> VariantType(rng, List.map f tyarglist, varntnm)
    | TypeSynonym(rng, tyarglist, tysynnm, tycont) -> TypeSynonym(rng, List.map f tyarglist, tysynnm, f tycont)
    | other                                        -> other


let apply_to_type_environment (theta : t) (tyenv : Typeenv.t) =
  let rec iter theta tyenvlst =
    match tyenvlst with
    | []                     -> tyenvlst
    | (varnm, tystr) :: tail -> (varnm, apply_to_type_struct theta tystr) :: (iter theta tail)
  in
    Typeenv.from_list (iter theta (Typeenv.to_list tyenv))


let rec emerge_in (tvid : Tyvarid.t) (tystr : type_struct) =
  let dummy = Range.dummy "emerge_in" in
    match tystr with
    | FuncType(_, dom, cod)    ->
        let (bdom, rngdom) = emerge_in tvid dom in
        let (bcod, rngcod) = emerge_in tvid cod in
          if bdom then (bdom, rngdom) else if bcod then (bcod, rngcod) else (false, dummy)
    | ListType(_, cont)            -> emerge_in tvid cont
    | RefType(_, cont)             -> emerge_in tvid cont
    | ProductType(_, lst)          -> emerge_in_list tvid lst
    | TypeVariable(rng, tvidx)     -> (Tyvarid.same tvidx tvid, rng)
    | VariantType(rng, lst, _)     -> emerge_in_list tvid lst
    | TypeSynonym(_, lst, _, cont) ->
        let (bcont, rngcont) = emerge_in tvid cont in
        let (blst, rnglst)   = emerge_in_list tvid lst in
          if bcont then (bcont, rngcont) else if blst then (blst, rnglst) else (false, dummy)
    | _                            -> (false, dummy)

and emerge_in_list (tvid : Tyvarid.t) (tylist : type_struct list) =
  let dummy = Range.dummy "emerge_in_list" in
    match tylist with
    | []           -> (false, dummy)
    | tyhd :: tytl ->
        let (bhd, rnghd) = emerge_in tvid tyhd in
        let (btl, rngtl) = emerge_in_list tvid tytl in
          if bhd then (bhd, rnghd) else if btl then (btl, rngtl) else (false, dummy)


let rec overwrite (theta : t) (key : Tyvarid.t) (value : type_struct) =
  match theta with
  | []             -> []
  | (k, v) :: tail ->
      if Tyvarid.same k key then
        (key, value) :: (overwrite tail key value)
      else (k, (overwrite_type_struct v key value)) :: (overwrite tail key value)


let overwrite_or_add (theta : t) (key : Tyvarid.t) (value : type_struct) =
  overwrite (add theta key value) key value


let report_inclusion_error (tystr1 : type_struct) (tystr2 : type_struct) =
  let rng1 = Typeenv.get_range_from_type tystr1 in
  let rng2 = Typeenv.get_range_from_type tystr2 in
  let (strty1, strty2) = string_of_type_struct_double tystr1 tystr2 in
  let msg =
    match (Range.is_dummy rng1, Range.is_dummy rng2) with
    | (true, true) -> "(cannot report position: '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')"
    | (_, false)   -> "at " ^ (Range.to_string rng2)
    | (false, _)   -> "at " ^ (Range.to_string rng1)
  in
    raise (InclusionError(
        msg ^ ":\n"
      ^ "    this expression has types\n"
      ^ "      " ^ strty1 ^ "\n"
      ^ "    and\n"
      ^ "      " ^ strty2 ^ "\n"
      ^ "    at the same time,\n"
      ^ "    but these are incompatible with each other"
    ))


let report_contradiction_error (tystr1 : type_struct) (tystr2 : type_struct) =
  let rng1 = Typeenv.get_range_from_type tystr1 in
  let rng2 = Typeenv.get_range_from_type tystr2 in
  let strty1 = string_of_type_struct tystr1 in
  let strty2 = string_of_type_struct tystr2 in
  let strrng1 = Range.to_string rng1 in
  let strrng2 = Range.to_string rng2 in
  let msg =
    match (Range.is_dummy rng1, Range.is_dummy rng2) with
    | (true, true)  -> "(cannot report position; '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')"
    | (true, false) ->
            "at " ^ strrng2 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty2 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty1
    | (false, true) ->
            "at " ^ strrng1 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty1 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty2
    | (false, false) ->
            "at " ^ strrng1 ^ ":\n"
          ^ "    this expression has type\n"
          ^ "      " ^ strty1 ^ "\n"
          ^ "    but is expected of type\n"
          ^ "      " ^ strty2 ^ ";\n"
          ^ "    this constraint is required by the expression\n"
          ^ "    at " ^ strrng2
  in
    raise (ContradictionError(msg))


let rec fix_subst (theta : t) = fix_subst_sub theta theta
and fix_subst_sub rest from =
  match rest with
  | []                    -> begin check_emergence from ; from end
  | (tvid, tystr) :: tail -> fix_subst_sub tail (overwrite from tvid tystr)


and check_emergence (theta : t) =
  match theta with
  | []                    -> ()
  | (tvid, tystr) :: tail ->
      let (b, rng) = emerge_in tvid tystr in
        if b then
          if Range.is_dummy rng then
            raise InternalInclusionError
          else
            report_inclusion_error (TypeVariable(rng, tvid)) tystr
        else
          check_emergence tail


let rec compose (theta2 : t) (theta1 : t) = fix_subst (compose_prim theta2 theta1)

and compose_prim (theta2 : t) (theta1 : t) =
  match theta2 with
  | []                     -> theta1
  | (tvid, tystr2) :: tail ->
      begin
        try
          let tystr1 = find theta1 tvid in
            (tvid, tystr1) :: (compose_prim (eliminate theta1 tvid) (compose_prim tail (unify tystr1 tystr2)))
        with
        | Not_found -> compose_prim tail (overwrite_or_add theta1 tvid tystr2)
      end


and unify (tystr1 : type_struct) (tystr2 : type_struct) =
  print_for_debug_subst (" unify [" ^ (string_of_type_struct_basic tystr1) ^ "] = ["  (* for debug *)
                     ^ (string_of_type_struct_basic tystr2) ^ "]\n") ;          (* for debug *)
  try
    match (tystr1, tystr2) with
    | (TypeSynonym(_, tyarglist1, _, tycont1), _) -> unify_sub (Variantenv.apply_to_type_synonym tyarglist1 tycont1) tystr2
    | (_, TypeSynonym(_, tyarglist2, _, tycont2)) -> unify_sub tystr1 (Variantenv.apply_to_type_synonym tyarglist2 tycont2)
    | _                                           -> unify_sub tystr1 tystr2
  with
  | InternalInclusionError     -> report_inclusion_error tystr1 tystr2
  | InternalContradictionError -> report_contradiction_error tystr1 tystr2


and unify_sub (tystr1 : type_struct) (tystr2 : type_struct) =
  print_for_debug_subst ("  [" ^ (string_of_type_struct_basic tystr1) ^ "] = ["  (* for debug *)
                     ^ (string_of_type_struct_basic tystr2) ^ "]\n") ;     (* for debug *)

  match (tystr1, tystr2) with
  | (TypeSynonym(_, _, _, _), _)   -> unify tystr1 tystr2
  | (_, TypeSynonym(_, _, _, _))   -> unify tystr1 tystr2

  | (IntType(_), IntType(_))       -> empty
  | (StringType(_), StringType(_)) -> empty
  | (BoolType(_), BoolType(_))     -> empty
  | (UnitType(_), UnitType(_))     -> empty

  | (FuncType(_, dom1, cod1), FuncType(_, dom2, cod2)) -> compose (unify_sub dom1 dom2) (unify_sub cod1 cod2)

  | (ListType(_, cont1), ListType(_, cont2))           -> unify_sub cont1 cont2

  | (RefType(_, cont1), RefType(_, cont2))             -> unify_sub cont1 cont2

  | (ProductType(_, tylist1), ProductType(_, tylist2)) -> unify_sub_list tylist1 tylist2

  | (VariantType(_, tyarglist1, varntnm1), VariantType(_, tyarglist2, varntnm2))
                              when varntnm1 = varntnm2 -> unify_sub_list tyarglist1 tyarglist2

  | (TypeVariable(rng1, tvid1), tystr) ->
      begin
        match tystr with
        | TypeVariable(rng2, tvid2) ->
            if Tyvarid.same tvid1 tvid2 then
              empty
            else
              let (tvid1new, tvid2new) = Tyvarid.make_unquantifiable_if_needed (tvid1, tvid2) in
                if Tyvarid.less_than tvid1 tvid2 then
                  if Range.is_dummy rng2  then [(tvid1new, TypeVariable(rng1, tvid2new))]
                                          else [(tvid1new, TypeVariable(rng2, tvid2new))]
                else
                  if Range.is_dummy rng1  then [(tvid2new, TypeVariable(rng2, tvid1new))]
                                          else [(tvid2new, TypeVariable(rng1, tvid1new))]
        | other ->
            let (b, _) = emerge_in tvid1 tystr in
              if b then
                report_inclusion_error (TypeVariable(rng1, tvid1)) tystr
              else
                if Range.is_dummy rng1 then [(tvid1, tystr)]
                                       else [(tvid1, Typeenv.overwrite_range_of_type tystr rng1)]
      end

  | (tystr, TypeVariable(rng, tvid)) -> unify_sub (TypeVariable(rng, tvid)) tystr

  | _                                -> raise InternalContradictionError


and unify_sub_list (tylist1 : type_struct list) (tylist2 : type_struct list) =
  match (tylist1, tylist2) with
  | ([], [])                 -> empty
  | (hd1 :: tl1, hd2 :: tl2) -> compose (unify_sub hd1 hd2) (unify_sub_list tl1 tl2)
  | _                        -> raise InternalContradictionError


(* for test *)
let string_of_subst (theta : t) =
  let rec iter (theta : t) =
    match theta with
    | []                    -> ""
    | (tvid, tystr) :: tail ->
        " | '" ^ (Tyvarid.show_direct tvid) ^ " := " ^ (string_of_type_struct_basic tystr) ^ "\n"
          ^ (iter tail)
  in
      " +-------------------------------\n"
    ^ (iter theta)
    ^ " +-------------------------------\n"
