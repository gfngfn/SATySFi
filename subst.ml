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
  let rec aux constr theta key =
    match theta with
    | []             -> raise Not_found
    | (k, v) :: tail ->
        if Tyvarid.same k key then constr @ tail else aux ((k, v) :: constr) tail key
  in
    aux [] theta key


let rec overwrite_type_struct (tystr : type_struct) (key : Tyvarid.t) (value : type_struct) =
  let iter = fun ty -> overwrite_type_struct ty key value in
  let (rng, tymain) = tystr in
    match tymain with
    | FuncType(dom, cod)                    -> (rng, FuncType(iter dom, iter cod))
    | ListType(cont)                        -> (rng, ListType(iter cont))
    | RefType(cont)                         -> (rng, RefType(iter cont))
    | ProductType(lst)                      -> (rng, ProductType(List.map iter lst))
    | TypeVariable(k)                       -> if k = key then value else (rng, TypeVariable(k))
    | VariantType(tyarglist, varntnm)       -> (rng, VariantType(List.map iter tyarglist, varntnm))
    | TypeSynonym(tyarglist, tysynnm, cont) -> (rng, TypeSynonym(List.map iter tyarglist, tysynnm, iter cont))
    | other                                 -> (rng, other)


let rec apply_to_type_struct (theta : t) (tystr : type_struct) =
  let iter = apply_to_type_struct theta in
  let (rng, tymain) = tystr in
    match tymain with
    | FuncType(tydom, tycod)                  -> (rng, FuncType(iter tydom, iter tycod))
    | ListType(tycont)                        -> (rng, ListType(iter tycont))
    | RefType(tycont)                         -> (rng, RefType(iter tycont))
    | ProductType(tylist)                     -> (rng, ProductType(List.map iter tylist))
    | TypeVariable(tv)                        -> ( try find theta tv with Not_found -> (rng, TypeVariable(tv)) )
    | VariantType(tyarglist, varntnm)         -> (rng, VariantType(List.map iter tyarglist, varntnm))
    | TypeSynonym(tyarglist, tysynnm, tycont) -> (rng, TypeSynonym(List.map iter tyarglist, tysynnm, iter tycont))
    | other                                   -> (rng, other)


let apply_to_type_environment (theta : t) (tyenv : Typeenv.t) =
  let rec aux theta tyenvlst =
    match tyenvlst with
    | []                     -> tyenvlst
    | (varnm, tystr) :: tail -> (varnm, apply_to_type_struct theta tystr) :: (aux theta tail)
  in
    Typeenv.from_list (aux theta (Typeenv.to_list tyenv))


let rec emerge_in (tvid : Tyvarid.t) (tystr : type_struct) =
  let dr = Range.dummy "emerge_in" in
  let (rng, tymain) = tystr in
    match tymain with
    | FuncType(dom, cod)        ->
        let (bdom, rngdom) = emerge_in tvid dom in
        let (bcod, rngcod) = emerge_in tvid cod in
          if bdom then (bdom, rngdom) else if bcod then (bcod, rngcod) else (false, dr)
    | ListType(cont)            -> emerge_in tvid cont
    | RefType(cont)             -> emerge_in tvid cont
    | ProductType(lst)          -> emerge_in_list tvid lst
    | TypeVariable(tvidx)       -> (Tyvarid.same tvidx tvid, rng)
    | VariantType(lst, _)       -> emerge_in_list tvid lst
    | TypeSynonym(lst, _, cont) ->
        let (bcont, rngcont) = emerge_in tvid cont in
        let (blst, rnglst)   = emerge_in_list tvid lst in
          if bcont then (bcont, rngcont) else if blst then (blst, rnglst) else (false, dr)
    | _                         -> (false, dr)

and emerge_in_list (tvid : Tyvarid.t) (tylist : type_struct list) =
  let dr = Range.dummy "emerge_in_list" in
    match tylist with
    | []           -> (false, dr)
    | tyhd :: tytl ->
        let (bhd, rnghd) = emerge_in tvid tyhd in
        let (btl, rngtl) = emerge_in_list tvid tytl in
          if bhd then (bhd, rnghd) else if btl then (btl, rngtl) else (false, dr)


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
  let (rng1, _) = tystr1 in
  let (rng2, _) = tystr2 in
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
  let (rng1, _) = tystr1 in
  let (rng2, _) = tystr2 in
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
            report_inclusion_error (rng, TypeVariable(tvid)) tystr
        else
          check_emergence tail


let rec compose (theta2 : t) (theta1 : t) = fix_subst (compose_prim theta2 theta1)

and compose_list thetalst = List.fold_right compose thetalst empty

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
    | ((_, TypeSynonym(tyarglist1, _, tycont1)), _) -> unify_sub (Variantenv.apply_to_type_synonym tyarglist1 tycont1) tystr2
    | (_, (_, TypeSynonym(tyarglist2, _, tycont2))) -> unify_sub tystr1 (Variantenv.apply_to_type_synonym tyarglist2 tycont2)
    | _                                             -> unify_sub tystr1 tystr2
  with
  | InternalInclusionError     -> report_inclusion_error tystr1 tystr2
  | InternalContradictionError -> report_contradiction_error tystr1 tystr2


and unify_sub (tystr1 : type_struct) (tystr2 : type_struct) =
  print_for_debug_subst ("  [" ^ (string_of_type_struct_basic tystr1) ^ "] = ["  (* for debug *)
                     ^ (string_of_type_struct_basic tystr2) ^ "]\n") ;     (* for debug *)
  let (rng1, tymain1) = tystr1 in
  let (rng2, tymain2) = tystr2 in
  match (tymain1, tymain2) with
  | (TypeSynonym(_, _, _), _) -> unify tystr1 tystr2
  | (_, TypeSynonym(_, _, _)) -> unify tystr1 tystr2

  | (IntType, IntType)        -> empty
  | (StringType, StringType)  -> empty
  | (BoolType, BoolType)      -> empty
  | (UnitType, UnitType)      -> empty

  | (FuncType(dom1, cod1), FuncType(dom2, cod2)) -> compose (unify_sub dom1 dom2) (unify_sub cod1 cod2)

  | (ListType(cont1), ListType(cont2))           -> unify_sub cont1 cont2

  | (RefType(cont1), RefType(cont2))             -> unify_sub cont1 cont2

  | (ProductType(tylist1), ProductType(tylist2)) -> unify_sub_list tylist1 tylist2

  | (VariantType(tyarglist1, varntnm1), VariantType(tyarglist2, varntnm2))
                        when varntnm1 = varntnm2 -> unify_sub_list tyarglist1 tyarglist2

  | (TypeVariable(tvid1), _) ->
      begin
        match tymain2 with
        | TypeVariable(tvid2) ->
            if Tyvarid.same tvid1 tvid2 then
              empty
            else
              let (tvid1new, tvid2new) = Tyvarid.make_unquantifiable_if_needed (tvid1, tvid2) in
                if Tyvarid.less_than tvid1 tvid2 then
                  if Range.is_dummy rng2 then [(tvid1new, (rng1, TypeVariable(tvid2new)))]
                                         else [(tvid1new, (rng2, TypeVariable(tvid2new)))]
                else
                  if Range.is_dummy rng1 then [(tvid2new, (rng1, TypeVariable(tvid1new)))]
                                         else [(tvid2new, (rng2, TypeVariable(tvid1new)))]
        | other ->
            let (b, _) = emerge_in tvid1 tystr2 in
              if b then
                report_inclusion_error (rng1, TypeVariable(tvid1)) tystr2
              else
                if Range.is_dummy rng1 then [(tvid1, (rng2, tymain2))]
                                       else [(tvid1, (rng1, tymain2))]
      end

  | (_, TypeVariable(_)) -> unify_sub tystr2 tystr1

  | _                    -> raise InternalContradictionError


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
