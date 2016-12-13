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


let add (theta : t) (key : Tyvarid.t) (value : type_struct) =
  let rec aux key value theta accrev =
    match theta with
    | []             -> List.rev ((key, value) :: accrev)
    | (k, v) :: tail -> if Tyvarid.same k key then List.rev_append accrev ((key, value) :: tail)
                                              else aux key value tail ((k, v) :: accrev)
  in
    aux key value theta []


let find (theta : t) (key : Tyvarid.t) =
  let (_, value) = List.find (fun (k, v) -> Tyvarid.same k key) theta in
    value


let mem (key : Tyvarid.t) (theta : t) =
  try 
    let _ = find theta key in true
  with
  | Not_found -> false


(* PUBLIC *)
let rec apply_to_type_struct (theta : t) (tystr : type_struct) =
  let iter = apply_to_type_struct theta in
  let (rng, tymain) = tystr in
    match tymain with
    | TypeVariable(tv)                        -> begin try find theta tv with Not_found -> (rng, TypeVariable(tv)) end
    | FuncType(tydom, tycod)                  -> (rng, FuncType(iter tydom, iter tycod))
    | ProductType(tylist)                     -> (rng, ProductType(List.map iter tylist))
    | ListType(tycont)                        -> (rng, ListType(iter tycont))
    | RefType(tycont)                         -> (rng, RefType(iter tycont))
    | VariantType(tyarglist, varntnm)         -> (rng, VariantType(List.map iter tyarglist, varntnm))
    | TypeSynonym(tyarglist, tysynnm, tycont) -> (rng, TypeSynonym(List.map iter tyarglist, tysynnm, iter tycont))
    | RecordType(asc)                         -> (rng, RecordType(Assoc.map_value iter asc))
    | other                                   -> (rng, other)


(* PUBLIC *)
let apply_to_type_environment (theta : t) (tyenv : Typeenv.t) =
  Typeenv.map (fun (varnm, tystr) -> (varnm, apply_to_type_struct theta tystr)) tyenv


let rec emerge_in (tvid : Tyvarid.t) (tystr : type_struct) =
  let dr = Range.dummy "emerge_in" in
  let iter      = emerge_in tvid in
  let iter_list = emerge_in_list tvid in
  let (rng, tymain) = tystr in
    match tymain with
    | FuncType(dom, cod)        ->
        let (bdom, rngdom) = iter dom in
        let (bcod, rngcod) = iter cod in
          if bdom then (bdom, rngdom) else if bcod then (bcod, rngcod) else (false, dr)
    | ListType(cont)            -> iter cont
    | RefType(cont)             -> iter cont
    | ProductType(lst)          -> iter_list lst
    | TypeVariable(tvidx)       -> (Tyvarid.same tvidx tvid, rng)
    | VariantType(lst, _)       -> iter_list lst
    | TypeSynonym(lst, _, cont) ->
        let (bcont, rngcont) = iter cont in
        let (blst, rnglst)   = iter_list lst in
          if bcont then (bcont, rngcont) else if blst then (blst, rnglst) else (false, dr)
    | RecordType(asc)           -> iter_list (Assoc.to_value_list asc)
    | ( UnitType
      | BoolType
      | IntType
      | StringType )            -> (false, dr)
    | ForallType(_, _, _)       -> (false, dr)
    | TypeArgument(_)           -> (false, dr)


and emerge_in_list (tvid : Tyvarid.t) (tylist : type_struct list) =
  let dr = Range.dummy "emerge_in_list" in
    match tylist with
    | []           -> (false, dr)
    | tyhd :: tytl ->
        let (bhd, rnghd) = emerge_in tvid tyhd in
        let (btl, rngtl) = emerge_in_list tvid tytl in
          if bhd then (bhd, rnghd) else if btl then (btl, rngtl) else (false, dr)


let replace_type_variable_in_subst (theta : t) (key : Tyvarid.t) (value : type_struct) =
  let f = (fun tystr -> replace_type_variable tystr key value) in
    List.map (fun (tvid, tystr) -> (tvid, f tystr)) theta


let replace_type_variable_in_equations (eqnlst : (type_struct * type_struct) list) (key : Tyvarid.t) (value : type_struct) =
  let f = (fun tystr -> replace_type_variable tystr key value) in
    List.map (fun (tystr1, tystr2) -> (f tystr1, f tystr2)) eqnlst


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


(* PUBLIC *)
let compose (theta2 : t) (theta1 : t) =
  let res1 = List.map (fun (tvid, tystr) -> (tvid, apply_to_type_struct theta2 tystr)) theta1 in
  let res2 = List.filter (fun (tvid, tystr) -> not (mem tvid theta1)) theta2 in
    List.append res1 res2


(* PUBLIC *)
let compose_list thetalst = List.fold_right compose thetalst empty


let rec unify_sub (eqnlst : (type_struct * type_struct) list) (acctheta : t) =
    let _ = print_for_debug_subst "    |----" in (* for debug *)
    let _ = List.iter (fun (tystr1, tystr2) -> print_for_debug_subst (" [" ^ (string_of_type_struct_basic tystr1)              (* for debug *)
                                                                      ^ " = " ^ (string_of_type_struct_basic tystr2) ^ "]")) eqnlst in (* for debug *)
    let _ = print_for_debug_subst "\n" in (* for debug *)
  match eqnlst with
  | []                          -> acctheta
  | (tystr1, tystr2) :: eqntail ->
    let iter_none newacctheta          = unify_sub eqntail newacctheta in
    let iter_add addedeqns newacctheta = unify_sub (List.append addedeqns eqntail) newacctheta in
    let iter_complete x y              = unify_sub x y in
    let (rng1, tymain1) = tystr1 in
    let (rng2, tymain2) = tystr2 in
      match (tymain1, tymain2) with
      | (TypeSynonym(tyarglist1, _, tycont1), _) -> iter_add [(Variantenv.apply_to_type_synonym tyarglist1 tycont1, tystr2)] acctheta
      | (_, TypeSynonym(tyarglist2, _, tycont2)) -> iter_add [(tystr1, Variantenv.apply_to_type_synonym tyarglist2 tycont2)] acctheta

      | (IntType, IntType)        -> iter_none acctheta
      | (StringType, StringType)  -> iter_none acctheta
      | (BoolType, BoolType)      -> iter_none acctheta
      | (UnitType, UnitType)      -> iter_none acctheta

      | (FuncType(dom1, cod1), FuncType(dom2, cod2)) -> iter_add [(dom1, dom2); (cod1, cod2)] acctheta
      | (ProductType(tylist1), ProductType(tylist2)) ->
          if List.length tylist1 <> List.length tylist2 then
            raise InternalContradictionError
          else
            iter_add (List.combine tylist1 tylist2) acctheta

      | (ListType(cont1), ListType(cont2))           -> iter_add [(cont1, cont2)] acctheta
      | (RefType(cont1), RefType(cont2))             -> iter_add [(cont1, cont2)] acctheta
      | (VariantType(tyarglist1, varntnm1), VariantType(tyarglist2, varntnm2))
                            when varntnm1 = varntnm2 -> iter_add (List.combine tyarglist1 tyarglist2) acctheta

      | (TypeVariable(tvid1), TypeVariable(tvid2))
                     when Tyvarid.same tvid1 tvid2 -> iter_none acctheta

      | (TypeVariable(tvid1), TypeVariable(tvid2)) ->
                let () = Tyvarid.make_unquantifiable_if_needed (tvid1, tvid2) in
                let (oldtvid, newtystr) = if Range.is_dummy rng1 then (tvid1, tystr2) else (tvid2, tystr1) in
                    let neweqnlst = replace_type_variable_in_equations eqntail oldtvid newtystr in
                    let newacctheta = add (replace_type_variable_in_subst acctheta oldtvid newtystr) oldtvid newtystr in
                      iter_complete neweqnlst newacctheta

      | (TypeVariable(tvid1), _) ->
                let (b, _) = emerge_in tvid1 tystr2 in
                  if b then
                      report_inclusion_error (rng1, TypeVariable(tvid1)) tystr2
                  else
                    let _ = print_for_debug_subst ("    substitute " ^ (string_of_type_struct_basic tystr1) ^ " with " ^ (string_of_type_struct_basic tystr2) ^ "\n") in (* for debug *)
                    let newtystr = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
                    let neweqnlst = replace_type_variable_in_equations eqntail tvid1 newtystr in
                    let newacctheta = add (replace_type_variable_in_subst acctheta tvid1 newtystr) tvid1 newtystr in
                      iter_complete neweqnlst newacctheta

      | (_, TypeVariable(_)) -> iter_add [(tystr2, tystr1)] acctheta

      | _                    -> raise InternalContradictionError


(* PUBLIC *)
let unify (tystr1 : type_struct) (tystr2 : type_struct) =
(*  print_for_debug_subst ("  unify [" ^ (string_of_type_struct_basic tystr1) ^ " = "  (* for debug *)
    ^ (string_of_type_struct_basic tystr2) ^ "]\n") ;          (* for debug *) *)
  let _ = print_for_debug_subst "  unify\n" in (* for debug *)
  try
    unify_sub [(tystr1, tystr2)] empty
  with
  | InternalInclusionError     -> report_inclusion_error tystr1 tystr2
  | InternalContradictionError -> report_contradiction_error tystr1 tystr2


(* for test *)
(* PUBLIC *)
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
