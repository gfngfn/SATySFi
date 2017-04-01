open Types
open Display

exception InternalInclusionError     of Kindenv.t
exception InternalContradictionError of Kindenv.t
exception InclusionError     of Kindenv.t * mono_type * mono_type
exception ContradictionError of Kindenv.t * mono_type * mono_type


type t = (Tyvarid.t * mono_type) list


let print_for_debug_subst msg =

  print_string msg ;

  ()


let empty = []


let add (theta : t) (key : Tyvarid.t) (value : mono_type) =
  let rec aux theta accrev =
    match theta with
    | []                                      -> List.rev ((key, value) :: accrev)
    | (k, v) :: tail  when Tyvarid.same k key -> List.rev_append accrev ((key, value) :: tail)
    | (k, v) :: tail                          -> aux tail ((k, v) :: accrev)
  in
    aux theta []


let find (theta : t) (key : Tyvarid.t) =
  let (_, value) = List.find (fun (k, v) -> Tyvarid.same k key) theta in
    value


let mem (key : Tyvarid.t) (theta : t) =
  try
    let _ = find theta key in true
  with
  | Not_found -> false


(* PUBLIC *)
let rec apply_to_mono_type (theta : t) ((rng, tymain) : mono_type) =
  let iter      = apply_to_mono_type theta in
  let iter_poly = poly_extend (apply_to_mono_type theta) in
    match tymain with
    | TypeVariable(tv)                        -> begin try find theta tv with Not_found -> (rng, TypeVariable(tv)) end
    | FuncType(tydom, tycod)                  -> (rng, FuncType(iter tydom, iter tycod))
    | ProductType(tylist)                     -> (rng, ProductType(List.map iter tylist))
    | ListType(tycont)                        -> (rng, ListType(iter tycont))
    | RefType(tycont)                         -> (rng, RefType(iter tycont))
    | VariantType(tyarglist, varntnm)         -> (rng, VariantType(List.map iter tyarglist, varntnm))
    | TypeSynonym(tyarglist, tysynnm, pty)    -> (rng, TypeSynonym(List.map iter tyarglist, tysynnm, iter_poly pty))
    | RecordType(asc)                         -> (rng, RecordType(Assoc.map_value iter asc))
    | _                                       -> (rng, tymain)


(* PUBLIC *)
let apply_to_type_environment (theta : t) (tyenv : Typeenv.t) =
  Typeenv.map (fun (varnm, pty) -> (varnm, poly_extend (apply_to_mono_type theta) pty)) tyenv


let rec occurs (tvid : Tyvarid.t) ((_, tymain) : mono_type) : bool =
  let iter      = occurs tvid in
  let iter_list = List.fold_left (fun b ty -> b || occurs tvid ty) false in
  let iter_poly = poly_extend_general (occurs tvid) (fun it _ _ ptysub -> it ptysub) in (* temporary : this should traverse kd? *)
    match tymain with
    | FuncType(dom, cod)        -> iter dom || iter cod
    | ListType(cont)            -> iter cont
    | RefType(cont)             -> iter cont
    | ProductType(lst)          -> iter_list lst
    | TypeVariable(tvidx)       -> Tyvarid.same tvidx tvid
    | VariantType(lst, _)       -> iter_list lst
    | TypeSynonym(lst, _, pty)  -> iter_list lst || iter_poly pty
    | RecordType(asc)           -> iter_list (Assoc.to_value_list asc)
    | ( UnitType
      | BoolType
      | IntType
      | StringType )            -> false
    | TypeArgument(_)           -> false


let replace_type_variable_in_subst (theta : t) (key : Tyvarid.t) (value : mono_type) =
  let f = (fun tystr -> replace_type_variable tystr key value) in
    List.map (fun (tvid, tystr) -> (tvid, f tystr)) theta


let replace_type_variable_in_equations (eqnlst : (mono_type * mono_type) list) (key : Tyvarid.t) (value : mono_type) =
  let f = (fun tystr -> replace_type_variable tystr key value) in
    List.map (fun (tystr1, tystr2) -> (f tystr1, f tystr2)) eqnlst


let replace_type_variable_in_mono_type (tystr : mono_type) (key : Tyvarid.t) (value : mono_type) =
  Typeenv.replace_id [(key, value)] tystr

(*
let replace_type_variable_in_kind (kdstr : kind) (key : Tyvarid.t) (value : mono_type) =
  match kdstr with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value (fun tystr -> replace_type_variable_in_mono_type tystr key value) asc)
*)

let report_inclusion_error (kdenv : Kindenv.t) (ty1 : mono_type) (ty2 : mono_type) =
(*
  let (rng1, _) = tystr1 in
  let (rng2, _) = tystr2 in
  let (strty1, strty2) = string_of_mono_type_double kdenv tystr1 tystr2 in
  let msg =
    match (Range.is_dummy rng1, Range.is_dummy rng2) with
    | (true, true) -> "(cannot report position: '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')"
    | (_, false)   -> "at " ^ (Range.to_string rng2)
    | (false, _)   -> "at " ^ (Range.to_string rng1)
  in
*)
    raise (InclusionError(kdenv, ty1, ty2))
(*
        msg ^ ":\n"
      ^ "    this expression has types\n"
      ^ "      " ^ strty1 ^ "\n"
      ^ "    and\n"
      ^ "      " ^ strty2 ^ "\n"
      ^ "    at the same time,\n"
      ^ "    but these are incompatible with each other"
    ))
*)

let report_contradiction_error (kdenv : Kindenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  raise (ContradictionError(kdenv, ty1, ty2))


(* PUBLIC *)
let compose (theta2 : t) (theta1 : t) =
  let res1 = List.map (fun (tvid, tystr) -> (tvid, apply_to_mono_type theta2 tystr)) theta1 in
  let res2 = List.filter (fun (tvid, tystr) -> not (mem tvid theta1)) theta2 in
    List.append res1 res2


(* PUBLIC *)
let compose_list thetalst = List.fold_right compose thetalst empty


let rec unify_sub (kdenv : Kindenv.t) (eqnlst : (mono_type * mono_type) list) (acctheta : t) (acckdenv : Kindenv.t) =
    let _ = print_for_debug_subst "    |----" in                                                                    (* for debug *)
    let _ = List.iter (fun (tystr1, tystr2) ->                                                                      (* for debug *)
      print_for_debug_subst (" [" ^ (string_of_mono_type_basic tystr1)                                            (* for debug *)
                             ^ " = " ^ (string_of_mono_type_basic tystr2) ^ "]")) eqnlst in                       (* for debug *)
    let _ = print_for_debug_subst "\n" in                                                                           (* for debug *)
    let _ = print_for_debug_subst ("        (kinds(K) " ^ (Display.string_of_kind_environment kdenv) ^ ")\n") in    (* for debug *)
    let _ = print_for_debug_subst ("        (kinds(S) " ^ (Display.string_of_kind_environment acckdenv) ^ ")\n") in (* for debug *)
  match eqnlst with
  | []                          -> (acctheta, kdenv)
  | (tystr1, tystr2) :: eqntail ->
    let iter_none ()       = unify_sub kdenv eqntail acctheta acckdenv in
    let iter_add addedeqns = unify_sub kdenv (List.append addedeqns eqntail) acctheta acckdenv in
    let iter_complete      = unify_sub in
    let (rng1, tymain1) = tystr1 in
    let (rng2, tymain2) = tystr2 in
      match (tymain1, tymain2) with
      | (TypeSynonym(tyarglist1, _, tycont1), _) -> iter_add [(Variantenv.apply_to_type_synonym tyarglist1 tycont1, tystr2)]
      | (_, TypeSynonym(tyarglist2, _, tycont2)) -> iter_add [(tystr1, Variantenv.apply_to_type_synonym tyarglist2 tycont2)]

      | (IntType, IntType)        -> iter_none ()
      | (StringType, StringType)  -> iter_none ()
      | (BoolType, BoolType)      -> iter_none ()
      | (UnitType, UnitType)      -> iter_none ()

      | (FuncType(dom1, cod1), FuncType(dom2, cod2)) -> iter_add [(dom1, dom2); (cod1, cod2)]

      | (ProductType(tylist1), ProductType(tylist2)) ->
          if List.length tylist1 <> List.length tylist2 then
            raise (InternalContradictionError(kdenv))
          else
            iter_add (List.combine tylist1 tylist2)

      | (RecordType(asc1), RecordType(asc2)) ->
          if not (Assoc.domain_same asc1 asc2) then
            raise (InternalContradictionError(kdenv))
          else
            iter_add (Assoc.combine_value asc1 asc2)

      | (VariantType(tyarglist1, varntnm1), VariantType(tyarglist2, varntnm2))
                            when varntnm1 = varntnm2 -> iter_add (List.combine tyarglist1 tyarglist2)

      | (ListType(cont1), ListType(cont2))           -> iter_add [(cont1, cont2)]
      | (RefType(cont1), RefType(cont2))             -> iter_add [(cont1, cont2)]

      | (TypeVariable(tvid1), TypeVariable(tvid2))
                     when Tyvarid.same tvid1 tvid2 -> iter_none ()

      | (TypeVariable(tvid1), TypeVariable(tvid2)) ->
                let () = Tyvarid.make_unquantifiable_if_needed (tvid1, tvid2) in
                let (oldtvid, newtvid, newtystr) = if Range.is_dummy rng1 then (tvid1, tvid2, tystr2) else (tvid2, tvid1, tystr1) in
                let _ = print_for_debug_subst                                                                      (* for debug *)
                  ("    substituteVV " ^ (string_of_mono_type_basic (Range.dummy "", TypeVariable(oldtvid)))     (* for debug *)
                   ^ " with " ^ (string_of_mono_type_basic newtystr) ^ "\n") in                                  (* for debug *)
                let kdstr1 = Kindenv.find kdenv tvid1 in
                let kdstr2 = Kindenv.find kdenv tvid2 in
                let (eqnlstbyrecord, kdstrunion) =
                  match (kdstr1, kdstr2) with
                  | (UniversalKind, UniversalKind)       -> ([], UniversalKind)
                  | (RecordKind(asc1), UniversalKind)    -> ([], RecordKind(asc1))
                  | (UniversalKind, RecordKind(asc2))    -> ([], RecordKind(asc2))
                  | (RecordKind(asc1), RecordKind(asc2)) ->
                      let pureunion = RecordKind(Assoc.union asc1 asc2) in
                        (Assoc.intersection asc1 asc2, Kindenv.replace_type_variable_in_kind pureunion oldtvid newtystr)
                in
                  let neweqnlst = replace_type_variable_in_equations (List.append eqnlstbyrecord eqntail) oldtvid newtystr in
                  let newkdenv = Kindenv.add (Kindenv.replace_type_variable_in_kindenv kdenv oldtvid newtystr) newtvid kdstrunion in
                  let newacctheta = add (replace_type_variable_in_subst acctheta oldtvid newtystr) oldtvid newtystr in
                  let newacckdenv = Kindenv.add (Kindenv.replace_type_variable_in_kindenv kdenv oldtvid newtystr) oldtvid kdstr1 in
                      (* doubtful *)
                    iter_complete newkdenv neweqnlst newacctheta newacckdenv

      | (TypeVariable(tvid1), RecordType(asc2)) ->
                let kdstr1 = Kindenv.find kdenv tvid1 in
                let binc = match kdstr1 with UniversalKind -> true | RecordKind(asc1) -> Assoc.domain_included asc1 asc2 in
                  if occurs tvid1 tystr2 then
                    report_inclusion_error kdenv tystr1 tystr2
                  else if not binc then
                    raise (InternalContradictionError(kdenv))
                  else
                    let newtystr2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
                    let _ = print_for_debug_subst                                      (* for debug *)
                      ("    substituteVR " ^ (string_of_mono_type_basic tystr1)     (* for debug *)
                       ^ " with " ^ (string_of_mono_type_basic newtystr2) ^ "\n") in (* for debug *)
                    let eqnlstbyrecord =
                      match kdstr1 with
                      | UniversalKind    -> []
                      | RecordKind(asc1) -> Assoc.intersection asc1 asc2
                    in
                      let neweqnlst = replace_type_variable_in_equations (List.append eqnlstbyrecord eqntail) tvid1 newtystr2 in
                      let newkdenv = Kindenv.replace_type_variable_in_kindenv kdenv tvid1 newtystr2 in
                      let newacctheta = add (replace_type_variable_in_subst acctheta tvid1 newtystr2) tvid1 newtystr2 in
                      let newacckdenv = Kindenv.add (Kindenv.replace_type_variable_in_kindenv acckdenv tvid1 newtystr2) tvid1 kdstr1 in
                        iter_complete newkdenv neweqnlst newacctheta newacckdenv


      | (TypeVariable(tvid1), _) ->
                  if occurs tvid1 tystr2 then
                      report_inclusion_error kdenv tystr1 tystr2
                  else
(*                  begin
                    match Kindenv.find kdenv tvid1 with
                    | RecordKind(_) -> report_contradiction_error
                    | UniversalKind -> *)
                    let newtystr2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
                    let _ = print_for_debug_subst                                      (* for debug *)
                      ("    substituteVX " ^ (string_of_mono_type_basic tystr1)      (* for debug *)
                       ^ " with " ^ (string_of_mono_type_basic newtystr2) ^ "\n") in (* for debug *)
                    let newkdenv = Kindenv.replace_type_variable_in_kindenv kdenv tvid1 newtystr2 in
                    let _ = print_for_debug_subst ("    kinds(old) " ^ (Display.string_of_kind_environment kdenv) ^ "\n") in    (* for debug *)
                    let _ = print_for_debug_subst ("    kinds(new) " ^ (Display.string_of_kind_environment newkdenv) ^ "\n") in (* for debug *)
                    let neweqnlst = replace_type_variable_in_equations eqntail tvid1 newtystr2 in
                    let newacctheta = add (replace_type_variable_in_subst acctheta tvid1 newtystr2) tvid1 newtystr2 in
                    let newacckdenv = Kindenv.add (Kindenv.replace_type_variable_in_kindenv acckdenv tvid1 newtystr2) tvid1 UniversalKind in
                      iter_complete newkdenv neweqnlst newacctheta newacckdenv
(*                  end *)

      | (_, TypeVariable(_)) -> iter_add [(tystr2, tystr1)]

      | _                    -> raise (InternalContradictionError(kdenv))


(* PUBLIC *)
let unify (kdenv : Kindenv.t) (tystr1 : mono_type) (tystr2 : mono_type) =
  let _ = print_for_debug_subst "  unify\n" in (* for debug *)
  try
    unify_sub kdenv [(tystr1, tystr2)] empty Kindenv.empty
  with
  | InternalInclusionError(kdenvsub)     -> report_inclusion_error kdenvsub tystr1 tystr2
  | InternalContradictionError(kdenvsub) -> report_contradiction_error kdenvsub tystr1 tystr2


(* for test *)
(* PUBLIC *)
let string_of_subst (theta : t) =
  let rec iter (theta : t) =
    match theta with
    | []                    -> ""
    | (tvid, tystr) :: tail ->
        " | '" ^ (Tyvarid.show_direct tvid) ^ " := " ^ (string_of_mono_type_basic tystr) ^ "\n"
          ^ (iter tail)
  in
      " +-------------------------------\n"
    ^ (iter theta)
    ^ " +-------------------------------\n"
