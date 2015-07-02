open Types
open Typeenv

let print_process msg =
(*
  print_string (msg ^ "\n") ;
*)
  ()

let error_reporting rng errmsg =
  let (sttln, sttpos, endln, endpos) = rng in
    if sttln == endln then
      errmsg ^ " (line " ^ (string_of_int sttln) ^ ", characters "
        ^ (string_of_int sttpos) ^ "-" ^ (string_of_int endpos) ^ ")"
    else
      errmsg ^ " (line " ^ (string_of_int sttln) ^ ", character " ^ (string_of_int sttpos)
        ^ " to line " ^ (string_of_int endln) ^ ", character " ^ (string_of_int endpos) ^ ")"

let tvidmax : type_variable_id ref = ref 0

let new_type_variable () =
  let res = TypeVariable(!tvidmax) in
  ( print_process ("  *make '" ^ (string_of_int !tvidmax)) ;
    tvidmax := !tvidmax + 1 ;
    res
  )

let rec find_in_list lst elm =
  match lst with
  | []                    -> raise Not_found
  | (tvid, tystr) :: tail -> if tvid == elm then tystr else find_in_list tail elm

let rec eliminate_forall tystr lst =
  match tystr with
  | ForallType(tvid, tycont) ->
      let ntvstr = new_type_variable () in
        eliminate_forall tycont ((tvid, ntvstr) :: lst)
  | other -> replace_id other lst

and replace_id tystr lst =
  match tystr with
  | TypeVariable(tvid)     ->
      ( try find_in_list lst tvid with Not_found -> TypeVariable(tvid) )
  | ListType(tycont)       -> ListType(replace_id tycont lst)
  | FuncType(tydom, tycod) -> FuncType(replace_id tydom lst, replace_id tycod lst)
  | other                  -> other

let make_bounded_free tystr = eliminate_forall tystr []

(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct * Subst.t) *)
let rec typecheck tyenv utast =
  let (rng, utastmain) = utast in
    match utastmain with
    | UTStringEmpty         -> (StringEmpty, StringType, Subst.empty)
    | UTBreakAndIndent      -> (BreakAndIndent, StringType, Subst.empty)
    | UTNumericConstant(nc) -> (NumericConstant(nc), IntType, Subst.empty)
    | UTStringConstant(sc)  -> (StringConstant(sc), StringType, Subst.empty)
    | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType, Subst.empty)
    | UTUnitConstant        -> (UnitConstant, UnitType, Subst.empty)
    | UTFinishHeaderFile    -> (FinishHeaderFile, TypeEnvironmentType(tyenv), Subst.empty)
    | UTNoContent           -> (NoContent, StringType, Subst.empty)
    | UTContentOf(nv) ->
        ( try
            let forallty = Typeenv.find tyenv nv in
            let ty = make_bounded_free forallty in
              (ContentOf(nv), ty, Subst.empty)
          with
          | Not_found -> raise (TypeCheckError(error_reporting rng ("undefined variable '" ^ nv ^ "'")))
        )

    | UTConcat(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let theta3 = Subst.unify ty1 StringType in
        let theta4 = Subst.unify ty2 StringType in
        let theta_result = Subst.compose theta4 (Subst.compose theta3 (Subst.compose theta2 theta1)) in
          (Concat(e1, e2), StringType, theta_result)

    | UTApply(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let beta = new_type_variable () in
        let theta3 = Subst.unify (FuncType(ty2, beta)) ty1 in
          let term_result = Apply(
                              Subst.apply_to_term (Subst.compose theta3 theta1) e1,
                              Subst.apply_to_term theta3 e2
                            ) in
          let type_result = Subst.apply_to_type_struct theta3 beta in
          let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
            (term_result, type_result, theta_result)

    | UTLambdaAbstract(varnm, utast1) ->
        let beta = new_type_variable () in
        let tyenv_new = Typeenv.add tyenv varnm beta in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
            let term_result = LambdaAbstract(varnm, e1) in
            let type_result = FuncType(Subst.apply_to_type_struct theta1 beta, ty1) in
            let theta_result = theta1 in
              (term_result, type_result, theta_result)

    | UTLetIn(utmutletcons, utast2) ->
        let tyenv_for_rec = add_mutual_variables tyenv utmutletcons in
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv tyenv_for_rec utmutletcons in
        let (e2, ty2, theta2) = typecheck tyenv_new utast2 in
          (LetIn(mutletcons, e2), ty2, Subst.compose theta2 theta1)

    | UTIfThenElse(utastb, utast1, utast2) ->
        let (eb, tyb, thetab) = typecheck tyenv utastb in
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let theta_result =  Subst.compose
                              (Subst.unify ty1 ty2)
                              (Subst.compose theta2 (Subst.compose theta1 thetab)) in
        let term_result = IfThenElse(
                            Subst.apply_to_term theta_result eb,
                            Subst.apply_to_term theta_result e1,
                            Subst.apply_to_term theta_result e2) in
          (term_result, Subst.apply_to_type_struct theta_result ty1, theta_result)

    | UTIfClassIsValid(utast1, utast2) ->
        let tyenv_new = Typeenv.add tyenv "class" StringType in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
          let (e2, ty2, theta2) = typecheck tyenv utast2 in
          let theta_result = Subst.compose (Subst.unify ty1 ty2) (Subst.compose theta2 theta1) in
          let term_result = IfClassIsValid(Subst.apply_to_term theta_result e1, Subst.apply_to_term theta_result e2) in
          let type_result = Subst.apply_to_type_struct theta_result ty1 in
            (term_result, type_result, theta_result)

    | UTIfIDIsValid(utast1, utast2) ->
        let tyenv_new = Typeenv.add tyenv "id" StringType in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
          let (e2, ty2, theta2) = typecheck tyenv utast2 in
          let theta_result = Subst.compose (Subst.unify ty1 ty2) (Subst.compose theta2 theta1) in
          let term_result = IfIDIsValid(Subst.apply_to_term theta_result e1, Subst.apply_to_term theta_result e2) in
          let type_result = Subst.apply_to_type_struct theta_result ty1 in
            (term_result, type_result, theta_result)

    | UTApplyClassAndID(_, _, utast1) -> typecheck tyenv utast1
  
    | UTListCons(utasthd, utasttl) ->
        let (ehd, tyhd, thetahd) = typecheck tyenv utasthd in
        let (etl, tytl, thetatl) = typecheck tyenv utasttl in
          let theta_result = Subst.compose thetatl thetahd in
          let type_result = ListType(Subst.apply_to_type_struct theta_result tyhd) in
          let term_result = ListCons(
                              Subst.apply_to_term theta_result ehd,
                              Subst.apply_to_term theta_result etl) in
            (term_result, type_result, theta_result)

    | UTEndOfList -> let ntyvar = new_type_variable () in (EndOfList, ListType(ntyvar), Subst.empty)
(*  
    | UTLetMutableIn(varnm, astdflt, astaft) ->
        let tydflt = typecheck tyenv astdflt in
        let tyenv_new = Typeenv.copy tyenv in
        ( Typeenv.add tyenv varnm tydflt ;
          let tyaft = typecheck tyenv_new astaft in tyaft
        )
 
    | UTOverwrite(varnm, astnew) ->
        let _ = typecheck tyenv astnew in UnitType
*)  
    | _ -> raise (TypeCheckError(error_reporting rng "this cannot happen / remains to be implemented"))

(* Typeenv.t -> untyped_mutual_let_cons -> Typeenv.t *)
and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> tyenv
  | UTMutualLetCons(nv, _, tailcons) ->
      let ntv = new_type_variable () in
        add_mutual_variables (Typeenv.add tyenv nv ntv) tailcons

(* Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv tyenv_for_rec mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv_for_rec, EndOfMutualLet, Subst.empty)
  | UTMutualLetCons(nv, utast1, tailcons) ->
      let (e1, ty1, theta1) = typecheck tyenv_for_rec utast1 in
      let ntystr = Typeenv.find tyenv_for_rec nv in (* for self recursion *)
      ( match ntystr with
        | TypeVariable(ntvid) ->
            let theta1_new = Subst.add theta1 ntvid ty1 in
            let forallty = make_forall_type ty1 (Subst.apply_to_type_environment theta1_new tyenv) in
            let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1_new tyenv_for_rec) nv forallty in
            let (tyenv_tail, mutletcons_tail, theta_tail) = typecheck_mutual_contents tyenv tyenv_new tailcons in
              (tyenv_tail, MutualLetCons(nv, e1, mutletcons_tail), Subst.compose theta_tail theta1_new)

        | _ -> raise (TypeCheckError("this cannot happen. (at typecheck_mutual_contents)"))
      )

(* untyped_abstract_tree -> (string * type_environment) *)
let main tyenv utast =
  let (e, ty, theta) = typecheck tyenv utast in
  let strty = string_of_type_struct ty in
    match ty with
    | TypeEnvironmentType(newtyenv) -> (strty, newtyenv, e)
    | _                             -> (strty, tyenv, e)

let initialize () = ( tvidmax := 0 )
