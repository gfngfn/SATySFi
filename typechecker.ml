open Types
open Typeenv

let print_process msg =
(*
  print_string (msg ^ "\n") ;
*)
  ()

let tvidmax : type_variable_id ref = ref 0

let new_type_variable_id () =
  let res = !tvidmax in
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
      let ntvstr = TypeVariable((-2, 0, 0, 0), new_type_variable_id ()) in
        eliminate_forall tycont ((tvid, ntvstr) :: lst)
  | other -> replace_id other lst

and replace_id tystr lst =
  match tystr with
  | TypeVariable(rng, tvid)     ->
      ( try find_in_list lst tvid with Not_found -> TypeVariable(rng, tvid) )
  | ListType(rng, tycont)       -> ListType(rng, replace_id tycont lst)
  | FuncType(rng, tydom, tycod) -> FuncType(rng, replace_id tydom lst, replace_id tycod lst)
  | other                  -> other

let make_bounded_free tystr = eliminate_forall tystr []

(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct_with_id * Subst.t) *)
let rec typecheck tyenv utast =
  let (rng, utastmain) = utast in
    match utastmain with
    | UTStringEmpty         -> (StringEmpty,         StringType(rng), Subst.empty)
    | UTBreakAndIndent      -> (BreakAndIndent,      StringType(rng), Subst.empty)
    | UTNumericConstant(nc) -> (NumericConstant(nc), IntType(rng),    Subst.empty)
    | UTStringConstant(sc)  -> (StringConstant(sc),  StringType(rng), Subst.empty)
    | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType(rng),   Subst.empty)
    | UTUnitConstant        -> (UnitConstant,        UnitType(rng),   Subst.empty)
    | UTNoContent           -> (NoContent,           StringType(rng), Subst.empty)
    | UTFinishHeaderFile    -> (FinishHeaderFile,    TypeEnvironmentType(rng, tyenv), Subst.empty)
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
        let theta3 = Subst.unify ty1 (StringType(get_range utast1)) in
        let theta4 = Subst.unify ty2 (StringType(get_range utast2)) in
        let theta_result = Subst.compose theta4 (Subst.compose theta3 (Subst.compose theta2 theta1)) in
          (Concat(e1, e2), StringType(rng), theta_result)

    | UTApply(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let beta = TypeVariable(rng, new_type_variable_id ()) in
        let theta3 = Subst.unify (FuncType(get_range utast1, ty2, beta)) ty1 in
          let term_result = Apply(
                              Subst.apply_to_term (Subst.compose theta3 theta1) e1,
                              Subst.apply_to_term theta3 e2
                            ) in
          let type_result = Subst.apply_to_type_struct theta3 beta in
          let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
            (term_result, type_result, theta_result)

    | UTLambdaAbstract(varrng, varnm, utast1) ->
        let beta = TypeVariable(varrng, new_type_variable_id ()) in
        let tyenv_new = Typeenv.add tyenv varnm beta in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
            let term_result = LambdaAbstract(varnm, e1) in
            let type_result = FuncType(rng, Subst.apply_to_type_struct theta1 beta, ty1) in
            let theta_result = theta1 in
              (term_result, type_result, theta_result)

    | UTLetIn(utmutletcons, utast2) ->
        let tyenv_for_rec = add_mutual_variables tyenv utmutletcons in
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv tyenv_for_rec utmutletcons in
        let (e2, ty2, theta2) = typecheck tyenv_new utast2 in
          (LetIn(mutletcons, e2), ty2, Subst.compose theta2 theta1)

    | UTLetMutableIn(varrng, varnm, utastdflt, utastaft) ->
        let (edflt, tydflt, thetadflt) = typecheck tyenv utastdflt in
        let tyenv_new = Subst.apply_to_type_environment thetadflt (Typeenv.add tyenv varnm (RefType(varrng, tydflt))) in
          let (eaft, tyaft, thetaaft) = typecheck tyenv_new utastaft in
            let theta_result = Subst.compose thetaaft thetadflt in
            let term_result = LetMutableIn(varnm, Subst.apply_to_term theta_result edflt,
                                                  Subst.apply_to_term theta_result eaft) in
            let type_result = Subst.apply_to_type_struct theta_result tyaft in
              (term_result, type_result, theta_result)

    | UTDeclareGlobalHash(utastkey, utastdflt) ->
        let (ekey, tykey, thetakey)    = typecheck tyenv utastkey in
        let (edflt, tydflt, thetadflt) = typecheck tyenv utastdflt in
        let thetasubkey  = Subst.unify tykey  (StringType(get_range utastkey))  in
        let thetasubdflt = Subst.unify tydflt (StringType(get_range utastdflt)) in
        let theta_result =  Subst.compose thetasubdflt (
        	                    Subst.compose thetasubkey (
        	                      Subst.compose thetadflt thetakey)) in
        let term_result  = DeclareGlobalHash( Subst.apply_to_term theta_result ekey,
                                              Subst.apply_to_term theta_result edflt) in
          (term_result, UnitType(rng), theta_result)

    | UTOverwriteGlobalHash(utastkey, utastnew) ->
        let (ekey, tykey, thetakey) = typecheck tyenv utastkey in
        let (enew, tynew, thetanew) = typecheck tyenv utastnew in
        let thetasubkey = Subst.unify tykey (StringType(get_range utastkey)) in
        let thetasubnew = Subst.unify tynew (StringType(get_range utastnew)) in
        let theta_result =  Subst.compose thetasubnew (
        	                    Subst.compose thetasubkey (
        	                      Subst.compose thetanew thetakey)) in
        let term_result  = OverwriteGlobalHash( Subst.apply_to_term theta_result ekey,
                                                Subst.apply_to_term theta_result enew) in
          (term_result, UnitType(rng), theta_result)

    | UTOverwrite(varrng, varnm, utastnew) ->
        let (_, tyvar, _) = typecheck tyenv (varrng, UTContentOf(varnm)) in
        let (enew, tynew, thetanew) = typecheck tyenv utastnew in
        let thetasub = Subst.unify tyvar (RefType(get_range utastnew, tynew)) in
            (*  actually 'get_range astnew' is not good
                since the right side expression has type 't, not 't ref *)
          (Overwrite(varnm, enew), UnitType(rng), Subst.compose thetasub thetanew)

    | UTSequential(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let theta_new = Subst.compose (Subst.unify ty1 (UnitType((-128, 0, 0, 0)))) theta1 in
        let tyenv_new = Subst.apply_to_type_environment theta_new tyenv in
        let (e2, ty2, theta2) = typecheck tyenv_new utast2 in
          let theta_result = Subst.compose theta2 theta_new in
          let type_result = Subst.apply_to_type_struct theta_result ty2 in
          let term_result = Sequential(Subst.apply_to_term theta_result e1,
          	                           Subst.apply_to_term theta_result e2) in
            (term_result, type_result, theta_result)

    | UTReferenceFinal(utast1) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let thetasub = Subst.unify ty1 (StringType(rng)) in
        let theta_result = Subst.compose thetasub theta1 in
        let term_result = ReferenceFinal(Subst.apply_to_term theta_result e1) in
          (term_result, StringType(rng), theta_result)

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
        let tyenv_new = Typeenv.add tyenv "class" (StringType((-6, 0, 0, 0))) in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
          let (e2, ty2, theta2) = typecheck tyenv utast2 in
          let theta_result = Subst.compose (Subst.unify ty1 ty2) (Subst.compose theta2 theta1) in
          let term_result = IfClassIsValid(Subst.apply_to_term theta_result e1, Subst.apply_to_term theta_result e2) in
          let type_result = Subst.apply_to_type_struct theta_result ty1 in
            (term_result, type_result, theta_result)

    | UTIfIDIsValid(utast1, utast2) ->
        let tyenv_new = Typeenv.add tyenv "id" (StringType((-7, 0, 0, 0))) in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
          let (e2, ty2, theta2) = typecheck tyenv utast2 in
          let theta_result = Subst.compose (Subst.unify ty1 ty2) (Subst.compose theta2 theta1) in
          let term_result = IfIDIsValid(Subst.apply_to_term theta_result e1, Subst.apply_to_term theta_result e2) in
          let type_result = Subst.apply_to_type_struct theta_result ty1 in
            (term_result, type_result, theta_result)

    | UTApplyClassAndID(utastcls, utastid, utast1) ->
        let (ecls, _, _) = typecheck tyenv utastcls in
        let (eid, _, _)  = typecheck tyenv utastid in
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
          (ApplyClassAndID(ecls, eid, e1), ty1, theta1)
  
    | UTListCons(utasthd, utasttl) ->
        let (ehd, tyhd, thetahd) = typecheck tyenv utasthd in
        let (etl, tytl, thetatl) = typecheck tyenv utasttl in
          let theta_result = Subst.compose thetatl thetahd in
          let type_result = ListType(rng, Subst.apply_to_type_struct theta_result tyhd) in
          let term_result = ListCons(
                              Subst.apply_to_term theta_result ehd,
                              Subst.apply_to_term theta_result etl) in
            (term_result, type_result, theta_result)

    | UTEndOfList ->
        let ntyvar = TypeVariable(rng, new_type_variable_id ()) in
          (EndOfList, ListType(rng, ntyvar), Subst.empty)

    | _ -> raise (TypeCheckError(error_reporting rng "this cannot happen / remains to be implemented"))

(* Typeenv.t -> untyped_mutual_let_cons -> Typeenv.t *)
and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> tyenv
  | UTMutualLetCons(nv, _, tailcons) ->
      let ntv = TypeVariable((-1, 0, 0, 0), new_type_variable_id ()) in
        add_mutual_variables (Typeenv.add tyenv nv ntv) tailcons

(* Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv tyenv_for_rec mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv_for_rec, EndOfMutualLet, Subst.empty)
  | UTMutualLetCons(nv, utast1, tailcons) ->
      let (e1, ty1, theta1) = typecheck tyenv_for_rec utast1 in
        let forallty = make_forall_type ty1 (Subst.apply_to_type_environment theta1 tyenv) in
        let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1 tyenv_for_rec) nv forallty in
        let (tyenv_tail, mutletcons_tail, theta_tail) = typecheck_mutual_contents tyenv tyenv_new tailcons in
          (tyenv_tail, MutualLetCons(nv, e1, mutletcons_tail), Subst.compose theta_tail theta1)

(* untyped_abstract_tree -> (string * type_environment) *)
let main tyenv utast =
  let (e, ty, theta) = typecheck tyenv utast in
  let strty = string_of_type_struct ty in
    match ty with
    | TypeEnvironmentType(_, newtyenv) -> (strty, newtyenv, e)
    | _                                -> (strty, tyenv, e)

let initialize () = ( tvidmax := 0 )
