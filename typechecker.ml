open Types
open Typeenv

exception TypeCheckError of string

let print_process msg =
(*
  print_string (msg ^ "\n") ;
*)
  ()

let rec string_of_type_struct tystr =
  match tystr with
  | TypeEnvironmentType(_) -> "env"
  | UnitType   -> "unit"
  | IntType    -> "int"
  | StringType -> "string"
  | BoolType   -> "bool"
  | FuncType(tyf, tyl) -> "(" ^ (string_of_type_struct tyf) ^ " -> " ^ (string_of_type_struct tyl) ^ ")"
  | ListType(ty)       -> "(" ^ (string_of_type_struct ty) ^ " list)"
  | TypeVariable(tvid) -> "'" ^ (string_of_int tvid) ^ ""
  | ForallType(tvid, tycont) -> "('" ^ (string_of_int tvid) ^ ". " ^ (string_of_type_struct tycont) ^ ")"


let tvidmax : type_variable_id ref = ref 0

let new_type_variable () =
  let res = TypeVariable(!tvidmax) in
  ( print_process ("  *make '" ^ (string_of_int !tvidmax)) ;
    tvidmax := !tvidmax + 1 ;
    res
  )

(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct * Subst.t) *)
let rec typecheck tyenv utast =
  let (rng, utastmain) = utast in
    match utastmain with
    | UTStringEmpty         -> (StringEmpty, StringType, Subst.empty)
    | UTBreakAndIndent      -> (BreakAndIndent, StringType, Subst.empty)
    | UTNumericConstant(nc) -> (NumericConstant(nc), IntType, Subst.empty)
    | UTStringConstant(sc)  -> (StringConstant(sc), StringType, Subst.empty)
    | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType, Subst.empty)
    | UTFinishHeaderFile    -> (FinishHeaderFile, TypeEnvironmentType(tyenv), Subst.empty)
    | UTNoContent           -> (NoContent, StringType, Subst.empty)
    | UTContentOf(nv) ->
        ( try
            let ty = Typeenv.find tyenv nv in
              (ContentOf(nv), ty, Subst.empty)
          with
          | Not_found -> raise (TypeCheckError("undefined variable '" ^ nv ^ "'"))
        )
(*
    | UTConcat(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let theta3 = Subst.unify ty1 StringType in
        let theta4 = Subst.unify ty2 StringType in
          let term_result = 
*)
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
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv_for_rec utmutletcons in
        let (e2, ty2, theta2) = typecheck tyenv_new utast2 in
          (LetIn(mutletcons, e2), ty2, Subst.compose theta2 theta1)
(*
    | UTIfThenElse(astb, asttru, astfls) ->
        let tyb = typecheck tyenv astb in
        let tytru = typecheck tyenv asttru in
        let tyfls = typecheck tyenv astfls in
        ( ( if equivalent BoolType tyb then () else Stacklist.push tyeq (BoolType, tyb) ) ;
          ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
          tytru
        )
    | UTIfClassIsValid(asttru, astfls) ->
        let tyenv_class = Typeenv.add tyenv "class" StringType in
          let tytru = typecheck tyenv_class asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTIfIDIsValid(asttru, astfls) ->
        let tyenv_id = Typeenv.add tyenv_id "id" StringType in
          let tytru = typecheck tyenv_id asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTApplyClassAndID(_, _, astf) -> typecheck tyenv astf
  
    | UTListCons(asthd, asttl) ->
        let tyhd = typecheck tyenv asthd in
        let tytl = typecheck tyenv asttl in
        ( ( if equivalent (ListType(tyhd)) tytl then () else Stacklist.push tyeq (ListType(tyhd), tytl) ) ;
          ListType(tyhd)
        )
    | UTEndOfList -> let ntyvar = new_type_variable "*empty" in ListType(ntyvar)
  
    | UTLetMutableIn(varnm, astdflt, astaft) ->
        let tydflt = typecheck tyenv astdflt in
        let tyenv_new = Typeenv.copy tyenv in
        ( Typeenv.add tyenv varnm tydflt ;
          let tyaft = typecheck tyenv_new astaft in tyaft
        )
  
    | UTSequential(astf, astl) ->
        let tyf = typecheck tyenv astf in
        let tyl = typecheck tyenv astl in
        ( ( if equivalent UnitType tyf then () else Stacklist.push tyeq (UnitType, tyf)) ;
          tyl
        )
    | UTOverwrite(varnm, astnew) ->
        let _ = typecheck tyenv astnew in UnitType
*)  
    | _ -> raise (TypeCheckError("this cannot happen / remains to be implemented"))

(* Typeenv.t -> untyped_mutual_let_cons -> Typeenv.t *)
and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> tyenv
  | UTMutualLetCons(nv, _, tailcons) ->
      let ntv = new_type_variable () in
        add_mutual_variables (Typeenv.add tyenv nv ntv) tailcons

(* Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv, EndOfMutualLet, Subst.empty)
  | UTMutualLetCons(nv, utast1, tailcons) ->
      let (e1, ty1, theta1) = typecheck tyenv utast1 in
      let ntystr = Typeenv.find tyenv nv in (* for self recursion *)
      ( match ntystr with
        | TypeVariable(ntvid) ->
            let theta1_new = Subst.add theta1 ntvid ty1 in
            let tyenv_new_pre = Subst.apply_to_type_environment theta1_new tyenv in
            let forallty = make_forall_type ty1 tyenv_new_pre in
            let tyenv_new = Typeenv.add tyenv_new_pre nv forallty in
            let (tyenv_tail, mutletcons_tail, theta_tail) = typecheck_mutual_contents tyenv_new tailcons in
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
