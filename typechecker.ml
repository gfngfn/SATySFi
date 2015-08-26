open Types
open Typeenv

let tvidmax : type_variable_id ref = ref 0

let new_type_variable_id () =
  let res = !tvidmax in tvidmax := !tvidmax + 1 ; res

let rec find_in_list lst elm =
  match lst with
  | []                    -> raise Not_found
  | (tvid, tystr) :: tail -> if tvid == elm then tystr else find_in_list tail elm


let rec make_bounded_free tystr = eliminate_forall tystr []
and eliminate_forall tystr lst =
  match tystr with
  | ForallType(tvid, tycont) ->
      let ntvstr = TypeVariable((-2, 0, 0, 0), new_type_variable_id ()) in
        eliminate_forall tycont ((tvid, ntvstr) :: lst)
  | other -> replace_id other lst

(* type_struct -> type_variable_id list -> type_struct *)
and replace_id tystr lst =
  match tystr with
  | TypeVariable(rng, tvid)     ->
      ( try find_in_list lst tvid with Not_found -> TypeVariable(rng, tvid) )
  | ListType(rng, tycont)       -> ListType(rng, replace_id tycont lst)
  | RefType(rng, tycont)        -> RefType(rng, replace_id tycont lst)
  | ProductType(rng, tylist)    -> ProductType(rng, replace_id_list tylist lst)
  | FuncType(rng, tydom, tycod) -> FuncType(rng, replace_id tydom lst, replace_id tycod lst)
  | other                       -> other

and replace_id_list tylist lst =
  match tylist with
  | []           -> []
  | head :: tail -> (replace_id head lst) :: (replace_id_list tail lst)

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
            let ty = overwrite_range_of_type (make_bounded_free forallty) rng in
              ( print_for_debug ("#C " ^ nv ^ " : " ^ (string_of_type_struct_basic forallty) (* for debug *)
                  ^ " = " ^ (string_of_type_struct_basic ty) ^ "\n") ;                       (* for debug *)
                (ContentOf(nv), ty, Subst.empty)
              )                                                                              (* for debug *)
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
        ( match ty1 with
          | FuncType(_, tydom, tycod) ->
              let theta3 = Subst.unify ty2 tydom in
                let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let term_result = Apply(
                                    Subst.apply_to_term theta_result e1,
                                    Subst.apply_to_term theta_result e2
                                  ) in
                let type_result = Subst.apply_to_type_struct theta_result tycod in
                ( print_for_debug ("\n%Apply1 " ^ (string_of_ast term_result) ^ " : " (* for debug *)
                    ^ (string_of_type_struct_basic type_result) ^ "\n") ;             (* for debug *)
                  print_for_debug ((Subst.string_of_subst theta_result) ^ "\n") ;     (* for debug *)
                    (term_result, type_result, theta_result)
                )                                                                     (* for debug *)
          | _ ->
              let beta = TypeVariable(rng, new_type_variable_id ()) in
              let theta3 = Subst.unify ty1 (FuncType(get_range utast1, ty2, beta)) in
                let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let term_result = Apply(
                                    Subst.apply_to_term theta_result e1,
                                    Subst.apply_to_term theta_result e2
                                  ) in
                let type_result = Subst.apply_to_type_struct theta_result beta in
                ( print_for_debug ("\n%Apply2 " ^ (string_of_ast term_result) ^ " : " (* for debug *)
                    ^ (string_of_type_struct_basic beta) ^ " = "                      (* for debug *)
                    ^ (string_of_type_struct_basic type_result) ^ "\n") ;             (* for debug *)
                  print_for_debug ((Subst.string_of_subst theta_result) ^ "\n") ;     (* for debug *)
                    (term_result, type_result, theta_result)
                )                                                                     (* for debug *)
        )

    | UTLambdaAbstract(varrng, varnm, utast1) ->
        let beta = TypeVariable(varrng, new_type_variable_id ()) in
        let tyenv_new = Typeenv.add tyenv varnm beta in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
            let term_result = LambdaAbstract(varnm, e1) in
            let tydom = (Subst.apply_to_type_struct theta1 beta) in
            let tycod = ty1 in
            let type_result = FuncType(rng, tydom, tycod) in
            let theta_result = theta1 in
              (term_result, type_result, theta_result)

    | UTLetIn(utmutletcons, utast2) ->
        let (tyenv_for_rec, tvtylst) = add_mutual_variables tyenv utmutletcons in
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv_for_rec utmutletcons tvtylst in
        let tyenv_forall = make_forall_type_mutual tyenv theta1 tvtylst in (* contains bugs *)
        let (e2, ty2, theta2) = typecheck tyenv_forall utast2 in
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
        let theta_new = Subst.compose (Subst.unify ty1 (UnitType(get_range utast1))) theta1 in
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
                              (Subst.unify ty2 ty1)
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
          let theta_result = Subst.compose (Subst.unify ty2 ty1) (Subst.compose theta2 theta1) in
          let term_result = IfClassIsValid(Subst.apply_to_term theta_result e1, Subst.apply_to_term theta_result e2) in
          let type_result = Subst.apply_to_type_struct theta_result ty1 in
            (term_result, type_result, theta_result)

    | UTIfIDIsValid(utast1, utast2) ->
        let tyenv_new = Typeenv.add tyenv "id" (StringType((-7, 0, 0, 0))) in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
          let (e2, ty2, theta2) = typecheck tyenv utast2 in
          let theta_result = Subst.compose (Subst.unify ty2 ty1) (Subst.compose theta2 theta1) in
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

    | UTWhileDo(utastb, utastc) ->
        let (eb, tyb, thetab) = typecheck tyenv utastb in
        let (ec, tyc, thetac) = typecheck tyenv utastc in
          let thetabsub = Subst.unify tyb (BoolType(get_range utastb)) in
          let thetacsub = Subst.unify tyc (UnitType(get_range utastc)) in
            let theta_result =  Subst.compose thetacsub
                                  (Subst.compose thetabsub
                                    (Subst.compose thetac thetab)) in
            let term_result = WhileDo(Subst.apply_to_term theta_result eb,
                                      Subst.apply_to_term theta_result ec) in
              (term_result, UnitType(rng), theta_result)

    | UTTupleCons(utasthd, utasttl) ->
        let (ehd, tyhd, thetahd) = typecheck tyenv utasthd in
        let (etl, tytl, thetatl) = typecheck tyenv utasttl in
        let theta_result = Subst.compose thetatl thetahd in
        let term_result = Subst.apply_to_term theta_result (TupleCons(ehd, etl)) in
        let type_result = Subst.apply_to_type_struct theta_result
          ( match tytl with
            | ProductType(rngtl, tylist) -> ProductType(rng, tyhd :: tylist)
            | _ -> raise (TypeCheckError("this cannot happen: illegal type for tuple"))
          )
        in
          (term_result, type_result, theta_result)

    | UTEndOfTuple -> (EndOfTuple, ProductType(rng, []), Subst.empty)

    | UTPatternMatch(utastobj, utpmcons) ->
        let (eobj, tyobj, thetaobj) = typecheck tyenv utastobj in
        let ntv = TypeVariable((-300, 0, 0, 0), new_type_variable_id ()) in
        let (pmcons, typm, thetapm) = typecheck_pattern_match_cons tyenv utpmcons tyobj thetaobj ntv in
          (PatternMatch(eobj, pmcons), typm, thetapm)
(*
    | _ -> raise (TypeCheckError(error_reporting rng "this cannot happen / remains to be implemented"))
*)

(* type_environment -> untyped_pattern_match_cons -> type_struct -> Subst.t -> type_struct
	-> (pattern_match_cons * type_struct * Subst.t) *)
and typecheck_pattern_match_cons tyenv utpmcons tyobj theta tyres =
  let (rng, utpmconsmain) = utpmcons in
    match utpmconsmain with
    | UTEndOfPatternMatch -> (EndOfPatternMatch, (Subst.apply_to_type_struct theta tyres), theta)
    | UTPatternMatchCons(utpat, utast1, tailcons) ->
        let (epat, typat, tyenvpat) = typecheck_pattern tyenv utpat in
        let thetapat = Subst.compose (Subst.unify tyobj typat) theta in
        let tyenv1 = Subst.apply_to_type_environment thetapat tyenvpat in
        let (e1, ty1, theta1) = typecheck tyenv1 utast1 in
        let theta2 = Subst.compose (Subst.unify ty1 tyres) (Subst.compose theta1 thetapat) in
        let tyres_new = Subst.apply_to_type_struct theta2 tyres in
        let (pmctl, tytl, thetatl) = typecheck_pattern_match_cons tyenv tailcons tyobj theta2 tyres_new in
          (PatternMatchCons(epat, e1, pmctl), tytl, thetatl)

(* type_environment * untyped_pattern_tree -> (pattern_tree * type_struct * type_environment) *)
and typecheck_pattern tyenv utpat =
  let (rng, utpatmain) = utpat in
    match utpatmain with
    | UTPNumericConstant(nc) -> (PNumericConstant(nc), IntType(rng), tyenv)
    | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), BoolType(rng), tyenv)
    | UTPUnitConstant        -> (PUnitConstant, UnitType(rng), tyenv)
    | UTPEndOfList ->
        let ntv = TypeVariable(rng, new_type_variable_id ()) in (PEndOfList, ListType(rng, ntv), tyenv)
    | UTPListCons(utpat1, utpat2) ->
        let (epat1, typat1, tyenv1) = typecheck_pattern tyenv utpat1 in
        let (epat2, typat2, tyenv2) = typecheck_pattern tyenv1 utpat2 in
          let theta = Subst.unify (ListType((-200, 0, 0, 0), typat1)) typat2 in
          let tyenv_result = Subst.apply_to_type_environment theta tyenv2 in
          let type_result = Subst.apply_to_type_struct theta typat2 in
            (PListCons(epat1, epat2), type_result, tyenv_result)

    | UTPEndOfTuple -> (PEndOfTuple, ProductType(rng, []), tyenv)
    | UTPTupleCons(utpat1, utpat2) ->
        let (epat1, typat1, tyenv1) = typecheck_pattern tyenv utpat1 in
        let (epat2, typat2, tyenv2) = typecheck_pattern tyenv1 utpat2 in
        let type_result =
          ( match typat2 with
            | ProductType(_, tylist) -> ProductType(rng, typat1 :: tylist)
            | _ -> raise (TypeCheckError("this cannot happen: illegal tuple in pattern"))
          )
        in
          (PTupleCons(epat1, epat2), type_result, tyenv2)
    | UTPWildCard ->
        let ntv = TypeVariable(rng, new_type_variable_id ()) in (PWildCard, ntv, tyenv)
    | UTPVariable(varnm) ->
        let ntv = TypeVariable(rng, new_type_variable_id ()) in
          (PVariable(varnm), ntv, Typeenv.add tyenv varnm ntv)


(* Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * ((var_name * type_struct) list)) *)
and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv, [])
  | UTMutualLetCons(varnm, astdef, tailcons) ->
      let ntv = TypeVariable(get_range astdef, new_type_variable_id ()) in
        let (tyenv_tail, tvtylst) = add_mutual_variables (Typeenv.add tyenv varnm ntv) tailcons in
          (tyenv_tail, ((varnm, ntv) :: tvtylst))

(* Typeenv.t -> Typeenv.t -> untyped_mutual_let_cons -> ((var_name * type_struct) list)
  -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv mutletcons tvtylst =
  match (mutletcons, tvtylst) with
  | (UTEndOfMutualLet, []) -> (tyenv, EndOfMutualLet, Subst.empty)

  | (UTMutualLetCons(nv, utast1, tailcons), (_, tvty) :: tvtytail) ->
      let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let theta1new = Subst.compose (Subst.unify ty1 tvty) theta1 in
        let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1new tyenv) nv ty1 in
        let (tyenv_tail, mutletcons_tail, theta_tail) = typecheck_mutual_contents tyenv_new tailcons tvtytail in
        let theta1final = Subst.compose theta_tail theta1new in
          (Subst.apply_to_type_environment theta1final tyenv_tail, MutualLetCons(nv, e1, mutletcons_tail), theta1final)

  | _ -> raise (TypeCheckError("this cannot happen: error in 'typecheck_mutual_contents'"))

and make_forall_type_mutual tyenv theta tvtylst =
  match tvtylst with
  | []                        -> tyenv
  | (varnm, tvty) :: tvtytail ->
      let prety = Subst.apply_to_type_struct theta tvty in
    ( print_for_debug (Subst.string_of_subst theta) ;
      print_for_debug (string_of_type_environment tyenv "MakeForall") ;
      print_for_debug ("#M " ^ varnm ^ " : " ^ (string_of_type_struct_basic prety) ^ "\n") ;
      let forallty  = make_forall_type prety tyenv in
      let tyenv_new = Typeenv.add tyenv varnm (erase_range_of_type forallty) in
        make_forall_type_mutual tyenv_new theta tvtytail
    )


(* untyped_abstract_tree -> (type_struct * type_environment) *)
let main tyenv utast =
  let (e, ty, theta) = typecheck tyenv utast in
    match ty with
    | TypeEnvironmentType(_, newtyenv) -> (ty, newtyenv, e)
    | _                                -> (ty, tyenv, e)

let initialize () = ( tvidmax := 0 )
