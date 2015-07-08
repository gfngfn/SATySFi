open Types
open Typeenv

let tvidmax : type_variable_id ref = ref 0

let new_type_variable_id () =
  let res = !tvidmax in tvidmax := !tvidmax + 1 ; res

let overwrite_range rng ty =
	match ty with
	| IntType(r)         -> IntType(rng)
	| StringType(r)      -> StringType(rng)
	| BoolType(r)        -> BoolType(rng)
	| UnitType(r)        -> UnitType(rng)
	| FuncType(r, d, c)  -> FuncType(rng, d, c)
	| ListType(r, c)     -> ListType(rng, c)
	| RefType(r, c)      -> RefType(rng, c)
	| TypeVariable(r, v) -> TypeVariable(rng, v)
	| ForallType(v, c)   -> ForallType(v, c)
	| TypeEnvironmentType(r, e) -> TypeEnvironmentType(rng, e)

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
  | other                       -> other

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
              ( print_for_debug ("  " ^ nv ^ " : " ^ (string_of_type_struct forallty) (* for test *)
              	  ^ "\n    = " ^ (string_of_type_struct ty) ^ "\n") ;                 (* for test *)
                (ContentOf(nv), (overwrite_range rng ty), Subst.empty)
              )
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
          let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
          let term_result = Apply(
                              Subst.apply_to_term theta_result e1,
                              Subst.apply_to_term theta_result e2
                            ) in
          let type_result = Subst.apply_to_type_struct theta_result beta in
          ( print_for_debug ("\n%Apply " ^ (string_of_ast term_result) ^ " : "
          	  ^ (string_of_type_struct beta) ^ " = " ^ (string_of_type_struct type_result) ^ "\n") ;
          	print_for_debug ((Subst.string_of_subst theta_result) ^ "\n") ; (* for test *)
            (term_result, type_result, theta_result)
          )

    | UTLambdaAbstract(varrng, varnm, utast1) ->
        let beta = TypeVariable(varrng, new_type_variable_id ()) in
        let tyenv_new = Typeenv.add tyenv varnm beta in
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
            let term_result = LambdaAbstract(varnm, e1) in
            let type_result = FuncType(rng, Subst.apply_to_type_struct theta1 beta, ty1) in
            let theta_result = theta1 in
              (term_result, type_result, theta_result)

    | UTLetIn(utmutletcons, utast2) ->
        let (tyenv_for_rec, tvtylst) = add_mutual_variables tyenv utmutletcons [] in
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv_for_rec utmutletcons tvtylst in
        let tyenv_forall = make_forall_type_mutual tyenv theta1 tvtylst in
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

    | _ -> raise (TypeCheckError(error_reporting rng "this cannot happen / remains to be implemented"))

(* Typeenv.t -> untyped_mutual_let_cons -> ((var_name * type_struct) list)
	-> (Typeenv.t * ((var_name * type_struct) list)) *)
and add_mutual_variables tyenv mutletcons tvtylst =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv, tvtylst)
  | UTMutualLetCons(varnm, _, tailcons) ->
      let ntv = TypeVariable((-1, 0, 0, 0), new_type_variable_id ()) in
        add_mutual_variables (Typeenv.add tyenv varnm ntv) tailcons ((varnm, ntv) :: tvtylst)

(* Typeenv.t -> Typeenv.t -> untyped_mutual_let_cons -> ((var_name * type_struct) list)
	-> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv mutletcons tvtylst =
  match (mutletcons, tvtylst) with
  | (UTEndOfMutualLet, []) ->
      (tyenv, EndOfMutualLet, Subst.empty)

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
    	print_for_debug (string_of_type_environment tyenv) ;
    	print_for_debug ("$ '" ^ varnm ^ " : " ^ (string_of_type_struct prety) ^ "\n") ;
      let forallty  = make_forall_type prety tyenv in
      let tyenv_new = Typeenv.add tyenv varnm forallty in
        make_forall_type_mutual tyenv_new theta tvtytail
    )


(* untyped_abstract_tree -> (string * type_environment) *)
let main tyenv utast =
  let (e, ty, theta) = typecheck tyenv utast in
  let strty = string_of_type_struct ty in
    match ty with
    | TypeEnvironmentType(_, newtyenv) -> (strty, newtyenv, e)
    | _                                -> (strty, tyenv, e)

let initialize () = ( tvidmax := 0 )
