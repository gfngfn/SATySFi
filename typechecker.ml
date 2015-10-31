open Types
open Display

(* !! mutable !! *)
let tvidmax : type_variable_id ref = ref 0
let final_tyenv : Typeenv.t ref       = ref Typeenv.empty
let final_varntenv : Variantenv.t ref = ref Variantenv.empty

let initialize () = ( tvidmax := 0 )

let new_type_variable_id () =
  let res = !tvidmax in tvidmax := !tvidmax + 1 ; res


(* 'a -> ('a * 'b) list -> 'b *)
let rec find_id_in_list elm lst =
  match lst with
  | []                    -> raise Not_found
  | (tvid, tystr) :: tail -> if tvid = elm then tystr else find_id_in_list elm tail


(* type_struct -> type_struct *)
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
      ( try find_id_in_list tvid lst with Not_found -> TypeVariable(rng, tvid) )
  | ListType(rng, tycont)       -> ListType(rng, replace_id tycont lst)
  | RefType(rng, tycont)        -> RefType(rng, replace_id tycont lst)
  | ProductType(rng, tylist)    -> ProductType(rng, List.map (fun tl -> replace_id tl lst) tylist)
  | FuncType(rng, tydom, tycod) -> FuncType(rng, replace_id tydom lst, replace_id tycod lst)
  | other                       -> other


(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct_with_id * Subst.t) *)
let rec typecheck varntenv tyenv (rng, utastmain) =
  match utastmain with
  | UTStringEmpty         -> (StringEmpty,         StringType(rng), Subst.empty)
  | UTBreakAndIndent      -> (BreakAndIndent,      StringType(rng), Subst.empty)
  | UTNumericConstant(nc) -> (NumericConstant(nc), IntType(rng),    Subst.empty)
  | UTStringConstant(sc)  -> (StringConstant(sc),  StringType(rng), Subst.empty)
  | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType(rng),   Subst.empty)
  | UTUnitConstant        -> (UnitConstant,        UnitType(rng),   Subst.empty)
  | UTNoContent           -> (NoContent,           StringType(rng), Subst.empty)
  | UTFinishHeaderFile    ->
      begin
        final_tyenv := tyenv ;
        final_varntenv := varntenv ;
        (FinishHeaderFile, UnitType(-1, 0, 0, 0), Subst.empty)
      end

  | UTContentOf(nv) ->
      begin try
        let forallty = Typeenv.find tyenv nv in
        let ty = Typeenv.overwrite_range_of_type (make_bounded_free forallty) rng in
          begin                                                                          (* for debug *)
            print_for_debug ("#C " ^ nv ^ " : " ^ (string_of_type_struct_basic forallty) (* for debug *)
              ^ " = " ^ (string_of_type_struct_basic ty) ^ "\n") ;                       (* for debug *)
            (ContentOf(nv), ty, Subst.empty)
          end                                                                            (* for debug *)
      with
      | Not_found ->
          	raise (TypeCheckError(error_reporting rng ("undefined variable '" ^ nv ^ "'")))
          (*
            (ContentOf(nv), FuncType((-1,0,0,0), IntType(-1,0,0,0), IntType(-1,0,0,0)), Subst.empty) (* for test *)
          *)
      end

  | UTConcat(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck varntenv tyenv utast2 in
      let theta3 = Subst.unify ty1 (StringType(get_range utast1)) in
      let theta4 = Subst.unify ty2 (StringType(get_range utast2)) in
      let theta_result = Subst.compose theta4 (Subst.compose theta3 (Subst.compose theta2 theta1)) in
        (Concat(e1, e2), StringType(rng), theta_result)

  | UTApply(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck varntenv tyenv utast2 in
        begin
        	match ty1 with
          | FuncType(_, tydom, tycod) ->
              let theta3 = Subst.unify ty2 tydom in
                let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let term_result = Apply(e1, e2) in
                let type_result = Subst.apply_to_type_struct theta_result tycod in
                  begin                                                                 (* for debug *)
                    print_for_debug ("\n%Apply1 " ^ (string_of_ast term_result) ^ " : " (* for debug *)
                      ^ (string_of_type_struct_basic type_result) ^ "\n") ;             (* for debug *)
                    print_for_debug ((Subst.string_of_subst theta_result) ^ "\n") ;     (* for debug *)
                      (term_result, type_result, theta_result)
                  end                                                                   (* for debug *)
          | _ ->
              let beta = TypeVariable(rng, new_type_variable_id ()) in
              let theta3 = Subst.unify ty1 (FuncType(get_range utast1, ty2, beta)) in
                let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let term_result = Apply(e1, e2) in
                let type_result = Subst.apply_to_type_struct theta_result beta in
                  begin                                                                 (* for debug *)
                    print_for_debug ("\n%Apply2 " ^ (string_of_ast term_result) ^ " : " (* for debug *)
                      ^ (string_of_type_struct_basic beta) ^ " = "                      (* for debug *)
                      ^ (string_of_type_struct_basic type_result) ^ "\n") ;             (* for debug *)
                    print_for_debug ((Subst.string_of_subst theta_result) ^ "\n") ;     (* for debug *)
                      (term_result, type_result, theta_result)
                  end                                                                   (* for debug *)
        end

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let beta = TypeVariable(varrng, new_type_variable_id ()) in
      let tyenv_new = Typeenv.add tyenv varnm beta in
        let (e1, ty1, theta1) = typecheck varntenv tyenv_new utast1 in
          let term_result = LambdaAbstract(varnm, e1) in
          let tydom = (Subst.apply_to_type_struct theta1 beta) in
          let tycod = ty1 in
          let type_result = FuncType(rng, tydom, tycod) in
          let theta_result = theta1 in
            (term_result, type_result, theta_result)

  | UTLetIn(utmutletcons, utast2) ->
      let (tyenv_forall, _, mutletcons, theta1) = make_type_environment_by_let varntenv tyenv utmutletcons in
      let (e2, ty2, theta2) = typecheck varntenv tyenv_forall utast2 in
        (LetIn(mutletcons, e2), ty2, Subst.compose theta2 theta1)

  | UTLetMutableIn(varrng, varnm, utastdflt, utastaft) ->
      let (edflt, tydflt, thetadflt) = typecheck varntenv tyenv utastdflt in
      let tyenv_new = Subst.apply_to_type_environment thetadflt (Typeenv.add tyenv varnm (RefType(varrng, tydflt))) in
        let (eaft, tyaft, thetaaft) = typecheck varntenv tyenv_new utastaft in
          let theta_result = Subst.compose thetaaft thetadflt in
          let term_result = LetMutableIn(varnm, edflt, eaft) in
          let type_result = Subst.apply_to_type_struct theta_result tyaft in
            (term_result, type_result, theta_result)

  | UTDeclareGlobalHash(utastkey, utastdflt) ->
      let (ekey, tykey, thetakey)    = typecheck varntenv tyenv utastkey in
      let (edflt, tydflt, thetadflt) = typecheck varntenv tyenv utastdflt in
      let thetasubkey  = Subst.unify tykey  (StringType(get_range utastkey))  in
      let thetasubdflt = Subst.unify tydflt (StringType(get_range utastdflt)) in
      let theta_result =  Subst.compose thetasubdflt
                            (Subst.compose thetasubkey
                              (Subst.compose thetadflt thetakey)) in
      let term_result  = DeclareGlobalHash(ekey, edflt) in
        (term_result, UnitType(rng), theta_result)

  | UTOverwriteGlobalHash(utastkey, utastnew) ->
      let (ekey, tykey, thetakey) = typecheck varntenv tyenv utastkey in
      let (enew, tynew, thetanew) = typecheck varntenv tyenv utastnew in
      let thetasubkey = Subst.unify tykey (StringType(get_range utastkey)) in
      let thetasubnew = Subst.unify tynew (StringType(get_range utastnew)) in
      let theta_result =  Subst.compose thetasubnew (
                            Subst.compose thetasubkey (
                              Subst.compose thetanew thetakey)) in
      let term_result  = OverwriteGlobalHash(ekey, enew) in
        (term_result, UnitType(rng), theta_result)

  | UTOverwrite(varrng, varnm, utastnew) ->
      let (_, tyvar, _) = typecheck varntenv tyenv (varrng, UTContentOf(varnm)) in
      let (enew, tynew, thetanew) = typecheck varntenv tyenv utastnew in
      let thetasub = Subst.unify tyvar (RefType(get_range utastnew, tynew)) in
          (*  actually 'get_range utastnew' is not good
              since the right side expression has type 't, not 't ref *)
        (Overwrite(varnm, enew), UnitType(rng), Subst.compose thetasub thetanew)

  | UTSequential(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
      let theta_new = Subst.compose (Subst.unify ty1 (UnitType(get_range utast1))) theta1 in
      let tyenv_new = Subst.apply_to_type_environment theta_new tyenv in
      let (e2, ty2, theta2) = typecheck varntenv tyenv_new utast2 in
        let theta_result = Subst.compose theta2 theta_new in
        let type_result = Subst.apply_to_type_struct theta_result ty2 in
        let term_result = Sequential(e1, e2) in
            (term_result, type_result, theta_result)

  | UTReferenceFinal(utast1) ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
      let thetasub = Subst.unify ty1 (StringType(rng)) in
      let theta_result = Subst.compose thetasub theta1 in
      let term_result = ReferenceFinal(e1) in
        (term_result, StringType(rng), theta_result)

  | UTIfThenElse(utastb, utast1, utast2) ->
      let (eb, tyb, thetab) = typecheck varntenv tyenv utastb in
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck varntenv tyenv utast2 in
      let theta_result =  Subst.compose (Subst.unify ty2 ty1)
                            (Subst.compose theta2
                              (Subst.compose theta1
                                (Subst.compose (Subst.unify tyb (BoolType(-7, 0, 0, 0)))
                                  thetab))) in
      let term_result = IfThenElse(eb, e1, e2) in
      let type_result = Subst.apply_to_type_struct theta_result ty1 in
        (term_result, type_result, theta_result)

  | UTIfClassIsValid(utast1, utast2) ->
      let tyenv_new = Typeenv.add tyenv "class" (StringType((-6, 0, 0, 0))) in
        let (e1, ty1, theta1) = typecheck varntenv tyenv_new utast1 in
        let (e2, ty2, theta2) = typecheck varntenv tyenv utast2 in
        let theta_result = Subst.compose (Subst.unify ty2 ty1) (Subst.compose theta2 theta1) in
        let term_result = IfClassIsValid(e1, e2) in
        let type_result = Subst.apply_to_type_struct theta_result ty1 in
          (term_result, type_result, theta_result)

  | UTIfIDIsValid(utast1, utast2) ->
      let tyenv_new = Typeenv.add tyenv "id" (StringType((-7, 0, 0, 0))) in
        let (e1, ty1, theta1) = typecheck varntenv tyenv_new utast1 in
        let (e2, ty2, theta2) = typecheck varntenv tyenv utast2 in
        let theta_result = Subst.compose (Subst.unify ty2 ty1) (Subst.compose theta2 theta1) in
        let term_result = IfIDIsValid(e1, e2) in
        let type_result = Subst.apply_to_type_struct theta_result ty1 in
          (term_result, type_result, theta_result)

  | UTApplyClassAndID(utastcls, utastid, utast1) ->
      let (ecls, _, _) = typecheck varntenv tyenv utastcls in
      let (eid, _, _)  = typecheck varntenv tyenv utastid in
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
        (ApplyClassAndID(ecls, eid, e1), ty1, theta1)
  
  | UTListCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck varntenv tyenv utasttl in
        let theta_result =  Subst.compose (Subst.unify tytl (ListType((-12, 0, 0, 0), tyhd)))
                              (Subst.compose thetatl thetahd) in
        let type_result = ListType(rng, Subst.apply_to_type_struct theta_result tyhd) in
        let term_result = ListCons(ehd, etl) in
          (term_result, type_result, theta_result)

  | UTEndOfList ->
      let ntyvar = TypeVariable(rng, new_type_variable_id ()) in
        (EndOfList, ListType(rng, ntyvar), Subst.empty)

  | UTWhileDo(utastb, utastc) ->
      let (eb, tyb, thetab) = typecheck varntenv tyenv utastb in
      let (ec, tyc, thetac) = typecheck varntenv tyenv utastc in
        let thetabsub = Subst.unify tyb (BoolType(get_range utastb)) in
        let thetacsub = Subst.unify tyc (UnitType(get_range utastc)) in
          let theta_result =  Subst.compose thetacsub
                                (Subst.compose thetabsub
                                  (Subst.compose thetac thetab)) in
          let term_result = WhileDo(eb, ec) in
            (term_result, UnitType(rng), theta_result)

  | UTTupleCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck varntenv tyenv utasttl in
      let theta_result = Subst.compose thetatl thetahd in
      let term_result = (TupleCons(ehd, etl)) in
      let type_result = Subst.apply_to_type_struct theta_result
        begin match tytl with
        | ProductType(rngtl, tylist) -> ProductType(rng, tyhd :: tylist)
        | _                          -> assert false
        end
      in
        (term_result, type_result, theta_result)

  | UTEndOfTuple -> (EndOfTuple, ProductType(rng, []), Subst.empty)

  | UTPatternMatch(utastobj, utpmcons) ->
      let (eobj, tyobj, thetaobj) = typecheck varntenv tyenv utastobj in
      let ntv = TypeVariable((-300, 0, 0, 0), new_type_variable_id ()) in
      let (pmcons, typm, thetapm) = typecheck_pattern_match_cons varntenv tyenv utpmcons tyobj thetaobj ntv in
        (PatternMatch(eobj, pmcons), typm, thetapm)

  | UTDeclareVariantIn(mutvarntcons, utastaft) ->
      let varntenv_new = Variantenv.add_mutual_cons varntenv mutvarntcons in
        typecheck varntenv_new tyenv utastaft

  | UTDeclareTypeSynonymIn(tynm, tystr, utastaft) ->
      let varntenv_new = Variantenv.add_type_synonym varntenv tynm tystr in
        typecheck varntenv_new tyenv utastaft

  | UTConstructor(constrnm, utastcont) ->
      begin try
        let (varntnm, tyvarnt) = Variantenv.find varntenv constrnm in
          let (econt, tycont, thetacont) = typecheck varntenv tyenv utastcont in
          let theta_result = Subst.compose (Subst.unify tycont tyvarnt) thetacont in
            (Constructor(constrnm, econt), VariantType(rng, varntnm), theta_result)
      with
      | Not_found -> raise (TypeCheckError(error_reporting rng "undefined constructor '" ^ constrnm ^ "'"))
      end

  | UTModule(mdlnm, utmdltr, utastaft) ->
      let (varntenv_new, tyenv_new, emdltr, thetadef) = typecheck_module varntenv tyenv varntenv tyenv mdlnm utmdltr in
      let (eaft, tyaft, thetaaft) = typecheck varntenv_new tyenv_new utastaft in
      let theta_result = Subst.compose thetaaft thetadef in
      let type_result  = Subst.apply_to_type_struct theta_result tyaft in
        (Module(mdlnm, emdltr, eaft), type_result, theta_result)



(* Variantenv.t -> Typeenv.t -> Variantenv.t -> Typeenv.t -> module_name -> untyped_module_tree
    -> (Variantenv.t * Typeenv.t * module_tree * Subst.t) *)
and typecheck_module veout teout vein tein mdlnm (rng, utmdldef) =
  match utmdldef with
  | UTMFinishModule                                       -> (veout, teout, MFinishModule, Subst.empty)
  | UTMDirectLetIn(utmutletcons, utmdlaft)                ->
      let (tein_new, tvtylst_added, mutletcons, theta) = make_type_environment_by_let vein tein utmutletcons in
      let _ = List.map (fun (x, _) -> print_for_debug ("[" ^ x ^ "]\n")) tvtylst_added in (* for debug *)
        let teout_new = add_list_to_type_environment teout tvtylst_added in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module veout teout_new vein tein_new mdlnm utmdlaft in
        let theta_result = Subst.compose thetaaft theta in
          (veout_result, teout_result, MDirectLetIn(mutletcons, eaft), theta_result)

  | UTMPublicLetIn(utmutletcons, utmdlaft)                ->
      let (tein_new, tvtylst_added, mutletcons, theta) = make_type_environment_by_let vein tein utmutletcons in
      let _ = List.map (fun (x, _) -> print_for_debug ("[" ^ x ^ "]\n")) tvtylst_added in (* for debug *)
      let teout_new = add_list_to_module_type_environment teout tvtylst_added mdlnm in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module veout teout_new vein tein_new mdlnm utmdlaft in
      let theta_result = Subst.compose thetaaft theta in
        (veout_result, teout_result, MPublicLetIn(mutletcons, eaft), theta_result)

  | UTMPrivateLetIn(utmutletcons, utmdlaft)               ->
      let (tein_new, _, mutletcons, theta) = make_type_environment_by_let vein tein utmutletcons in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module veout teout vein tein_new mdlnm utmdlaft in
      let theta_result = Subst.compose thetaaft theta in
        (veout_result, teout_result, MPrivateLetIn(mutletcons, eaft), theta_result)
(*
  | UTMPublicLetMutableIn(rng, varnm, utast, utmdlaft)    ->
  | UTMPublicDeclareVariantIn(utmutvarntcons, utmdlaft)   ->
  | UTMPublicDeclareTypeSynonymIn(tynm, tystr, utmdlaft)  ->
  | UTMPrivateLetMutableIn(rng, varnm, utast, utmdlaft)   ->
  | UTMPrivateDeclareVariantIn(utmutvarntcons, utmdlaft)  ->
  | UTMPrivateDeclareTypeSynonymIn(tynm, tystr, utmdlaft) ->
*)

(* Typeenv.t -> (var_name * type_struct) list -> Typeenv.t *)
and add_list_to_type_environment tyenv tvtylst =
  match tvtylst with
  | []                         -> tyenv
  | (varnm, tystr) :: tvtytail ->
      add_list_to_type_environment (Typeenv.add tyenv varnm tystr) tvtytail

(* Typeenv.t -> (var_name * type_struct) list -> module_name -> Typeenv.t *)
and add_list_to_module_type_environment tyenv tvtylst mdlnm =
  match tvtylst with
  | []                         -> tyenv
  | (varnm, tystr) :: tvtytail ->
      add_list_to_module_type_environment (Typeenv.add tyenv (mdlnm ^ "." ^ varnm) tystr) tvtytail mdlnm


(* Typeenv.t -> untyped_pattern_match_cons -> type_struct -> Subst.t -> type_struct
	  -> (pattern_match_cons * type_struct * Subst.t) *)
and typecheck_pattern_match_cons varntenv tyenv (rng, utpmconsmain) tyobj theta tyres =
  match utpmconsmain with
  | UTEndOfPatternMatch -> (EndOfPatternMatch, (Subst.apply_to_type_struct theta tyres), theta)

  | UTPatternMatchCons(utpat, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern varntenv tyenv utpat in
      let thetapat  = Subst.compose (Subst.unify tyobj typat) theta in
      let tyenv1    = Subst.apply_to_type_environment thetapat tyenvpat in
      let (e1, ty1, theta1)       = typecheck varntenv tyenv1 utast1 in
      let theta2    = Subst.compose (Subst.unify ty1 tyres) (Subst.compose theta1 thetapat) in
      let tyres_new = Subst.apply_to_type_struct theta2 tyres in
      let (pmctl, tytl, thetatl)  = typecheck_pattern_match_cons varntenv tyenv tailcons tyobj theta2 tyres_new in
        (PatternMatchCons(epat, e1, pmctl), tytl, thetatl)

  | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern varntenv tyenv utpat in
      let (eb, tyb, thetab)       = typecheck varntenv tyenvpat utastb in
      let thetapat  = Subst.compose (Subst.unify tyb (BoolType(-400, 0, 0, 0)))
                        (Subst.compose thetab
                        	(Subst.compose (Subst.unify tyobj typat) theta)) in
      let tyenv1    = Subst.apply_to_type_environment thetapat tyenvpat in
      let (e1, ty1, theta1)       = typecheck varntenv tyenv1 utast1 in
      let theta2    = Subst.compose (Subst.unify ty1 tyres) (Subst.compose theta1 thetapat) in
      let tyres_new = Subst.apply_to_type_struct theta2 tyres in
      let (pmctl, tytl, thetatl)  = typecheck_pattern_match_cons varntenv tyenv tailcons tyobj theta2 tyres_new in
        (PatternMatchConsWhen(epat, eb, e1, pmctl), tytl, thetatl)


(* Typeenv.t * untyped_pattern_tree -> (pattern_tree * type_struct * Typeenv.t) *)
and typecheck_pattern varntenv tyenv (rng, utpatmain) =
  match utpatmain with
  | UTPNumericConstant(nc) -> (PNumericConstant(nc), IntType(rng), tyenv)
  | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), BoolType(rng), tyenv)
  | UTPStringConstant(ut1)  ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv ut1 in
      let theta_new = Subst.compose (Subst.unify (StringType(-201, 0, 0, 0)) ty1) theta1 in
      let tyenv_new = Subst.apply_to_type_environment theta_new tyenv in
        (PStringConstant(e1), StringType(rng), tyenv_new)

  | UTPUnitConstant        -> (PUnitConstant, UnitType(rng), tyenv)

  | UTPListCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = typecheck_pattern varntenv tyenv utpat1 in
      let (epat2, typat2, tyenv2) = typecheck_pattern varntenv tyenv1 utpat2 in
        let theta = Subst.unify typat2 (ListType((-200, 0, 0, 0), typat1)) in
        let tyenv_result = Subst.apply_to_type_environment theta tyenv2 in
        let type_result = Subst.apply_to_type_struct theta typat2 in
          (PListCons(epat1, epat2), type_result, tyenv_result)

  | UTPEndOfList ->
      let ntv = TypeVariable(rng, new_type_variable_id ()) in (PEndOfList, ListType(rng, ntv), tyenv)

  | UTPTupleCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = typecheck_pattern varntenv tyenv utpat1 in
      let (epat2, typat2, tyenv2) = typecheck_pattern varntenv tyenv1 utpat2 in
      let type_result =
        begin
        	match typat2 with
          | ProductType(_, tylist) -> ProductType(rng, typat1 :: tylist)
          | _                      -> assert false
        end
      in
        (PTupleCons(epat1, epat2), type_result, tyenv2)

  | UTPEndOfTuple -> (PEndOfTuple, ProductType(rng, []), tyenv)

  | UTPWildCard ->
      let ntv = TypeVariable(rng, new_type_variable_id ()) in (PWildCard, ntv, tyenv)

  | UTPVariable(varnm) ->
      let ntv = TypeVariable(rng, new_type_variable_id ()) in
        (PVariable(varnm), ntv, Typeenv.add tyenv varnm ntv)

  | UTPAsVariable(varnm, utpat1) ->
      let ntv = TypeVariable(rng, new_type_variable_id ()) in
      let (epat1, typat1, tyenv1) = typecheck_pattern varntenv tyenv utpat1 in
        (PAsVariable(varnm, epat1), typat1, Typeenv.add tyenv varnm ntv)

  | UTPConstructor(constrnm, utpat1) ->
      begin try
        let (varntnm, tycont) = Variantenv.find varntenv constrnm in
        let (epat1, typat1, tyenv1) = typecheck_pattern varntenv tyenv utpat1 in
        let tyenv_new = Subst.apply_to_type_environment (Subst.unify tycont typat1) tyenv1 in
          (PConstructor(constrnm, epat1), VariantType(rng, varntnm), tyenv_new)
      with
      | Not_found -> raise (TypeCheckError(error_reporting rng "undefined constructor '" ^ constrnm ^ "'"))
      end


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons ->
	  (Typeenv.t * (var_name * type_struct) list * mutual_let_cons * Subst.t) *)
and make_type_environment_by_let varntenv tyenv utmutletcons =
  let (tyenv_for_rec, tvtylst) = add_mutual_variables varntenv tyenv utmutletcons in
  let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents varntenv tyenv_for_rec utmutletcons tvtylst in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual varntenv tyenv_new tyenv theta1 tvtylst [] in
    (tyenv_forall, tvtylst_forall, mutletcons, theta1)


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * ((var_name * type_struct) list)) *)
and add_mutual_variables varntenv tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet                         -> (tyenv, [])
  | UTMutualLetCons(varnm, astdef, tailcons) ->
      let ntv = TypeVariable(get_range astdef, new_type_variable_id ()) in
        let (tyenv_tail, tvtylst) = add_mutual_variables varntenv (Typeenv.add tyenv varnm ntv) tailcons in
          (tyenv_tail, ((varnm, ntv) :: tvtylst))


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons -> ((var_name * type_struct) list)
  -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents varntenv tyenv mutletcons tvtylst =
  match (mutletcons, tvtylst) with
  | (UTEndOfMutualLet, []) -> (tyenv, EndOfMutualLet, Subst.empty)

  | (UTMutualLetCons(nv, utast1, tailcons), (_, tvty) :: tvtytail) ->
      let (e1, ty1, theta1) = typecheck varntenv tyenv utast1 in
        let theta1new = Subst.compose (Subst.unify ty1 tvty) theta1 in
        let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1new tyenv) nv ty1 in
        let (tyenv_tail, mutletcons_tail, theta_tail) = typecheck_mutual_contents varntenv tyenv_new tailcons tvtytail in
        let theta1final = Subst.compose theta_tail theta1new in
          (Subst.apply_to_type_environment theta1final tyenv_tail, MutualLetCons(nv, e1, mutletcons_tail), theta1final)

  | _ -> assert false


(* Variantenv.t -> Typeenv.t -> Typeenv.t -> Subst.t -> (var_name * type_struct) list ->
	  (var_name * type_struct) list -> (Typeenv.t * ((var_name * type_struct) list) *)
and make_forall_type_mutual varntenv tyenv tyenv_before_let theta tvtylst tvtylst_forall =
  match tvtylst with
  | []                        -> (tyenv, tvtylst_forall)
  | (varnm, tvty) :: tvtytail ->
      let prety = Subst.apply_to_type_struct theta tvty in
        begin                                                                                     (* for debug *)
          print_for_debug (Subst.string_of_subst theta) ;                                         (* for debug *)
          print_for_debug (Typeenv.string_of_type_environment tyenv "MakeForall") ;               (* for debug *)
          print_for_debug ("#M " ^ varnm ^ " : " ^ (string_of_type_struct_basic prety) ^ "\n") ;  (* for debug *)
          let forallty  = Typeenv.erase_range_of_type (Typeenv.make_forall_type prety tyenv_before_let) in
          let tyenv_new = Typeenv.add tyenv varnm forallty in
          let tvtylst_forall_new = (varnm, forallty) :: tvtylst_forall in
            make_forall_type_mutual varntenv tyenv_new tyenv_before_let theta tvtytail tvtylst_forall_new
        end                                                                                       (* for debug *)


(* untyped_abstract_tree -> (type_struct * Variantenv.t * Typeenv.t) *)
let main varntenv tyenv utast =
    begin
      final_varntenv := varntenv ;
      final_tyenv := tyenv ;
      let (e, ty, theta) = typecheck varntenv tyenv utast in
        (ty, !final_varntenv, !final_tyenv, e)
    end
