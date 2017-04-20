open Types
open Display

exception UndefinedVariable    of Range.t * var_name
exception UndefinedConstructor of Range.t * var_name


let print_for_debug_typecheck msg =
(*
  print_string msg ;
*)
  ()


let rec unify_ (varntenv : Variantenv.t) (ty1 : mono_type) (ty2 : mono_type) = () (* temporary *)


let final_tyenv    : Typeenv.t ref    = ref Typeenv.empty
let final_varntenv : Variantenv.t ref = ref Variantenv.empty


let rec typecheck qtfbl (varntenv : Variantenv.t) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?q:(q = qtfbl) ?v:(v = varntenv) t = typecheck q v t in
  let unify = unify_ varntenv in
  match utastmain with
  | UTStringEmpty         -> (StringEmpty,         (rng, StringType))
  | UTBreakAndIndent      -> (SoftBreakAndIndent,  (rng, StringType))
  | UTNumericConstant(nc) -> (NumericConstant(nc), (rng, IntType)   )
  | UTStringConstant(sc)  -> (StringConstant(sc),  (rng, StringType))
  | UTBooleanConstant(bc) -> (BooleanConstant(bc), (rng, BoolType)  )
  | UTUnitConstant        -> (UnitConstant,        (rng, UnitType)  )
  | UTFinishHeaderFile    ->
      begin
        final_tyenv    := tyenv ;
        final_varntenv := varntenv ;
        (FinishHeaderFile, (Range.dummy "finish-header-file", UnitType))
      end

  | UTContentOf(varnm) ->
      begin
        try
          let pty = Typeenv.find tyenv varnm in
          let tyfree = instantiate qtfbl pty in
          let tyres = overwrite_range_of_type tyfree rng in
            begin                                                                                                   (* for debug *)
              print_for_debug_typecheck ("#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty)              (* for debug *)
                ^ " = " ^ (string_of_mono_type_basic tyres) ^ " ("                                                  (* for debug *)
                                         ^ (Range.to_string rng) ^ ")\n") ;                                         (* for debug *)
              (ContentOf(varnm), tyres)
            end                                                                                                     (* for debug *)
        with
        | Not_found -> raise (UndefinedVariable(rng, varnm))
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        try
          let (tyid, pty) = Variantenv.find varntenv constrnm in
          let (e1, ty1) = typecheck_iter tyenv utast1 in
          let () = unify ty1 tyfree in
          let tyres = (rng, VariantType(tyarglist, tyid)) in
            (Constructor(constrnm, e1), tyres)
        with
        | Not_found -> raise (UndefinedConstructor(rng, constrnm))
      end

  | UTConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, StringType) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let () = unify ty2 (get_range utast2, StringType) in
        (Concat(e1, e2), (rng, StringType))

  | UTApply(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let _ = print_for_debug_typecheck ("#Apply " ^ (string_of_utast (rng, utastmain)) ^ "\n") in (* for debug *)
      begin
        match ty1 with
        | (_, FuncType(tydom, tycod)) ->
            let () = unify tydom ty2 in
            let _ = print_for_debug_typecheck ("1 " ^ (string_of_ast (Apply(e1, e2))) ^ " : "                  (* for debug *)
                                               ^ (string_of_mono_type_basic tycod) ^ "\n") in      (* for debug *)
            let tycodnew = overwrite_range_of_type tycod rng in
            let _ = print_for_debug_typecheck ((Subst.string_of_subst (thetaU @@ theta2 @@ theta1)) ^ "\n") in (* for debug *)
              (Apply(e1, e2), thetaU @> tycodnew, thetaU @@ theta2 @@ theta1, kdenvU)
        | _ ->
            let tvid = Tyvarid.fresh UniversalKind qtfbl () in
            let beta = (rng, TypeVariable(ref Free(tvid))) in
            let () = unify ty1 (get_range utast1, FuncType(ty2, beta)) in
            let _ = print_for_debug_typecheck ("2 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " ^ (string_of_mono_type_basic beta) ^ " = " ^ (string_of_mono_type_basic (thetaU @> beta)) ^ "\n") in (* for debug *)
            let _ = print_for_debug_typecheck ((Subst.string_of_subst (thetaU @@ theta2 @@ theta1)) ^ "\n") in (* for debug *)
                (Apply(e1, e2), beta)
      end

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (varrng, TypeVariable(ref Free(tvid))) in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnm (Poly(beta))) utast1 in
        let tydom = beta in
        let tycod = ty1 in
          (LambdaAbstract(varnm, e1), (rng, FuncType(tydom, tycod)))

  | UTLetIn(utmutletcons, utast2) ->
      let (tyenv_forall, _, mutletcons, theta1, _) = make_type_environment_by_let qtfbl varntenv tyenv utmutletcons in
      let (e2, ty2) = typecheck_iter tyenv_forall utast2 in
        (LetIn(mutletcons, e2), ty2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      let () = unify tyB (Range.dummy "if-bool", BoolType) in
      let (e1, ty1) = typecheck_iter (thetaUB @=> tyenv) utast1 in
      let (e2, ty2) = typecheck_iter (theta1UB @=> tyenv) utast2 in
      let () = unify ty2 ty1 in
        (IfThenElse(eB, e1, e2), ty1)

(* ---- impleratives ---- *)

  | UTLetMutableIn(varrng, varnm, utastI, utastA) ->
      let (tyenvI, eI, tyI) = make_type_environment_by_let_mutable varntenv tyenv varrng varnm utastI in
      let (eA, tyA) = typecheck_iter tyenvI utastA in
        (LetMutableIn(varnm, eI, eA), tyA)

  | UTOverwrite(varrng, varnm, utastN) ->
      let (_, tyvar) = typecheck_iter tyenv (varrng, UTContentOf(varnm)) in
      let (eN, tyN) = typecheck_iter tyenv utastN in
      let () = unify tyvar (get_range utastN, RefType(tyN)) in
          (*  actually 'get_range utastnew' is not good
              since the right side expression has type 't, not 't ref *)
        (Overwrite(varnm, eN), (rng, UnitType))

  | UTSequential(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, UnitType) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
        (Sequential(e1, e2), ty2)

  | UTWhileDo(utastB, utastC) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      let () = unify tyB (get_range utastB, BoolType) in
      let (eC, tyC) = typecheck_iter tyenv utastC in
      let () = unify tyC (get_range utastC, UnitType) in
        (WhileDo(eB, eC), (rng, UnitType))

  | UTLazyContent(utast1) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
        (LazyContent(e1), ty1)

(* ---- final reference ---- *)

  | UTDeclareGlobalHash(utastK, utastI) ->
      let (eK, tyK) = typecheck_iter tyenv utastK in
      let () = (unify tyK (get_range utastK, StringType)) in
      let (eI, tyI) = typecheck_iter tyenv utastI in
      let () = unify tyI (get_range utastI, StringType) in
        (DeclareGlobalHash(eK, eI), (rng, UnitType))

  | UTOverwriteGlobalHash(utastK, utastN) ->
      let (eK, tyK) = typecheck_iter tyenv utastK in
      let () = unify tyK (get_range utastK, StringType) in
      let thetaUK = thetaU @@ thetaK in
      let (eN, tyN) = typecheck_iter tyenv utastN in
      let () = unify tyN (get_range utastN, StringType) in
        (OverwriteGlobalHash(eK, eN), (rng, UnitType))

  | UTReferenceFinal(utast1) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (rng, StringType) in
        (ReferenceFinal(e1), (rng, StringType))

(* ---- class/id option ---- *)

  | UTApplyClassAndID(utastcls, utastid, utast1) ->
      let dr = Range.dummy "ut-apply-class-and-id" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (Mono((dr, VariantType([(dr, StringType)], Variantenv.find_type_id varntenv "maybe")))) in
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (Mono((dr, VariantType([(dr, StringType)], Variantenv.find_type_id varntenv "maybe")))) in
      let (ecls, _) = typecheck_iter tyenv utastcls in
      let (eid, _)  = typecheck_iter tyenv utastid in
      let (e1, ty1) = typecheck_iter tyenv_new utast1 in
        (ApplyClassAndID(ecls, eid, e1), ty1)

  | UTClassAndIDRegion(utast1) ->
      let dr = Range.dummy "ut-class-and-id-region" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (Mono((dr, VariantType([(dr, StringType)], Variantenv.find_type_id varntenv "maybe")))) in (* temporary *)
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (Mono((dr, VariantType([(dr, StringType)], Variantenv.find_type_id varntenv "maybe")))) in (* temporary *)
      let (e1, ty1) = typecheck_iter tyenv_new utast1 in
        (e1, ty1)

(* ---- lightweight itemize ---- *)

  | UTItemize(utitmz) ->
      let eitmz = typecheck_itemize qtfbl varntenv tyenv utitmz Subst.empty in
        (eitmz, (rng, VariantType([], Variantenv.find_type_id varntenv "itemize"))) (* temporary *)

(* ---- list ---- *)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      let () = unify tyT (Range.dummy "list-cons", ListType(tyH)) in
      let tyres = (rng, ListType(tyH)) in
        (ListCons(eH, eT), tyres)

  | UTEndOfList ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        (EndOfList, (rng, ListType(beta)))

(* ---- tuple ---- *)

  | UTTupleCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      let tyres =
        match tyT with
        | (_, ProductType(tylist)) -> (rng, ProductType(tyH :: tylist))
        | _                        -> assert false
      in
        (TupleCons(eH, eT), tyres)

  | UTEndOfTuple -> (EndOfTuple, (rng, ProductType([])))

(* ---- records ---- *)

  | UTRecord(flutlst) -> typecheck_record qtfbl varntenv tyenv flutlst rng

  | UTAccessField(utast1, fldnm) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let tvid1 = Tyvarid.fresh (RecordKind(Assoc.of_list [(fldnm, betaF)])) qtfbl () in
      let beta1 = (get_range utast1, TypeVariable(ref (Free(tvid1)))) in
      let tvidF = Tyvarid.fresh UniversalKind qtfbl () in
      let betaF = (rng, TypeVariable(ref (Free(tvidF)))) in
      let _ = print_for_debug_typecheck ("#Kinds(access) " ^ (Display.string_of_kind_environment kdenvnew) ^ "\n") in (* for debug *)
      let () = unify beta1 ty1 in
        (AccessField(e1, fldnm), betaF)

(* ---- other fundamentals ---- *)

  | UTPatternMatch(utastO, utpmcons) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(ref (Free(tvid)))) in
      let (pmcons, tyP) =
            typecheck_pattern_match_cons qtfbl varntenv tyenv utpmcons tyO beta in
        (PatternMatch(eO, pmcons), tyP)

  | UTDeclareVariantIn(mutvarntcons, utastA) ->
      let varntenvnew = Variantenv.add_mutual_cons varntenv mutvarntcons in
        typecheck_iter ~v:varntenvnew tyenv utastA
(*
  | UTModule(mdlnm, utmdltr, utastA) ->
      let (varntenvnew, tyenvnew, emdltr, thetaD) = typecheck_module qtfbl kdenv varntenv tyenv varntenv tyenv mdlnm utmdltr in
      let (eA, tyA, thetaA, kdenvA) = typecheck_iter ~v:varntenvnew kdenv tyenvnew utastA in (* temporary *)
        (Module(mdlnm, emdltr, eA), tyaft, thetaA @@ thetaD, kdenvA)
*)

and typecheck_record
    (qtfbl : quantifiability) (varntenv : Variantenv.t) (tyenv : Typeenv.t)
    (flutlst : (field_name * untyped_abstract_tree) list) (rng : Range.t)
=
  let rec aux
      (tyenv : Typeenv.t) (lst : (field_name * untyped_abstract_tree) list)
      (accelst : (field_name * abstract_tree) list) (acctylst : (field_name * mono_type) list)
  =
    match lst with
    | []                       -> (List.rev accelst, List.rev acctylst, acctheta, kdenv)
    | (fldnmX, utastX) :: tail ->
        let (eX, tyX) = typecheck qtfbl varntenv tyenv utastX in
          aux tyenv tail ((fldnmX, eX) :: accelst) ((fldnmX, tyX) :: acctylst)
  in
  let (elst, tylst) = aux tyenv flutlst [] [].empty in
  let tylstfinal = List.map (fun (fldnm, ty) -> (fldnm, ty)) tylst in
    (Record(Assoc.of_list elst), (rng, RecordType(Assoc.of_list tylstfinal)))


and typecheck_itemize (qtfbl : quantifiability) (varntenv : Variantenv.t) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) =
  let (e1, ty1) = typecheck qtfbl varntenv tyenv utast1 in
  let () = unify_ varntenv ty1 (Range.dummy "typecheck_itemize_string", StringType) in
  let (elst) = typecheck_itemize_list qtfbl varntenv kdenvU tyenv1 utitmzlst in
    (Constructor("Item", TupleCons(e1, TupleCons(elst, EndOfTuple))))


and typecheck_itemize_list (qtfbl : quantifiability) (varntenv : Variantenv.t) (tyenv : Typeenv.t)
    (utitmzlst : untyped_itemize list) =
  match utitmzlst with
  | []                  -> (EndOfList, acctheta, kdenv)
  | hditmz :: tlitmzlst ->
      let ehd = typecheck_itemize qtfbl varntenv tyenv hditmz in
      let etl = typecheck_itemize_list qtfbl varntenv tyenv tlitmzlst in
        (ListCons(ehd, etl))

(*
and typecheck_module
    qtfbl (kdenv : Kindenv.t) (veout : Variantenv.t) (teout : Typeenv.t) (vein : Variantenv.t) (tein : Typeenv.t)
    (mdlnm : module_name) (utmdltr : untyped_module_tree)
=
  let (rng, utmdldef) = utmdltr in
  match utmdldef with

  | UTMFinishModule -> (veout, teout, MFinishModule, Subst.empty)

  | UTMDirectLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl kdenv vein tein utmutletcons in
        let teout_new = add_list_to_type_environment "" teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MDirectLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPublicLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl kdenv vein tein utmutletcons in
        let teout_new = add_list_to_type_environment mdlnm teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPublicLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPrivateLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, _, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl kdenv vein tein utmutletcons in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPrivateLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPublicDeclareVariantIn(utmutvarntcons, utmdlaft) ->
      let vein_new  = Variantenv.add_mutual_cons (LocalScope(mdlnm)) vein utmutvarntcons in
      let veout_new = Variantenv.add_mutual_cons_hidden mdlnm veout utmutvarntcons in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout_new teout vein_new tein mdlnm utmdlaft in
        (veout_result, teout_result, eaft, thetaaft)

  | UTMPrivateDeclareVariantIn(utmutvarntcons, utmdlaft)  ->
      let vein_new  = Variantenv.add_mutual_cons (LocalScope(mdlnm)) vein utmutvarntcons in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout vein_new tein mdlnm utmdlaft in
        (veout_result, teout_result, eaft, thetaaft)

  | UTMPublicLetMutableIn(varrng, varnm, utini, utmdlaft) ->
      let (tein_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable kdenv vein tein varrng varnm utini in
      let teout_new = add_list_to_type_environment mdlnm teout [(varnm, (varrng, RefType(tyini)))] in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout_new vein tein_new mdlnm utmdlaft in
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), thetaaft @@ thetaini)

  | UTMPrivateLetMutableIn(varrng, varnm, utini, utmdlaft) ->
      let (tein_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable kdenv vein tein varrng varnm utini in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl kdenv veout teout vein tein_new mdlnm utmdlaft in
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), thetaaft @@ thetaini)


and add_list_to_type_environment (mdlnm : module_name) (tyenv : Typeenv.t) (tvtylst : (var_name * mono_type) list) =
  match tvtylst with
  | []                      -> tyenv
  | (varnm, ty) :: tvtytail ->
      add_list_to_type_environment mdlnm (Typeenv.add tyenv (Variantenv.append_module_name mdlnm varnm) (Mono(ty))) tvtytail
*)


and typecheck_pattern_match_cons
    (qtfbl : quantifiability) (varntenv : Variantenv.t) (tyenv : Typeenv.t)
      (utpmcons : untyped_pattern_match_cons) (tyobj : mono_type) (tyres : mono_type) =
  let iter = typecheck_pattern_match_cons qtfbl varntenv in
  let unify = unify_ varntenv in
  match utpmcons with
  | UTEndOfPatternMatch -> (EndOfPatternMatch, tyres)

  | UTPatternMatchCons(utpat, utast1, tailcons) ->
      let (epat, typat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let () = unify typat tyobj in
      let (e1, ty1) = typecheck qtfbl varntenv tyenvpat utast1 in
      let () = unify ty1 tyres in
      let (pmctl, tytl) =
            iter tyenv tailcons tyobj tyres in
        (PatternMatchCons(epat, e1, pmctl), tytl)

  | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let () = unify typat tyobj in
      let (eB, tyB) = typecheck qtfbl varntenv tyenvpat utastb in
      let () = unify tyB (Range.dummy "pattern-match-cons-when", BoolType) in
      let (e1, ty1) = typecheck qtfbl varntenv tyenvpat utast1 in
      let () = unify ty1 tyres in
      let (pmctl, tytl) =
            iter tyenv tailcons tyobj tyres in
        (PatternMatchConsWhen(epat, eB, e1, pmctl), tytl)


and typecheck_pattern (qtfbl : quantifiability) (varntenv : Variantenv.t) (tyenv : Typeenv.t) (rng, utpatmain) =
  let iter = typecheck_pattern qtfbl varntenv in
  let unify = unify_ varntenv in
  match utpatmain with
  | UTPNumericConstant(nc) -> (PNumericConstant(nc), (rng, IntType), tyenv)
  | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BoolType), tyenv)
  | UTPStringConstant(ut1) ->
      let (e1, ty1) = typecheck qtfbl varntenv tyenv ut1 in
      let () = unify (Range.dummy "pattern-string-constant", StringType) ty1 in
        (PStringConstant(e1), (rng, StringType), tyenv)

  | UTPUnitConstant        -> (PUnitConstant, (rng, UnitType), tyenv)

  | UTPListCons(utpat1, utpat2) ->
      let (epat1, typat1) = iter kdenv tyenv utpat1 in
      let (epat2, typat2) = iter kdenv1 tyenv1 utpat2 in
      let () = unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
        (PListCons(epat1, epat2), typat2, tyenv2)

  | UTPEndOfList ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        (PEndOfList, (rng, ListType(beta)), tyenv)

  | UTPTupleCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
      let (epat2, typat2, tyenv2) = iter tyenv1 utpat2 in
      let tyres =
        match typat2 with
        | (rng, ProductType(tylist)) -> (rng, ProductType(typat1 :: tylist))
        | _                          -> assert false
      in
        (PTupleCons(epat1, epat2), tyres, tyenv2)

  | UTPEndOfTuple -> (PEndOfTuple, (rng, ProductType([])), tyenv)

  | UTPWildCard ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        (PWildCard, beta, tyenv)

  | UTPVariable(varnm) ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        (PVariable(varnm), beta, Typeenv.add tyenv varnm (Poly(beta)))

  | UTPAsVariable(varnm, utpat1) ->
      let tvid = Tyvarid.fresh UniversalKind qtfbl () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
      let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
        (PAsVariable(varnm, epat1), typat1, Typeenv.add tyenv varnm (Poly(beta)))

  | UTPConstructor(constrnm, utpat1) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist) = Typeenv.instantiate qtfbl tyforall in
          let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
          let () = unify tyfree typat1 in
            (PConstructor(constrnm, epat1), (rng, VariantType(tyarglist, varntnm)), tyenv1)
        with
        | Not_found -> raise (UndefinedConstructor(rng, constrnm))
      end


and make_type_environment_by_let qtfbl (varntenv : Variantenv.t) (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) =
  let rec add_mutual_variables (acctyenv : Typeenv.t) (mutletcons : untyped_mutual_let_cons) =
    let iter = add_mutual_variables in
      match mutletcons with
      | UTEndOfMutualLet                             -> (acctyenv, [])
      | UTMutualLetCons(_, varnm, astdef, tailcons)  ->
          let tvid = Tyvarid.fresh UniversalKind qtfbl () in
          let beta = (get_range astdef, TypeVariable(ref (Free(tvid)))) in
          let _ = print_for_debug_typecheck ("#AddMutualVar " ^ varnm ^ " : " ^ (Tyvarid.show_direct tvid) ^ " :: U\n") in (* for debug *)
          let (kdenvfinal, tyenvfinal, tvtylst) = iter (Typeenv.add acctyenv varnm (Poly(beta))) tailcons in
            (tyenvfinal, ((varnm, beta) :: tvtylst))
  in
  let rec typecheck_mutual_contents
      (varntenv : Variantenv.t)
      (tyenvforrec : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) (tvtylst : (var_name * mono_type) list)
      (acctvtylstout : (var_name * mono_type) list)
  =
    let iter = typecheck_mutual_contents varntenv in
    let unify = unify_ varntenv in
    match (utmutletcons, tvtylst) with
    | (UTEndOfMutualLet, []) -> (tyenvforrec, EndOfMutualLet, List.rev acctvtylstout)

    | (UTMutualLetCons(mntyopt, varnm, utast1, tailcons), (_, beta) :: tvtytail) ->
        let (e1, ty1) = typecheck qtfbl varntenv tyenvforrec utast1 in
        begin
          match mntyopt with
          | None ->
              let () = unify ty1 (theta1a @> beta) in
                let (tyenvfinal, mutletcons_tail, tvtylstoutfinal) = iter tyenvforrec tailcons tvtytail ((varnm, beta) :: acctvtylstout) in
                  (tyenvfinal, MutualLetCons(varnm, e1, mutletcons_tail), tvtylstoutfinal)

          | Some(mnty) ->
              let (ptyin, ptyout) = Variantenv.fix_manual_type_for_inner_and_outer qtfbl varntenv mnty in
              let (tyin, _) = Typeenv.instantiate qtfbl ptyin in
              let () = unify ty1 (theta1a @> beta) in
              let () = unify tyin (thetaU1a @> beta) in
                let (tyenvfinal, mutletconstail, tvtylstoutfinal) =
                      iter tyenvforrec tailcons tvtytail ((varnm, beta (* <-doubtful *)) :: acctvtylstout) in
                    (tyenvfinal, MutualLetCons(varnm, e1, mutletconstail), tvtylstoutfinal)

          end

    | _ -> assert false
  in
  let rec make_forall_type_mutual (tyenv : Typeenv.t) (tyenv_before_let : Typeenv.t) tvtylst tvtylst_forall =
    match tvtylst with
    | []                        -> (tyenv, tvtylst_forall)
    | (varnm, tvty) :: tvtytail ->
        let prety = theta @> tvty in
          begin                                                                                                        (* for debug *)
            print_for_debug_typecheck ("#MakeForall " ^ varnm ^ " : " ^ (string_of_mono_type_basic prety) ^ "\n") ;  (* for debug *)
            let pty = poly_extend erase_range_of_type (Typeenv.make_forall_type prety tyenv_before_let) in
  (*          let forallty  = Typeenv.make_forall_type prety tyenv_before_let in                              (* for test *) *)
            let tvtylst_forall_new = (varnm, pty) :: tvtylst_forall in
              make_forall_type_mutual (Typeenv.add tyenv varnm pty) tyenv_before_let tvtytail tvtylst_forall_new
          end                                                                                                          (* for debug *)
  in
  let (tyenvforrec, tvtylstforrec) = add_mutual_variables tyenv utmutletcons in
  let (tyenv_new, mutletcons, tvtylstout) =
        typecheck_mutual_contents varntenv tyenvforrec utmutletcons tvtylstforrec [] in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual tyenv_new tyenv tvtylstout [] in
    (tyenv_forall, tvtylst_forall, mutletcons)


and make_type_environment_by_let_mutable (varntenv : Variantenv.t) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI) = typecheck Unquantifiable varntenv tyenv utastI in
  let tyenvI = Typeenv.add tyenv varnm (Mono(varrng, RefType(tyI))) in
    (tyenvI, eI, tyI)


let main (varntenv : Variantenv.t) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) =
    begin
      final_varntenv := varntenv ;
      final_tyenv := tyenv ;
      let (e, ty) = typecheck Quantifiable varntenv tyenv utast in
        (ty, !final_varntenv, !final_tyenv, e)
    end
