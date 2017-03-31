open Types
open Display

exception Error of string

let print_for_debug_typecheck msg =
(*
  print_string msg ;
*)
  ()

let final_tyenv    : Typeenv.t ref    = ref Typeenv.empty
let final_varntenv : Variantenv.t ref = ref Variantenv.empty
let final_kdenv    : Kindenv.t ref    = ref Kindenv.empty

let report_error_with_range rng msg =
  raise (Error("at " ^ (Range.to_string rng) ^ ":\n    " ^ msg))


let (@>)  = Subst.apply_to_type_struct
let (@=>) = Subst.apply_to_type_environment
let (@@)  = Subst.compose (* right assoc. *)


let rec typecheck qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?q:(q = qtfbl) ?v:(v = varntenv) k t = typecheck q v k t in
  match utastmain with
  | UTStringEmpty         -> (StringEmpty,         (rng, StringType), Subst.empty, kdenv)
  | UTBreakAndIndent      -> (SoftBreakAndIndent,  (rng, StringType), Subst.empty, kdenv)
  | UTNumericConstant(nc) -> (NumericConstant(nc), (rng, IntType),    Subst.empty, kdenv)
  | UTStringConstant(sc)  -> (StringConstant(sc),  (rng, StringType), Subst.empty, kdenv)
  | UTBooleanConstant(bc) -> (BooleanConstant(bc), (rng, BoolType),   Subst.empty, kdenv)
  | UTUnitConstant        -> (UnitConstant,        (rng, UnitType),   Subst.empty, kdenv)
  | UTFinishHeaderFile    ->
      begin
        final_tyenv    := tyenv ;
        final_varntenv := varntenv ;
        final_kdenv    := kdenv ;
        (FinishHeaderFile, (Range.dummy "finish-header-file", UnitType), Subst.empty, kdenv)
      end

  | UTContentOf(varnm) ->
      begin
        try
          let tyforall = Typeenv.find tyenv varnm in
          let (tyfree, _, kdenvfree) = Typeenv.make_bounded_free qtfbl kdenv tyforall in
          let tyres = overwrite_range_of_type tyfree rng in
            begin                                                                                             (* for debug *)
(*
              print_for_debug_typecheck ("#Content " ^ varnm ^ " : " ^ (string_of_type_struct_basic tyforall) (* for debug *)
                ^ " = " ^ (string_of_type_struct_basic tyres) ^ " ("                                          (* for debug *)
                                         ^ (Range.to_string rng) ^ ")\n") ;                                   (* for debug *)
*)
              print_for_debug_typecheck ("#Kinds(old) " ^ (Display.string_of_kind_environment kdenv) ^ "\n") ;                 (* for debug *)
              print_for_debug_typecheck ("#Kinds(new) " ^ (Display.string_of_kind_environment kdenvfree) ^ "\n") ;             (* for debug *)
              (ContentOf(varnm), tyres, Subst.empty, kdenvfree)
            end                                                                                               (* for debug *)
        with
        | Not_found -> report_error_with_range rng ("undefined variable '" ^ varnm ^ "'")
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist, kdenvfree) = Typeenv.make_bounded_free qtfbl kdenv tyforall in
          let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenvfree tyenv utast1 in
(*          let tyvarnt = overwrite_range_of_type tyfree (Typeenv.get_range_from_type tycont) in *)
          let tyvarnt = tyfree in
            let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 tyvarnt in
            let thetaU1 = thetaU @@ theta1 in
            let tyres  = overwrite_range_of_type (thetaU1 @> (rng, VariantType(tyarglist, varntnm))) rng in
              (Constructor(constrnm, e1), tyres, thetaU1, kdenvU)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end

  | UTConcat(utast1, utast2) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
      let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 (get_range utast1, StringType) in
      let thetaU1 = thetaU @@ theta1 in
      let (e2, ty2, theta2, kdenv2) = typecheck_iter kdenvU (thetaU1 @=> tyenv) utast2 in
      let (thetaV, kdenvV) = Subst.unify kdenv2 ty2 (get_range utast2, StringType) in
        (Concat(e1, e2), (rng, StringType), thetaV @@ theta2 @@ thetaU1, kdenv2)

  | UTApply(utast1, utast2) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
      let (e2, ty2, theta2, kdenv2) = typecheck_iter kdenv1 (theta1 @=> tyenv) utast2 in
      let ty1new = theta2 @> ty1 in
      let _ = print_for_debug_typecheck "#Apply" in (* for debug *)
      begin
        match ty1new with
        | (_, FuncType(tydom, tycod)) ->
            let (thetaU, kdenvU) = Subst.unify kdenv2 tydom ty2 in
            let _ = print_for_debug_typecheck ("1 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " ^ (string_of_type_struct_basic (thetaU @> tycod)) ^ "\n") in (* for debug *)
            let tycodnew = overwrite_range_of_type tycod rng in
            let _ = print_for_debug_typecheck ((Subst.string_of_subst (thetaU @@ theta2 @@ theta1)) ^ "\n") in (* for debug *)
              (Apply(e1, e2), thetaU @> tycodnew, thetaU @@ theta2 @@ theta1, kdenvU)
        | _ ->
            let tvid = Tyvarid.fresh qtfbl in
            let beta = (rng, TypeVariable(tvid)) in
            let (thetaU, kdenvU) = Subst.unify (Kindenv.add kdenv2 tvid UniversalKind) (theta2 @> ty1) (get_range utast1, FuncType(ty2, beta)) in
            let _ = print_for_debug_typecheck ("2 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " ^ (string_of_type_struct_basic beta) ^ " = " ^ (string_of_type_struct_basic (thetaU @> beta)) ^ "\n") in (* for debug *)
            let _ = print_for_debug_typecheck ((Subst.string_of_subst (thetaU @@ theta2 @@ theta1)) ^ "\n") in (* for debug *)
                (Apply(e1, e2), thetaU @> beta, thetaU @@ theta2 @@ theta1, kdenvU)
      end

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (varrng, TypeVariable(tvid)) in
      let (e1, ty1, theta1, kdenv1) = typecheck_iter (Kindenv.add kdenv tvid UniversalKind) (Typeenv.add tyenv varnm (Mono(beta))) utast1 in
        let tydom = theta1 @> beta in
        let tycod = ty1 in
          (LambdaAbstract(varnm, e1), (rng, FuncType(tydom, tycod)), theta1, kdenv1)

  | UTLetIn(utmutletcons, utast2) ->
      let (kdenv_forall, tyenv_forall, _, mutletcons, theta1, _) = make_type_environment_by_let qtfbl varntenv kdenv tyenv utmutletcons in
      let (e2, ty2, theta2, kdenv2) = typecheck_iter kdenv_forall tyenv_forall utast2 in
        (LetIn(mutletcons, e2), ty2, theta2 @@ theta1, kdenv2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let (eB, tyB, thetaB, kdenvB) = typecheck_iter kdenv tyenv utastB in
      let (thetaU, kdenvU) = Subst.unify kdenvB tyB (Range.dummy "if-bool", BoolType) in
      let thetaUB = thetaU @@ thetaB in
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenvU (thetaUB @=> tyenv) utast1 in
      let theta1UB = theta1 @@ thetaUB in
      let (e2, ty2, theta2, kdenv2) = typecheck_iter kdenv1 (theta1UB @=> tyenv) utast2 in
      let (thetaV, kdenvV) = Subst.unify kdenv2 ty2 ty1 in
        (IfThenElse(eB, e1, e2), (thetaV @@ theta2) @> ty1, thetaV @@ theta2 @@ theta1UB, kdenvV)

(* ---- impleratives ---- *)

  | UTLetMutableIn(varrng, varnm, utastI, utastA) ->
      let (tyenvI, eI, tyI, thetaI, kdenvI) = make_type_environment_by_let_mutable varntenv kdenv tyenv varrng varnm utastI in
      let (eA, tyA, thetaA, kdenvA) = typecheck_iter kdenvI tyenvI utastA in
        (LetMutableIn(varnm, eI, eA), tyA, thetaA @@ thetaI, kdenvA)

  | UTOverwrite(varrng, varnm, utastN) ->
      let (_, tyvar, _, _) = typecheck_iter kdenv tyenv (varrng, UTContentOf(varnm)) in
      let (eN, tyN, thetaN, kdenvN) = typecheck_iter kdenv tyenv utastN in
      let (thetaS, kdenvS) = Subst.unify kdenvN tyvar (get_range utastN, RefType(tyN)) in
          (*  actually 'get_range utastnew' is not good
              since the right side expression has type 't, not 't ref *)
        (Overwrite(varnm, eN), (rng, UnitType), thetaS @@ thetaN, kdenvS)

  | UTSequential(utast1, utast2) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
      let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 (get_range utast1, UnitType) in
      let thetaU1 = thetaU @@ theta1 in
      let (e2, ty2, theta2, kdenv2) = typecheck_iter kdenvU (thetaU1 @=> tyenv) utast2 in
        (Sequential(e1, e2), ty2, theta2 @@ thetaU1, kdenv2)

  | UTWhileDo(utastB, utastC) ->
      let (eB, tyB, thetaB, kdenvB) = typecheck_iter kdenv tyenv utastB in
      let (thetaU, kdenvB) = Subst.unify kdenvB tyB (get_range utastB, BoolType) in
      let thetaUB = thetaU @@ thetaB in
      let (eC, tyC, thetaC, kdenvC) = typecheck_iter kdenvB (thetaUB @=> tyenv) utastC in
      let (thetaV, kdenvV) = Subst.unify kdenvC tyC (get_range utastC, UnitType) in
        (WhileDo(eB, eC), (rng, UnitType), thetaV @@ thetaC @@ thetaUB, kdenvC)

  | UTLazyContent(utast1) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
        (LazyContent(e1), ty1, theta1, kdenv1)

(* ---- final reference ---- *)

  | UTDeclareGlobalHash(utastK, utastI) ->
      let (eK, tyK, thetaK, kdenvK) = typecheck_iter kdenv tyenv utastK in
      let (thetaU, kdenvU) = (Subst.unify kdenvK tyK (get_range utastK, StringType)) in
      let thetaUK = thetaU @@ thetaK in
      let (eI, tyI, thetaI, kdenvI) = typecheck_iter kdenvU (thetaUK @=> tyenv) utastI in
      let (thetaV, kdenvV) = Subst.unify kdenvI tyI (get_range utastI, StringType) in
        (DeclareGlobalHash(eK, eI), (rng, UnitType), thetaV @@ thetaI @@ thetaUK, kdenvV)

  | UTOverwriteGlobalHash(utastK, utastN) ->
      let (eK, tyK, thetaK, kdenvK) = typecheck_iter kdenv tyenv utastK in
      let (thetaU, kdenvU) = Subst.unify kdenvK tyK (get_range utastK, StringType) in
      let thetaUK = thetaU @@ thetaK in
      let (eN, tyN, thetaN, kdenvN) = typecheck_iter kdenvU (thetaUK @=> tyenv) utastN in
      let (thetaV, kdenvV) = Subst.unify kdenvN tyN (get_range utastN, StringType) in
        (OverwriteGlobalHash(eK, eN), (rng, UnitType), thetaV @@ thetaN @@ thetaUK, kdenvV)

  | UTReferenceFinal(utast1) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
      let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 (rng, StringType) in
        (ReferenceFinal(e1), (rng, StringType), thetaU @@ theta1, kdenvU)

(* ---- class/id option ---- *)

  | UTApplyClassAndID(utastcls, utastid, utast1) ->
      let dr = Range.dummy "ut-apply-class-and-id" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (Mono((dr, VariantType([(dr, StringType)], "maybe")))) in
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (Mono((dr, VariantType([(dr, StringType)], "maybe")))) in
      let (ecls, _, _, _) = typecheck_iter kdenv tyenv utastcls in
      let (eid, _, _, _)  = typecheck_iter kdenv tyenv utastid in
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv_new utast1 in
        (ApplyClassAndID(ecls, eid, e1), ty1, theta1, kdenv1)

  | UTClassAndIDRegion(utast1) ->
      let dr = Range.dummy "ut-class-and-id-region" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (Mono((dr, VariantType([(dr, StringType)], "maybe")))) in
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (Mono((dr, VariantType([(dr, StringType)], "maybe")))) in
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv_new utast1 in
        (e1, ty1, theta1, kdenv1)

(* ---- lightweight itemize ---- *)

  | UTItemize(utitmz) ->
      let (eitmz, thetaitmz, kdenvitmz) = typecheck_itemize qtfbl varntenv kdenv tyenv utitmz Subst.empty in
        (eitmz, (rng, VariantType([], "itemize")), thetaitmz, kdenvitmz)

(* ---- list ---- *)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH, thetaH, kdenvH) = typecheck_iter kdenv tyenv utastH in
      let (eT, tyT, thetaT, kdenvT) = typecheck_iter kdenvH (thetaH @=> tyenv) utastT in
      let (thetaU, kdenvU) = Subst.unify kdenvT tyT (Range.dummy "list-cons", ListType(tyH)) in
        let tyres = (rng, ListType((thetaU @@ thetaT) @> tyH)) in
          (ListCons(eH, eT), tyres, thetaU @@ thetaT @@ thetaH, kdenvU)

  | UTEndOfList ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (rng, TypeVariable(tvid)) in
        (EndOfList, (rng, ListType(beta)), Subst.empty, Kindenv.add kdenv tvid UniversalKind)

(* ---- tuple ---- *)

  | UTTupleCons(utastH, utastT) ->
      let (eH, tyH, thetaH, kdenvH) = typecheck_iter kdenv tyenv utastH in
      let (eT, tyT, thetaT, kdenvT) = typecheck_iter kdenvH (thetaH @=> tyenv) utastT in
      let tyres =
        match tyT with
        | (_, ProductType(tylist)) -> (rng, ProductType(tyH :: tylist))
        | _                        -> assert false
      in
        (TupleCons(eH, eT), tyres, thetaT @@ thetaH, kdenvT)

  | UTEndOfTuple -> (EndOfTuple, (rng, ProductType([])), Subst.empty, kdenv)

(* ---- records ---- *)

  | UTRecord(flutlst) -> typecheck_record qtfbl varntenv kdenv tyenv flutlst rng

  | UTAccessField(utast1, fldnm) ->
      let (e1, ty1, theta1, kdenv1) = typecheck_iter kdenv tyenv utast1 in
      let tvid1 = Tyvarid.fresh qtfbl in
      let beta1 = (get_range utast1, TypeVariable(tvid1)) in
      let tvidF = Tyvarid.fresh qtfbl in
      let betaF = (rng, TypeVariable(tvidF)) in
      let kdenvnew = Kindenv.add (Kindenv.add kdenv tvidF UniversalKind) tvid1 (RecordKind(Assoc.of_list [(fldnm, betaF)])) in
      let _ = print_for_debug_typecheck ("#Kinds(access) " ^ (Display.string_of_kind_environment kdenvnew) ^ "\n") in (* for debug *)
      let (thetaU, kdenvU) = Subst.unify kdenvnew beta1 ty1 in
        (AccessField(e1, fldnm), thetaU @> betaF, thetaU @@ theta1, kdenvU)

(* ---- other fundamentals ---- *)

  | UTPatternMatch(utastO, utpmcons) ->
      let (eO, tyO, thetaO, kdenvO) = typecheck_iter kdenv tyenv utastO in
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(tvid)) in
      let (pmcons, tyP, thetaP, kdenvP) =
            typecheck_pattern_match_cons qtfbl varntenv (Kindenv.add kdenvO tvid UniversalKind) (thetaO @=> tyenv) utpmcons tyO thetaO beta in
        (PatternMatch(eO, pmcons), tyP, thetaP @@ thetaO, kdenvP)

  | UTDeclareVariantIn(mutvarntcons, utastA) ->
      let varntenvnew = Variantenv.add_mutual_cons GlobalScope varntenv mutvarntcons in
        typecheck_iter ~v:varntenvnew kdenv tyenv utastA
(*
  | UTModule(mdlnm, utmdltr, utastA) ->
      let (varntenvnew, tyenvnew, emdltr, thetaD) = typecheck_module qtfbl kdenv varntenv tyenv varntenv tyenv mdlnm utmdltr in
      let (eA, tyA, thetaA, kdenvA) = typecheck_iter ~v:varntenvnew kdenv tyenvnew utastA in (* temporary *)
        (Module(mdlnm, emdltr, eA), tyaft, thetaA @@ thetaD, kdenvA)
*)

and typecheck_record
    qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t)
    (flutlst : (field_name * untyped_abstract_tree) list) (rng : Range.t)
=
  let rec aux
      (kdenv : Kindenv.t) (tyenv : Typeenv.t) (lst : (field_name * untyped_abstract_tree) list)
      (accelst : (field_name * abstract_tree) list) (acctylst : (field_name * type_struct) list) (acctheta : Subst.t)
  =
    match lst with
    | []                       -> (List.rev accelst, List.rev acctylst, acctheta, kdenv)
    | (fldnmX, utastX) :: tail ->
        let (eX, tyX, thetaX, kdenvX) = typecheck qtfbl varntenv kdenv tyenv utastX in
          aux kdenvX (thetaX @=> tyenv) tail ((fldnmX, eX) :: accelst) ((fldnmX, tyX) :: acctylst) (thetaX @@ acctheta)
  in
  let (elst, tylst, thetares, kdenvres) = aux kdenv tyenv flutlst [] [] Subst.empty in
  let tylstfinal = List.map (fun (fldnm, tystr) -> (fldnm, thetares @> tystr)) tylst in
    (Record(Assoc.of_list elst), (rng, RecordType(Assoc.of_list tylstfinal)), thetares, kdenvres)


and typecheck_itemize qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) (acctheta : Subst.t) =
  let tyenv1 = acctheta @=> tyenv in
  let (e1, ty1, theta1, kdenv1) = typecheck qtfbl varntenv kdenv tyenv1 utast1 in
  let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 (Range.dummy "typecheck_itemize_string", StringType) in
  let thetaU1a = thetaU @@ theta1 @@ acctheta in
  let (elst, thetalst, kdenvlst) = typecheck_itemize_list qtfbl varntenv kdenvU (thetaU1a @=> tyenv1) utitmzlst thetaU1a in
    (Constructor("Item", TupleCons(e1, TupleCons(elst, EndOfTuple))), thetalst, kdenvlst)


and typecheck_itemize_list qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t)
    (utitmzlst : untyped_itemize list) (acctheta : Subst.t) =
  match utitmzlst with
  | []                  -> (EndOfList, acctheta, kdenv)
  | hditmz :: tlitmzlst ->
      let (ehd, thetahd, kdenvhd) = typecheck_itemize qtfbl varntenv kdenv tyenv hditmz acctheta in
      let (etl, thetatl, kdenvtl) = typecheck_itemize_list qtfbl varntenv kdenvhd (thetahd @=> tyenv) tlitmzlst thetahd in
        (ListCons(ehd, etl), thetatl, kdenvtl)

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
*)

and add_list_to_type_environment (mdlnm : module_name) (tyenv : Typeenv.t) (tvtylst : (var_name * type_struct) list) =
  match tvtylst with
  | []                      -> tyenv
  | (varnm, ty) :: tvtytail ->
      add_list_to_type_environment mdlnm (Typeenv.add tyenv (Variantenv.append_module_name mdlnm varnm) (Mono(ty))) tvtytail


(* Typeenv.t -> untyped_pattern_match_cons -> type_struct -> Subst.t -> type_struct
    -> (pattern_match_cons * type_struct * Subst.t) *)
and typecheck_pattern_match_cons
  qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t)
  (utpmcons : untyped_pattern_match_cons) (tyobj : type_struct) (acctheta : Subst.t) (tyres : type_struct)
=
  let iter = typecheck_pattern_match_cons qtfbl varntenv in
  match utpmcons with
  | UTEndOfPatternMatch -> (EndOfPatternMatch, tyres, acctheta, kdenv)

  | UTPatternMatchCons(utpat, utast1, tailcons) ->
      let (epat, typat, tyenvpat, kdenvpat) = typecheck_pattern qtfbl varntenv kdenv tyenv utpat in
      let (thetaU, kdenvU) = Subst.unify kdenvpat typat tyobj in
      let thetaUa = thetaU @@ acctheta in
      let (e1, ty1, theta1, kdenv1) = typecheck qtfbl varntenv kdenvU (thetaUa @=> tyenvpat) utast1 in
      let (thetaV, kdenvV) = Subst.unify kdenv1 ty1 tyres in
      let thetaV1Ua = thetaV @@ theta1 @@ thetaUa in
      let (pmctl, tytl, thetatl, kdenvtl) =
            iter kdenvV (thetaV1Ua @=> tyenv) tailcons (thetaV1Ua @> tyobj) thetaV1Ua (thetaV1Ua @> tyres) in
        (PatternMatchCons(epat, e1, pmctl), tytl, thetatl, kdenvtl)

  | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
      let (epat, typat, tyenvpat, kdenvpat) = typecheck_pattern qtfbl varntenv kdenv tyenv utpat in
      let (thetaU, kdenvU) = Subst.unify kdenvpat typat tyobj in
      let (eB, tyB, thetaB, kdenvB) = typecheck qtfbl varntenv kdenvU tyenvpat utastb in
      let (thetaW, kdenvW) = Subst.unify kdenvB tyB (Range.dummy "pattern-match-cons-when", BoolType) in
      let thetaWBUa = thetaW @@ thetaB @@ thetaU @@ acctheta in
      let (e1, ty1, theta1, kdenv1) = typecheck qtfbl varntenv kdenvW (thetaWBUa @=> tyenvpat) utast1 in
      let (thetaV, kdenvV) = Subst.unify kdenv1 ty1 tyres in
      let thetaV1WBUa = thetaV @@ theta1 @@ thetaWBUa in
      let (pmctl, tytl, thetatl, kdenvtl) =
            iter kdenvV (thetaV1WBUa @=> tyenv) tailcons (thetaV1WBUa @> tyobj) thetaV1WBUa (thetaV1WBUa @> tyres) in
        (PatternMatchConsWhen(epat, eB, e1, pmctl), tytl, thetatl, kdenvtl)


and typecheck_pattern qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) (rng, utpatmain) =
  let iter = typecheck_pattern qtfbl varntenv in
  match utpatmain with
  | UTPNumericConstant(nc) -> (PNumericConstant(nc), (rng, IntType), tyenv, kdenv)
  | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BoolType), tyenv, kdenv)
  | UTPStringConstant(ut1) ->
      let (e1, ty1, theta1, kdenv1) = typecheck qtfbl varntenv kdenv tyenv ut1 in
      let (thetaU, kdenvU) = Subst.unify kdenv1 (Range.dummy "pattern-string-constant", StringType) ty1 in
      let thetaU1 = thetaU @@ theta1 in
        (PStringConstant(e1), (rng, StringType), thetaU1 @=> tyenv, kdenvU)

  | UTPUnitConstant        -> (PUnitConstant, (rng, UnitType), tyenv, kdenv)

  | UTPListCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1, kdenv1) = iter kdenv tyenv utpat1 in
      let (epat2, typat2, tyenv2, kdenv2) = iter kdenv1 tyenv1 utpat2 in
        let (thetaU, kdenvU) = Subst.unify kdenv2 typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
          (PListCons(epat1, epat2), thetaU @> typat2, thetaU @=> tyenv2, kdenvU)

  | UTPEndOfList ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (rng, TypeVariable(tvid)) in
        (PEndOfList, (rng, ListType(beta)), tyenv, Kindenv.add kdenv tvid UniversalKind)

  | UTPTupleCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1, kdenv1) = iter kdenv tyenv utpat1 in
      let (epat2, typat2, tyenv2, kdenv2) = iter kdenv1 tyenv1 utpat2 in
      let tyres =
        match typat2 with
        | (rng, ProductType(tylist)) -> (rng, ProductType(typat1 :: tylist))
        | _                          -> assert false
      in
        (PTupleCons(epat1, epat2), tyres, tyenv2, kdenv2)

  | UTPEndOfTuple -> (PEndOfTuple, (rng, ProductType([])), tyenv, kdenv)

  | UTPWildCard ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (rng, TypeVariable(tvid)) in
        (PWildCard, beta, tyenv, Kindenv.add kdenv tvid UniversalKind)

  | UTPVariable(varnm) ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (rng, TypeVariable(tvid)) in
        (PVariable(varnm), beta, Typeenv.add tyenv varnm (Mono(beta)), Kindenv.add kdenv tvid UniversalKind)

  | UTPAsVariable(varnm, utpat1) ->
      let tvid = Tyvarid.fresh qtfbl in
      let beta = (rng, TypeVariable(tvid)) in
      let (epat1, typat1, tyenv1, kdenv1) = iter (Kindenv.add kdenv tvid UniversalKind) tyenv utpat1 in
        (PAsVariable(varnm, epat1), typat1, Typeenv.add tyenv varnm (Mono(beta)), kdenv1)

  | UTPConstructor(constrnm, utpat1) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist, kdenvfree) = Typeenv.make_bounded_free qtfbl kdenv tyforall in
          let (epat1, typat1, tyenv1, kdenv1) = iter kdenvfree tyenv utpat1 in
          let (thetaU, kdenvU) = Subst.unify kdenv1 tyfree typat1 in
            (PConstructor(constrnm, epat1), thetaU @> (rng, VariantType(tyarglist, varntnm)), thetaU @=> tyenv1, kdenvU)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end


and make_type_environment_by_let qtfbl (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) =
  let rec add_mutual_variables (acckdenv : Kindenv.t) (acctyenv : Typeenv.t) (mutletcons : untyped_mutual_let_cons) =
    let iter = add_mutual_variables in
      match mutletcons with
      | UTEndOfMutualLet                             -> (acckdenv, acctyenv, [])
      | UTMutualLetCons(_, varnm, astdef, tailcons)  ->
          let tvid = Tyvarid.fresh qtfbl in
          let beta = (get_range astdef, TypeVariable(tvid)) in
          let _ = print_for_debug_typecheck ("#AddMutualVar " ^ varnm ^ " : " ^ (Tyvarid.show_direct tvid) ^ " :: U\n") in (* for debug *)
          let _ = print_for_debug_typecheck ("#Kinds(old) " ^ (Display.string_of_kind_environment acckdenv) ^ "\n") in                      (* for debug *)
          let (kdenvfinal, tyenvfinal, tvtylst) = iter (Kindenv.add acckdenv tvid UniversalKind) (Typeenv.add acctyenv varnm (Mono(beta))) tailcons in
            (kdenvfinal, tyenvfinal, ((varnm, beta) :: tvtylst))
  in
  let rec typecheck_mutual_contents
      (kdenvforrec : Kindenv.t) (tyenvforrec : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) (tvtylst : (var_name * type_struct) list)
      (accthetain : Subst.t) (accthetaout : Subst.t) (acctvtylstout : (var_name * type_struct) list)
  =
    match (utmutletcons, tvtylst) with
    | (UTEndOfMutualLet, []) -> (kdenvforrec, tyenvforrec, EndOfMutualLet, accthetain, accthetaout, List.rev acctvtylstout)

    | (UTMutualLetCons(tyopt, varnm, utast1, tailcons), (_, beta) :: tvtytail) ->
        let (e1, ty1, theta1, kdenv1) = typecheck qtfbl varntenv kdenvforrec tyenvforrec utast1 in
        let theta1a = theta1 @@ accthetain in
        begin
          match tyopt with
          | None ->
              let (thetaU, kdenvU) = (Subst.unify kdenv1 ty1 (theta1a @> beta)) in
              let theta1in = thetaU @@ theta1a in
              let theta1out = theta1 @@ accthetaout in
                let (kdenvfinal, tyenvfinal, mutletcons_tail, thetainfinal, thetaoutfinal, tvtylstoutfinal) = typecheck_mutual_contents kdenvU (theta1in @=> tyenvforrec) tailcons tvtytail theta1in theta1out ((varnm, beta) :: acctvtylstout) in
                  (kdenvfinal, tyenvfinal, MutualLetCons(varnm, e1, mutletcons_tail), thetainfinal, thetaoutfinal, tvtylstoutfinal)

          | Some(tystrmanu) ->
              let (tystrforin, tystrforout) = Variantenv.fix_manual_type_for_inner_and_outer qtfbl varntenv tystrmanu in
              let (thetaU, kdenvU) = Subst.unify kdenv1 ty1 (theta1a @> beta) in
              let thetaU1a = thetaU @@ theta1a in
              let (thetaV, kdenvV) = Subst.unify kdenvU tystrforin (thetaU1a @> beta) in
              let theta1in  = thetaV @@ thetaU1a in
              let theta1out = theta1 @@ accthetaout in
                let (kdenvfinal, tyenvfinal, mutletconstail, thetainfinal, thetaoutfinal, tvtylstoutfinal) =
                      typecheck_mutual_contents kdenvV (theta1in @=> tyenvforrec) tailcons tvtytail theta1in theta1out ((varnm, beta (* <-doubtful *)) :: acctvtylstout) in
                    (kdenvfinal, tyenvfinal, MutualLetCons(varnm, e1, mutletconstail), thetainfinal, thetaoutfinal, tvtylstoutfinal)

          end

    | _ -> assert false
  in
  let rec make_forall_type_mutual (kdenv : Kindenv.t) (tyenv : Typeenv.t) (tyenv_before_let : Typeenv.t) (theta : Subst.t) tvtylst tvtylst_forall =
    match tvtylst with
    | []                        -> (kdenv, tyenv, tvtylst_forall)
    | (varnm, tvty) :: tvtytail ->
        let prety = theta @> tvty in
          begin                                                                                                        (* for debug *)
            print_for_debug_typecheck (Subst.string_of_subst theta) ;                                                  (* for debug *)
            print_for_debug_typecheck ("#MakeForall " ^ varnm ^ " : " ^ (string_of_type_struct_basic prety) ^ "\n") ;  (* for debug *)
            print_for_debug_typecheck ("#Kinds " ^ (Display.string_of_kind_environment kdenv) ^ "\n") ;                (* for debug *)
            let pty = poly_extend erase_range_of_type (Typeenv.make_forall_type prety tyenv_before_let kdenv) in
  (*          let forallty  = Typeenv.make_forall_type prety tyenv_before_let in                              (* for test *) *)
            let tvtylst_forall_new = (varnm, pty) :: tvtylst_forall in
              make_forall_type_mutual kdenv (Typeenv.add tyenv varnm pty) tyenv_before_let theta tvtytail tvtylst_forall_new
          end                                                                                                          (* for debug *)
  in
  let (kdenvforrec, tyenvforrec, tvtylstforrec) = add_mutual_variables kdenv tyenv utmutletcons in
  let _ = print_for_debug_typecheck ("#Kinds(forrec) " ^ (Display.string_of_kind_environment kdenvforrec) ^ "\n") in (* for debug *)
  let (kdenv_new, tyenv_new, mutletcons, thetain, thetaout, tvtylstout) =
        typecheck_mutual_contents kdenvforrec tyenvforrec utmutletcons tvtylstforrec Subst.empty Subst.empty [] in
  let _ = print_for_debug_typecheck ("#Kinds(before) " ^ (Display.string_of_kind_environment kdenv_new) ^ "\n") in (* for debug *)
  let (kdenv_forall, tyenv_forall, tvtylst_forall) = make_forall_type_mutual kdenv_new tyenv_new tyenv thetain tvtylstout [] in
    (kdenv_forall, tyenv_forall, tvtylst_forall, mutletcons, thetain, thetaout)


and make_type_environment_by_let_mutable (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI, thetaI, kdenvI) = typecheck Tyvarid.Unquantifiable varntenv kdenv tyenv utastI in
  let tyenvI = thetaI @=> (Typeenv.add tyenv varnm (Mono(varrng, RefType(tyI)))) in
    (tyenvI, eI, tyI, thetaI, kdenvI)


let main (varntenv : Variantenv.t) (kdenv : Kindenv.t) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) =
    begin
      final_varntenv := varntenv ;
      final_tyenv := tyenv ;
      final_kdenv := kdenv ;
      let (e, ty, theta, _) = typecheck Tyvarid.Quantifiable varntenv Kindenv.empty tyenv utast in
        (ty, !final_varntenv, !final_kdenv, !final_tyenv, e)
    end
