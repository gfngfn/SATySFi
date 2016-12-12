open Types
open Display

exception Error of string

let print_for_debug_typecheck msg =

  print_string msg ;

  ()

let final_tyenv    : Typeenv.t ref    = ref Typeenv.empty
let final_varntenv : Variantenv.t ref = ref Variantenv.empty


let report_error_with_range rng msg =
  raise (Error("at " ^ (Range.to_string rng) ^ ":\n    " ^ msg))


let (@>)  = Subst.apply_to_type_struct
let (@=>) = Subst.apply_to_type_environment
let (@@)  = Subst.compose (* right assoc. *)


let rec typecheck qtfbl varntenv tyenv (rng, utastmain) =
  match utastmain with
  | UTStringEmpty         -> (StringEmpty,         (rng, StringType), Subst.empty)
  | UTBreakAndIndent      -> (SoftBreakAndIndent,  (rng, StringType), Subst.empty)
  | UTNumericConstant(nc) -> (NumericConstant(nc), (rng, IntType),    Subst.empty)
  | UTStringConstant(sc)  -> (StringConstant(sc),  (rng, StringType), Subst.empty)
  | UTBooleanConstant(bc) -> (BooleanConstant(bc), (rng, BoolType),   Subst.empty)
  | UTUnitConstant        -> (UnitConstant,        (rng, UnitType),   Subst.empty)
  | UTNoContent           -> (NoContent,           (rng, StringType), Subst.empty)
  | UTFinishHeaderFile    ->
      begin
        final_tyenv    := tyenv ;
        final_varntenv := varntenv ;
        (FinishHeaderFile, (Range.dummy "finish-header-file", UnitType), Subst.empty)
      end

  | UTContentOf(varnm) ->
      begin
        try
          let tyforall    = Typeenv.find tyenv varnm in
          let (tyfree, _) = Typeenv.make_bounded_free qtfbl tyforall in
          let ty = Typeenv.overwrite_range_of_type tyfree rng in
            begin                                                                                             (* for debug *)
              print_for_debug_typecheck ("#Content " ^ varnm ^ " : " ^ (string_of_type_struct_basic tyforall) (* for debug *)
                ^ " = " ^ (string_of_type_struct_basic ty) ^ " ("                                             (* for debug *)
                ^ (Range.to_string rng) ^ ")\n") ;                                                            (* for debug *)
              (ContentOf(varnm), ty, Subst.empty)
            end                                                                                               (* for debug *)
        with
        | Not_found -> report_error_with_range rng ("undefined variable '" ^ varnm ^ "'")
      end

  | UTConstructor(constrnm, utastcont) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist) = Typeenv.make_bounded_free qtfbl tyforall in
          let (econt, tycont, thetacont) = typecheck qtfbl varntenv tyenv utastcont in
(*          let tyvarnt = Typeenv.overwrite_range_of_type tyfree (Typeenv.get_range_from_type tycont) in *)
          let tyvarnt = tyfree in
            let theta_result    = (Subst.unify tycont tyvarnt) @@ thetacont in
            let type_result_sub = theta_result @> (rng, VariantType(tyarglist, varntnm)) in
            let type_result     = Typeenv.overwrite_range_of_type type_result_sub rng in
            begin                                                                                         (* for debug *)
              print_for_debug_typecheck ("#V " ^ varntnm ^ " : " ^ (string_of_type_struct_basic tyforall) (* for debug *)
                ^ " = " ^ (string_of_type_struct_basic type_result) ^ " ("                                (* for debug *)
                ^ ((Range.to_string rng) ^ ")\n")) ;                                                      (* for debug *)
              (Constructor(constrnm, econt), type_result, theta_result)
            end                                                                                           (* for debug *)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end

  | UTConcat(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let thetau = Subst.unify ty1 (get_range utast1, StringType) in
      let thetau1 = thetau @@ theta1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv (thetau1 @=> tyenv) utast2 in
      let thetav = Subst.unify ty2 (get_range utast2, StringType) in
      let theta_result = thetav @@ theta2 @@ thetau1 in
        (Concat(e1, e2), (rng, StringType), theta_result)

  | UTApply(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv (theta1 @=> tyenv) utast2 in
        let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        let thetau = Subst.unify (theta2 @> ty1) (get_range utast1, FuncType(ty2, beta)) in
          let theta_result = thetau @@ theta2 @@ theta1 in
          let type_result  = thetau @> beta in
            begin                                                                               (* for debug *)
              print_for_debug_typecheck ("\n#Apply " ^ (string_of_ast (Apply(e1, e2))) ^ " : "  (* for debug *)
                ^ (string_of_type_struct_basic beta) ^ " = "                                    (* for debug *)
                ^ (string_of_type_struct_basic type_result) ^ "\n") ;                           (* for debug *)
              print_for_debug_typecheck ((Subst.string_of_subst theta_result) ^ "\n") ;         (* for debug *)
                (Apply(e1, e2), type_result, theta_result)
            end                                                                                 (* for debug *)

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let beta = (varrng, TypeVariable(Tyvarid.fresh qtfbl)) in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv (Typeenv.add tyenv varnm beta) utast1 in
        let tydom = theta1 @> beta in
        let tycod = ty1 in
          (LambdaAbstract(varnm, e1), (rng, FuncType(tydom, tycod)), theta1)

  | UTLetIn(utmutletcons, utast2) ->
      let (tyenv_forall, _, mutletcons, theta1, _) = make_type_environment_by_let qtfbl varntenv tyenv utmutletcons in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv_forall utast2 in
        (LetIn(mutletcons, e2), ty2, theta2 @@ theta1)

  | UTIfThenElse(utastb, utast1, utast2) ->
      let (eb, tyb, thetab) = typecheck qtfbl varntenv tyenv utastb in
      let thetaub = (Subst.unify tyb (Range.dummy "if-bool", BoolType)) @@ thetab in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv (thetab @=> tyenv) utast1 in
      let theta1ub = theta1 @@ thetaub in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv (theta1ub @=> tyenv) utast2 in
      let thetav = Subst.unify ty2 ty1 in
      let theta_result = thetav @@ theta2 @@ theta1ub in
      let type_result = (thetav @@ theta2) @> ty1 in
        (IfThenElse(eb, e1, e2), type_result, theta_result)

(* ---- impleratives ---- *)

  | UTLetMutableIn(varrng, varnm, utastini, utastaft) ->
      let (tyenv_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable varntenv tyenv varrng varnm utastini in
      let (eaft, tyaft, thetaaft) = typecheck qtfbl varntenv tyenv_new utastaft in
        (LetMutableIn(varnm, eini, eaft), tyaft, thetaaft @@ thetaini)

  | UTOverwrite(varrng, varnm, utastnew) ->
      let (_, tyvar, _) = typecheck qtfbl varntenv tyenv (varrng, UTContentOf(varnm)) in
      let (enew, tynew, thetanew) = typecheck qtfbl varntenv tyenv utastnew in
      let thetasub = Subst.unify tyvar (get_range utastnew, RefType(tynew)) in
          (*  actually 'get_range utastnew' is not good
              since the right side expression has type 't, not 't ref *)
        (Overwrite(varnm, enew), (rng, UnitType), thetasub @@ thetanew)

  | UTSequential(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let thetau1 = (Subst.unify ty1 (get_range utast1, UnitType)) @@ theta1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv (thetau1 @=> tyenv) utast2 in
        (Sequential(e1, e2), thetau1 @> ty2, theta2 @@ thetau1)

  | UTWhileDo(utastb, utastc) ->
      let (eb, tyb, thetab) = typecheck qtfbl varntenv tyenv utastb in
      let thetaub = (Subst.unify tyb (get_range utastb, BoolType)) @@ thetab in
      let (ec, tyc, thetac) = typecheck qtfbl varntenv (thetaub @=> tyenv) utastc in
      let thetav = Subst.unify tyc (get_range utastc, UnitType) in
        (WhileDo(eb, ec), (rng, UnitType), thetav @@ thetac @@ thetaub)

  | UTLazyContent(utast1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
        (LazyContent(e1), ty1, theta1)

(* ---- final reference ---- *)

  | UTDeclareGlobalHash(utastkey, utastini) ->
      let (ekey, tykey, thetak) = typecheck qtfbl varntenv tyenv utastkey in
      let thetauk = (Subst.unify tykey (get_range utastkey, StringType)) @@ thetak in
      let (eini, tyini, thetai) = typecheck qtfbl varntenv (thetauk @=> tyenv) utastini in
      let thetav = Subst.unify tyini (get_range utastini, StringType) in
        (DeclareGlobalHash(ekey, eini), (rng, UnitType), thetav @@ thetai @@ thetauk)

  | UTOverwriteGlobalHash(utastkey, utastnew) ->
      let (ekey, tykey, thetak) = typecheck qtfbl varntenv tyenv utastkey in
      let thetauk = (Subst.unify tykey (get_range utastkey, StringType)) @@ thetak in
      let (enew, tynew, thetan) = typecheck qtfbl varntenv (thetauk @=> tyenv) utastnew in
      let thetav = Subst.unify tynew (get_range utastnew, StringType) in
        (OverwriteGlobalHash(ekey, enew), (rng, UnitType), thetav @@ thetan @@ thetauk)

  | UTReferenceFinal(utast1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let thetau = Subst.unify ty1 (rng, StringType) in
        (ReferenceFinal(e1), (rng, StringType), thetau @@ theta1)

(* ---- class/id option ---- *)

  | UTApplyClassAndID(utastcls, utastid, utast1) ->
      let dr = Range.dummy "ut-apply-class-and-id" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (dr, VariantType([(dr, StringType)], "maybe")) in
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (dr, VariantType([(dr, StringType)], "maybe")) in
      let (ecls, _, _) = typecheck qtfbl varntenv tyenv utastcls in
      let (eid, _, _)  = typecheck qtfbl varntenv tyenv utastid in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv_new utast1 in
        (ApplyClassAndID(ecls, eid, e1), ty1, theta1)

  | UTClassAndIDRegion(utast1) ->
      let dr = Range.dummy "ut-class-and-id-region" in
      let tyenv1    = Typeenv.add tyenv  "class-name" (dr, VariantType([(dr, StringType)], "maybe")) in
      let tyenv_new = Typeenv.add tyenv1 "id-name"    (dr, VariantType([(dr, StringType)], "maybe")) in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv_new utast1 in
        (e1, ty1, theta1)

(* ---- lightweight itemize ---- *)

  | UTItemize(utitmz) ->
      let (eitmz, thetaitmz) = typecheck_itemize qtfbl varntenv tyenv utitmz Subst.empty in
        (eitmz, (rng, VariantType([], "itemize")), thetaitmz)

(* ---- list ---- *)

  | UTListCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck qtfbl varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck qtfbl varntenv (thetahd @=> tyenv) utasttl in
      let thetau = Subst.unify tytl (Range.dummy "list-cons", ListType(tyhd)) in
        let type_result = (rng, ListType((thetau @@ thetatl) @> tyhd)) in
          (ListCons(ehd, etl), type_result, thetau @@ thetatl @@ thetahd)

  | UTEndOfList ->
      let ntyvar = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (EndOfList, (rng, ListType(ntyvar)), Subst.empty)

(* ---- tuple ---- *)

  | UTTupleCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck qtfbl varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck qtfbl varntenv (thetahd @=> tyenv) utasttl in
      let theta_result = thetatl @@ thetahd in
      let type_result =
        match tytl with
        | (rngtl, ProductType(tylist)) -> (rng, ProductType(tyhd :: tylist))
        | _                            -> assert false
      in
        (TupleCons(ehd, etl), type_result, theta_result)

  | UTEndOfTuple -> (EndOfTuple, (rng, ProductType([])), Subst.empty)

(* ---- other fundamentals ---- *)

  | UTPatternMatch(utastobj, utpmcons) ->
      let (eobj, tyobj, thetaobj) = typecheck qtfbl varntenv tyenv utastobj in
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(Tyvarid.fresh qtfbl)) in
      let (pmcons, typm, thetapm) = typecheck_pattern_match_cons qtfbl varntenv (thetaobj @=> tyenv) utpmcons tyobj thetaobj beta in
        (PatternMatch(eobj, pmcons), typm, thetapm)

  | UTDeclareVariantIn(mutvarntcons, utastaft) ->
      let varntenv_new = Variantenv.add_mutual_cons GlobalScope varntenv mutvarntcons in
        typecheck qtfbl varntenv_new tyenv utastaft

  | UTModule(mdlnm, utmdltr, utastaft) ->
      let (varntenv_new, tyenv_new, emdltr, thetadef) = typecheck_module qtfbl varntenv tyenv varntenv tyenv mdlnm utmdltr in
      let (eaft, tyaft, thetaaft) = typecheck qtfbl varntenv_new tyenv_new utastaft in
        (Module(mdlnm, emdltr, eaft), tyaft, thetaaft @@ thetadef)


and typecheck_itemize qtfbl varntenv tyenv (UTItem(utast1, utitmzlst)) (acctheta : Subst.t) =
  let tyenv1 = acctheta @=> tyenv in
  let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv1 utast1 in
  let thetau1a = (Subst.unify ty1 (Range.dummy "typecheck_itemize_string", StringType)) @@ theta1 @@ acctheta in
  let (elst, thetalst)  = typecheck_itemize_list qtfbl varntenv (thetau1a @=> tyenv1) utitmzlst thetau1a in
    (Constructor("Item", TupleCons(e1, TupleCons(elst, EndOfTuple))), thetalst)


and typecheck_itemize_list qtfbl varntenv tyenv (utitmzlst : untyped_itemize list) (acctheta : Subst.t) =
  match utitmzlst with
  | []                  -> (EndOfList, acctheta)
  | hditmz :: tlitmzlst ->
      let (ehd, thetahd) = typecheck_itemize qtfbl varntenv tyenv hditmz acctheta in
      let (etl, thetatl) = typecheck_itemize_list qtfbl varntenv (thetahd @=> tyenv) tlitmzlst thetahd in
        (ListCons(ehd, etl), thetatl)


and typecheck_module
    qtfbl (veout : Variantenv.t) (teout : Typeenv.t) (vein : Variantenv.t) (tein : Typeenv.t)
    (mdlnm : module_name) (utmdltr : untyped_module_tree)
=
  let (rng, utmdldef) = utmdltr in
  match utmdldef with

  | UTMFinishModule -> (veout, teout, MFinishModule, Subst.empty)

  | UTMDirectLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let teout_new = add_list_to_type_environment "" teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MDirectLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPublicLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let teout_new = add_list_to_type_environment mdlnm teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPublicLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPrivateLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, _, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPrivateLetIn(mutletcons, eaft), thetaaft @@ thetaout)

  | UTMPublicDeclareVariantIn(utmutvarntcons, utmdlaft) ->
      let vein_new  = Variantenv.add_mutual_cons (LocalScope(mdlnm)) vein utmutvarntcons in
      let veout_new = Variantenv.add_mutual_cons_hidden mdlnm veout utmutvarntcons in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout_new teout vein_new tein mdlnm utmdlaft in
        (veout_result, teout_result, eaft, thetaaft)

  | UTMPrivateDeclareVariantIn(utmutvarntcons, utmdlaft)  ->
      let vein_new  = Variantenv.add_mutual_cons (LocalScope(mdlnm)) vein utmutvarntcons in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout vein_new tein mdlnm utmdlaft in
        (veout_result, teout_result, eaft, thetaaft)

  | UTMPublicLetMutableIn(varrng, varnm, utini, utmdlaft) ->
      let (tein_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable vein tein varrng varnm utini in
      let teout_new = add_list_to_type_environment mdlnm teout [(varnm, (varrng, RefType(tyini)))] in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout_new vein tein_new mdlnm utmdlaft in
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), thetaaft @@ thetaini)

  | UTMPrivateLetMutableIn(varrng, varnm, utini, utmdlaft) ->
      let (tein_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable vein tein varrng varnm utini in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout vein tein_new mdlnm utmdlaft in
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), thetaaft @@ thetaini)


and add_list_to_type_environment (mdlnm : module_name) (tyenv : Typeenv.t) (tvtylst : (var_name * type_struct) list) =
  match tvtylst with
  | []                         -> tyenv
  | (varnm, tystr) :: tvtytail ->
      add_list_to_type_environment mdlnm (Typeenv.add tyenv (Variantenv.append_module_name mdlnm varnm) tystr) tvtytail


(* Typeenv.t -> untyped_pattern_match_cons -> type_struct -> Subst.t -> type_struct
    -> (pattern_match_cons * type_struct * Subst.t) *)
and typecheck_pattern_match_cons
    qtfbl varntenv tyenv (utpmcons : untyped_pattern_match_cons) (tyobj : type_struct) (acctheta : Subst.t) (tyres : type_struct) =
  match utpmcons with
  | UTEndOfPatternMatch -> (EndOfPatternMatch, tyres, acctheta)

  | UTPatternMatchCons(utpat, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let thetau = Subst.unify typat tyobj in
      let thetaua = thetau @@ acctheta in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv (thetaua @=> tyenvpat) utast1 in
      let thetav1ua = (Subst.unify ty1 tyres) @@ theta1 @@ thetaua in
      let (pmctl, tytl, thetatl) =
            typecheck_pattern_match_cons qtfbl varntenv (thetav1ua @=> tyenv) tailcons (thetav1ua @> tyobj) thetav1ua (thetav1ua @> tyres) in
        (PatternMatchCons(epat, e1, pmctl), tytl, thetatl)

  | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let thetau = Subst.unify typat tyobj in
      let (eb, tyb, thetab) = typecheck qtfbl varntenv tyenvpat utastb in
      let thetawbua = (Subst.unify tyb (Range.dummy "pattern-match-cons-when", BoolType)) @@ thetab @@ thetau @@ acctheta in
      let tyenv1 = thetawbua @=> tyenvpat in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv1 utast1 in
      let thetav1wbua = (Subst.unify ty1 tyres) @@ theta1 @@ thetawbua in
      let (pmctl, tytl, thetatl) =
            typecheck_pattern_match_cons qtfbl varntenv (thetav1wbua @=> tyenv) tailcons (thetav1wbua @> tyobj) thetav1wbua (thetav1wbua @> tyres) in
        (PatternMatchConsWhen(epat, eb, e1, pmctl), tytl, thetatl)


and typecheck_pattern qtfbl varntenv tyenv (rng, utpatmain) =
  match utpatmain with
  | UTPNumericConstant(nc) -> (PNumericConstant(nc), (rng, IntType), tyenv)
  | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BoolType), tyenv)
  | UTPStringConstant(ut1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv ut1 in
      let thetau1 = (Subst.unify (Range.dummy "pattern-string-constant", StringType) ty1) @@ theta1 in
        (PStringConstant(e1), (rng, StringType), thetau1 @=> tyenv)

  | UTPUnitConstant        -> (PUnitConstant, (rng, UnitType), tyenv)

  | UTPListCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
      let (epat2, typat2, tyenv2) = typecheck_pattern qtfbl varntenv tyenv1 utpat2 in
        let thetau = Subst.unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
          (PListCons(epat1, epat2), thetau @> typat2, thetau @=> tyenv2)

  | UTPEndOfList ->
      let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (PEndOfList, (rng, ListType(beta)), tyenv)

  | UTPTupleCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
      let (epat2, typat2, tyenv2) = typecheck_pattern qtfbl varntenv tyenv1 utpat2 in
      let type_result =
        match typat2 with
        | (rng, ProductType(tylist)) -> (rng, ProductType(typat1 :: tylist))
        | _                          -> assert false
      in
        (PTupleCons(epat1, epat2), type_result, tyenv2)

  | UTPEndOfTuple -> (PEndOfTuple, (rng, ProductType([])), tyenv)

  | UTPWildCard ->
      let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (PWildCard, beta, tyenv)

  | UTPVariable(varnm) ->
      let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (PVariable(varnm), beta, Typeenv.add tyenv varnm beta)

  | UTPAsVariable(varnm, utpat1) ->
      let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
      let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
        (PAsVariable(varnm, epat1), typat1, Typeenv.add tyenv varnm beta)

  | UTPConstructor(constrnm, utpat1) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist) = Typeenv.make_bounded_free qtfbl tyforall in
          let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
          let thetau = Subst.unify tyfree typat1 in
            (PConstructor(constrnm, epat1), thetau @> (rng, VariantType(tyarglist, varntnm)), thetau @=> tyenv1)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end


and make_type_environment_by_let qtfbl (varntenv : Variantenv.t) (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) =
  let rec add_mutual_variables acctyenv (mutletcons : untyped_mutual_let_cons) =
    let iter = add_mutual_variables in
      match mutletcons with
      | UTEndOfMutualLet                             -> (acctyenv, [])
      | UTMutualLetCons(_, varnm, astdef, tailcons)  ->
          let beta = (get_range astdef, TypeVariable(Tyvarid.fresh qtfbl)) in
          let (tyenv_tail, tvtylst) = iter (Typeenv.add acctyenv varnm beta) tailcons in
            (tyenv_tail, ((varnm, beta) :: tvtylst))
  in
  let rec typecheck_mutual_contents
      (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) (tvtylst : (var_name * type_struct) list)
      (accthetain : Subst.t) (accthetaout : Subst.t) acctvtylstout
  =
    match (utmutletcons, tvtylst) with
    | (UTEndOfMutualLet, []) -> (tyenv, EndOfMutualLet, accthetain, accthetaout, List.rev acctvtylstout)

    | (UTMutualLetCons(tyopt, varnm, utast1, tailcons), (_, tvty) :: tvtytail) ->
        let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
        begin
          match tyopt with
          | None            ->
              let theta1in  = (Subst.unify ty1 tvty) @@ theta1 @@ accthetain in
              let theta1out = theta1 @@ accthetaout in
                let tyenv_new = Typeenv.add (theta1in @=> tyenv) varnm ty1 in
                let (tyenvfinal, mutletcons_tail, thetainfinal, thetaoutfinal, tvtylstoutfinal) =
                      typecheck_mutual_contents tyenv_new tailcons tvtytail theta1in theta1out ((varnm, tvty) :: acctvtylstout) in
                  (tyenvfinal, MutualLetCons(varnm, e1, mutletcons_tail), thetainfinal, thetaoutfinal, tvtylstoutfinal)
(*
          | Some(tystrmanu) ->
              let (tystrforin, tystrforout) = Variantenv.fix_manual_type_for_inner_and_outer qtfbl varntenv tystrmanu in
              let theta1in  = (Subst.unify tystrforin tvty) @@ (Subst.unify ty1 tvty) @@ theta1 in
              let theta1out = theta1 in
                let tyenv_new = Typeenv.add (theta1in @=> tyenv) varnm ty1 in
                let (tyenv_tail, mutletcons_tail, thetain_tail, thetaout_tail, tvtylstout_tail) =
                      typecheck_mutual_contents tyenv_new tailcons tvtytail in
                  let thetain_result  = thetain_tail @@ theta1in in
                  let thetaout_result = thetaout_tail @@ theta1out in
                  let tyenv_result    = thetain_result @=> tyenv_tail in
                  let tvtylstout_result = (varnm, tystrforout) :: tvtylstout_tail in
                    (tyenv_result, MutualLetCons(varnm, e1, mutletcons_tail), thetain_result, thetaout_result, tvtylstout_result)
*)
          end

    | _ -> assert false
  in
  (* Typeenv.t -> Typeenv.t -> Subst.t -> (var_name * type_struct) list ->
      (var_name * type_struct) list -> (Typeenv.t * ((var_name * type_struct) list) *)
  let rec make_forall_type_mutual tyenv tyenv_before_let (theta : Subst.t) tvtylst tvtylst_forall =
    match tvtylst with
    | []                        -> (tyenv, tvtylst_forall)
    | (varnm, tvty) :: tvtytail ->
        let prety = theta @> tvty in
          begin                                                                                                        (* for debug *)
            print_for_debug_typecheck (Subst.string_of_subst theta) ;                                                  (* for debug *)
            print_for_debug_typecheck ("#MakeForall " ^ varnm ^ " : " ^ (string_of_type_struct_basic prety) ^ "\n") ;  (* for debug *)
            let forallty  = Typeenv.erase_range_of_type (Typeenv.make_forall_type prety tyenv_before_let) in
  (*          let forallty  = Typeenv.make_forall_type prety tyenv_before_let in                              (* for test *) *)
            let tyenv_new = Typeenv.add tyenv varnm forallty in
            let tvtylst_forall_new = (varnm, forallty) :: tvtylst_forall in
              make_forall_type_mutual tyenv_new tyenv_before_let theta tvtytail tvtylst_forall_new
          end                                                                                                          (* for debug *)
  in
  let (tyenvforrec, tvtylstforrec) = add_mutual_variables tyenv utmutletcons in
  let (tyenv_new, mutletcons, thetain, thetaout, tvtylstout) =
        typecheck_mutual_contents tyenvforrec utmutletcons tvtylstforrec Subst.empty Subst.empty [] in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual tyenv_new tyenv thetain tvtylstout [] in
    (tyenv_forall, tvtylst_forall, mutletcons, thetain, thetaout)


(* Variantenv.t -> Typeenv.t -> code_range -> var_name -> untyped_abstract_tree ->
    (Typeenv.t * abstract_tree * type_struct * Subst.t) *)
and make_type_environment_by_let_mutable varntenv tyenv varrng varnm utastini =
  let (eini, tyini, thetaini) = typecheck Tyvarid.Unquantifiable varntenv tyenv utastini in
    begin                                                                                       (* for debug *)
      print_for_debug_typecheck ("#LetMutable " ^ (string_of_type_struct_basic tyini) ^ "\n") ; (* for debug *)
      let tyenv_new = thetaini @=> (Typeenv.add tyenv varnm (varrng, RefType(tyini))) in
        (tyenv_new, eini, tyini, thetaini)
    end                                                                                         (* for debug *)


(* untyped_abstract_tree -> (type_struct * Variantenv.t * Typeenv.t) *)
let main varntenv tyenv utast =
    begin
      final_varntenv := varntenv ;
      final_tyenv := tyenv ;
      let (e, ty, theta) = typecheck Tyvarid.Quantifiable varntenv tyenv utast in
        (ty, !final_varntenv, !final_tyenv, e)
    end
