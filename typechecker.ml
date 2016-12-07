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


let report_error_with_range rng msg =
  raise (Error("at " ^ (Range.to_string rng) ^ ":\n    " ^ msg))


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
            begin                                                                                       (* for debug *)
              print_for_debug_typecheck ("#C " ^ varnm ^ " : " ^ (string_of_type_struct_basic tyforall) (* for debug *)
                ^ " = " ^ (string_of_type_struct_basic ty) ^ "\n") ;                                    (* for debug *)
              print_for_debug_typecheck ((Range.to_string rng) ^ "\n") ;                                (* for debug *)
              (ContentOf(varnm), ty, Subst.empty)
            end                                                                                         (* for debug *)
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
            let theta_result    = Subst.compose (Subst.unify tycont tyvarnt) thetacont in
            let type_result_sub = Subst.apply_to_type_struct theta_result (rng, VariantType(tyarglist, varntnm)) in
            let type_result     = Typeenv.overwrite_range_of_type type_result_sub rng in
            begin                                                                                         (* for debug *)
              print_for_debug_typecheck ("#V " ^ varntnm ^ " : " ^ (string_of_type_struct_basic tyforall) (* for debug *)
                ^ " = " ^ (string_of_type_struct_basic type_result) ^ "\n") ;                             (* for debug *)
              print_for_debug_typecheck ((Range.to_string rng) ^ "\n") ;                                  (* for debug *)
              (Constructor(constrnm, econt), type_result, theta_result)
            end                                                                                           (* for debug *)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end

  | UTConcat(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv utast2 in
      let theta3 = Subst.unify ty1 (get_range utast1, StringType) in
      let theta4 = Subst.unify ty2 (get_range utast2, StringType) in
      let theta_result = Subst.compose theta4 (Subst.compose theta3 (Subst.compose theta2 theta1)) in
        (Concat(e1, e2), (rng, StringType), theta_result)

  | UTApply(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv utast2 in
        begin
          match ty1 with
          | (_, FuncType(tydom, tycod)) ->
              let theta3 = Subst.unify ty2 tydom in
                let theta_result    = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let type_result_sub = Subst.apply_to_type_struct theta_result tycod in
                let type_result     = Typeenv.overwrite_range_of_type type_result_sub rng in
                  begin                                                                               (* for debug *)
                    print_for_debug_typecheck ("\n%Apply1 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " (* for debug *)
                      ^ (string_of_type_struct_basic type_result) ^ "\n") ;                           (* for debug *)
                    print_for_debug_typecheck ((Subst.string_of_subst theta_result) ^ "\n") ;         (* for debug *)
                      (Apply(e1, e2), type_result, theta_result)
                  end                                                                                 (* for debug *)
          | _ ->
              let beta = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
              let theta3 = Subst.unify ty1 (get_range utast1, FuncType(ty2, beta)) in
                let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
                let type_result  = Subst.apply_to_type_struct theta_result beta in
                  begin                                                                               (* for debug *)
                    print_for_debug_typecheck ("\n%Apply2 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " (* for debug *)
                      ^ (string_of_type_struct_basic beta) ^ " = "                                    (* for debug *)
                      ^ (string_of_type_struct_basic type_result) ^ "\n") ;                           (* for debug *)
                    print_for_debug_typecheck ((Subst.string_of_subst theta_result) ^ "\n") ;         (* for debug *)
                      (Apply(e1, e2), type_result, theta_result)
                  end                                                                                 (* for debug *)
        end

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let beta = (varrng, TypeVariable(Tyvarid.fresh qtfbl)) in
      let tyenv_new = Typeenv.add tyenv varnm beta in
        let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv_new utast1 in
          let tydom = (Subst.apply_to_type_struct theta1 beta) in
          let tycod = ty1 in
          let theta_result = theta1 in
            (LambdaAbstract(varnm, e1), (rng, FuncType(tydom, tycod)), theta_result)

  | UTLetIn(utmutletcons, utast2) ->
      let (tyenv_forall, _, mutletcons, theta1, _) = make_type_environment_by_let qtfbl varntenv tyenv utmutletcons in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv_forall utast2 in
        (LetIn(mutletcons, e2), ty2, Subst.compose theta2 theta1)

  | UTIfThenElse(utastb, utast1, utast2) ->
      let (eb, tyb, thetab) = typecheck qtfbl varntenv tyenv utastb in
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv utast2 in
      let theta_result =  Subst.compose (Subst.unify ty2 ty1)
                            (Subst.compose theta2
                              (Subst.compose theta1
                                (Subst.compose (Subst.unify tyb (Range.dummy "if-bool", BoolType))
                                  thetab))) in
      let type_result = Subst.apply_to_type_struct theta_result ty1 in
        (IfThenElse(eb, e1, e2), type_result, theta_result)

(* ---- impleratives ---- *)

  | UTLetMutableIn(varrng, varnm, utastini, utastaft) ->
      let (tyenv_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable varntenv tyenv varrng varnm utastini in
      let (eaft, tyaft, thetaaft) = typecheck qtfbl varntenv tyenv_new utastaft in
        let theta_result = Subst.compose thetaaft thetaini in
        let type_result  = Subst.apply_to_type_struct theta_result tyaft in
          (LetMutableIn(varnm, eini, eaft), type_result, theta_result)

  | UTOverwrite(varrng, varnm, utastnew) ->
      let (_, tyvar, _) = typecheck qtfbl varntenv tyenv (varrng, UTContentOf(varnm)) in
      let (enew, tynew, thetanew) = typecheck qtfbl varntenv tyenv utastnew in
      let thetasub = Subst.unify tyvar (get_range utastnew, RefType(tynew)) in
          (*  actually 'get_range utastnew' is not good
              since the right side expression has type 't, not 't ref *)
        (Overwrite(varnm, enew), (rng, UnitType), Subst.compose thetasub thetanew)

  | UTSequential(utast1, utast2) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let theta_new = Subst.compose (Subst.unify ty1 (get_range utast1, UnitType)) theta1 in
      let tyenv_new = Subst.apply_to_type_environment theta_new tyenv in
      let (e2, ty2, theta2) = typecheck qtfbl varntenv tyenv_new utast2 in
        let theta_result = Subst.compose theta2 theta_new in
        let type_result = Subst.apply_to_type_struct theta_result ty2 in
            (Sequential(e1, e2), type_result, theta_result)

  | UTWhileDo(utastb, utastc) ->
      let (eb, tyb, thetab) = typecheck qtfbl varntenv tyenv utastb in
      let (ec, tyc, thetac) = typecheck qtfbl varntenv tyenv utastc in
        let thetabsub = Subst.unify tyb (get_range utastb, BoolType) in
        let thetacsub = Subst.unify tyc (get_range utastc, UnitType) in
          let theta_result =  Subst.compose thetacsub
                                (Subst.compose thetabsub
                                  (Subst.compose thetac thetab)) in
            (WhileDo(eb, ec), (rng, UnitType), theta_result)

  | UTLazyContent(utast1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
        (LazyContent(e1), ty1, theta1)

(* ---- final reference ---- *)

  | UTDeclareGlobalHash(utastkey, utastini) ->
      let (ekey, tykey, thetakey) = typecheck qtfbl varntenv tyenv utastkey in
      let (eini, tyini, thetaini) = typecheck qtfbl varntenv tyenv utastini in
      let thetasubkey = Subst.unify tykey (get_range utastkey, StringType) in
      let thetasubini = Subst.unify tyini (get_range utastini, StringType) in
      let theta_result =  Subst.compose_list [thetasubini; thetasubkey; thetaini; thetakey] in
        (DeclareGlobalHash(ekey, eini), (rng, UnitType), theta_result)

  | UTOverwriteGlobalHash(utastkey, utastnew) ->
      let (ekey, tykey, thetakey) = typecheck qtfbl varntenv tyenv utastkey in
      let (enew, tynew, thetanew) = typecheck qtfbl varntenv tyenv utastnew in
      let thetasubkey = Subst.unify tykey (get_range utastkey, StringType) in
      let thetasubnew = Subst.unify tynew (get_range utastnew, StringType) in
      let theta_result =  Subst.compose_list [thetasubnew; thetasubkey; thetanew; thetakey] in
        (OverwriteGlobalHash(ekey, enew), (rng, UnitType), theta_result)

  | UTReferenceFinal(utast1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
      let thetasub = Subst.unify ty1 (rng, StringType) in
      let theta_result = Subst.compose thetasub theta1 in
        (ReferenceFinal(e1), (rng, StringType), theta_result)

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
      let (eitmz, thetaitmz) = typecheck_itemize qtfbl varntenv tyenv utitmz in
        (eitmz, (rng, VariantType([], "itemize")), thetaitmz)

(* ---- list ---- *)

  | UTListCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck qtfbl varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck qtfbl varntenv tyenv utasttl in
        let theta_result =  Subst.compose (Subst.unify tytl (Range.dummy "list-cons", ListType(tyhd)))
                              (Subst.compose thetatl thetahd) in
        let type_result = (rng, ListType(Subst.apply_to_type_struct theta_result tyhd)) in
          (ListCons(ehd, etl), type_result, theta_result)

  | UTEndOfList ->
      let ntyvar = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (EndOfList, (rng, ListType(ntyvar)), Subst.empty)

(* ---- tuple ---- *)

  | UTTupleCons(utasthd, utasttl) ->
      let (ehd, tyhd, thetahd) = typecheck qtfbl varntenv tyenv utasthd in
      let (etl, tytl, thetatl) = typecheck qtfbl varntenv tyenv utasttl in
      let theta_result = Subst.compose thetatl thetahd in
      let term_result = (TupleCons(ehd, etl)) in
      let type_result = Subst.apply_to_type_struct theta_result
        begin
          match tytl with
          | (rngtl, ProductType(tylist)) -> (rng, ProductType(tyhd :: tylist))
          | _                            -> assert false
        end
      in
        (term_result, type_result, theta_result)

  | UTEndOfTuple -> (EndOfTuple, (rng, ProductType([])), Subst.empty)

(* ---- other fundamentals ---- *)

  | UTPatternMatch(utastobj, utpmcons) ->
      let (eobj, tyobj, thetaobj) = typecheck qtfbl varntenv tyenv utastobj in
      let ntv = (Range.dummy "ntv", TypeVariable(Tyvarid.fresh qtfbl)) in
      let (pmcons, typm, thetapm) = typecheck_pattern_match_cons qtfbl varntenv tyenv utpmcons tyobj thetaobj ntv in
        (PatternMatch(eobj, pmcons), typm, thetapm)

  | UTDeclareVariantIn(mutvarntcons, utastaft) ->
      let varntenv_new = Variantenv.add_mutual_cons GlobalScope varntenv mutvarntcons in
        typecheck qtfbl varntenv_new tyenv utastaft

  | UTModule(mdlnm, utmdltr, utastaft) ->
      let (varntenv_new, tyenv_new, emdltr, thetadef) = typecheck_module qtfbl varntenv tyenv varntenv tyenv mdlnm utmdltr in
      let (eaft, tyaft, thetaaft) = typecheck qtfbl varntenv_new tyenv_new utastaft in
      let theta_result = Subst.compose thetaaft thetadef in
      let type_result  = Subst.apply_to_type_struct theta_result tyaft in
        (Module(mdlnm, emdltr, eaft), type_result, theta_result)


and typecheck_itemize qtfbl varntenv tyenv (UTItem(utast1, utitmzlst)) =
    let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
    let (elst, thetalst)  = typecheck_itemize_list qtfbl varntenv tyenv utitmzlst in
      let theta_result = Subst.compose thetalst (Subst.compose theta1 (Subst.unify ty1 (Range.dummy "typecheck_itemize_string", StringType))) in
        (Constructor("Item", TupleCons(e1, TupleCons(elst, EndOfTuple))), theta_result)

and typecheck_itemize_list qtfbl varntenv tyenv utitmzlst =
  match utitmzlst with
  | []                  -> (EndOfList, Subst.empty)
  | hditmz :: tlitmzlst ->
      let (ehd, thetahd) = typecheck_itemize qtfbl varntenv tyenv hditmz in
      let (etl, thetatl) = typecheck_itemize_list qtfbl varntenv tyenv tlitmzlst in
        (ListCons(ehd, etl), Subst.compose thetatl thetahd)


(* Variantenv.t -> Typeenv.t -> Variantenv.t -> Typeenv.t -> module_name -> untyped_module_tree
    -> (Variantenv.t * Typeenv.t * module_tree * Subst.t) *)
and typecheck_module qtfbl (veout : Variantenv.t) (teout : Typeenv.t) (vein : Variantenv.t) (tein : Typeenv.t) (mdlnm : module_name) (utmdltr : untyped_module_tree) =
  let (rng, utmdldef) = utmdltr in
  match utmdldef with

  | UTMFinishModule -> (veout, teout, MFinishModule, Subst.empty)

  | UTMDirectLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let teout_new = add_list_to_type_environment "" teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MDirectLetIn(mutletcons, eaft), Subst.compose thetaaft thetaout)

  | UTMPublicLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, tvtylstout, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let teout_new = add_list_to_type_environment mdlnm teout tvtylstout in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout_new vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPublicLetIn(mutletcons, eaft), Subst.compose thetaaft thetaout)

  | UTMPrivateLetIn(utmutletcons, utmdlaft) ->
      let (tein_new, _, mutletcons, thetain, thetaout) = make_type_environment_by_let qtfbl vein tein utmutletcons in
        let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout vein tein_new mdlnm utmdlaft in
          (veout_result, teout_result, MPrivateLetIn(mutletcons, eaft), Subst.compose thetaaft thetaout)

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
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), Subst.compose thetaaft thetaini)

  | UTMPrivateLetMutableIn(varrng, varnm, utini, utmdlaft) ->
      let (tein_new, eini, tyini, thetaini) = make_type_environment_by_let_mutable vein tein varrng varnm utini in
      let (veout_result, teout_result, eaft, thetaaft) = typecheck_module qtfbl veout teout vein tein_new mdlnm utmdlaft in
        (veout_result, teout_result, MPublicLetMutableIn(varnm, eini, eaft), Subst.compose thetaaft thetaini)


(* module_name -> Typeenv.t -> (var_name * type_struct) list -> Typeenv.t *)
and add_list_to_type_environment (mdlnm : module_name) (tyenv : Typeenv.t) (tvtylst : (var_name * type_struct) list) =
  match tvtylst with
  | []                         -> tyenv
  | (varnm, tystr) :: tvtytail ->
      add_list_to_type_environment mdlnm (Typeenv.add tyenv (Variantenv.append_module_name mdlnm varnm) tystr) tvtytail


(* Typeenv.t -> untyped_pattern_match_cons -> type_struct -> Subst.t -> type_struct
    -> (pattern_match_cons * type_struct * Subst.t) *)
and typecheck_pattern_match_cons qtfbl varntenv tyenv utpmcons tyobj theta tyres =
  match utpmcons with
  | UTEndOfPatternMatch -> (EndOfPatternMatch, (Subst.apply_to_type_struct theta tyres), theta)

  | UTPatternMatchCons(utpat, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let thetapat  = Subst.compose (Subst.unify typat tyobj) theta in
      let tyenv1    = Subst.apply_to_type_environment thetapat tyenvpat in
      let (e1, ty1, theta1)       = typecheck qtfbl varntenv tyenv1 utast1 in
      let theta2    = Subst.compose (Subst.unify ty1 tyres) (Subst.compose theta1 thetapat) in
      let tyres_new = Subst.apply_to_type_struct theta2 tyres in
      let tyobj_new = Subst.apply_to_type_struct theta2 tyobj in
      let (pmctl, tytl, thetatl)  = typecheck_pattern_match_cons qtfbl varntenv tyenv tailcons tyobj_new theta2 tyres_new in
        (PatternMatchCons(epat, e1, pmctl), tytl, thetatl)

  | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
      let (epat, typat, tyenvpat) = typecheck_pattern qtfbl varntenv tyenv utpat in
      let (eb, tyb, thetab)       = typecheck qtfbl varntenv tyenvpat utastb in
      let thetapat  = Subst.compose (Subst.unify tyb (Range.dummy "pattern-match-cons-when", BoolType))
                        (Subst.compose thetab
                          (Subst.compose (Subst.unify typat tyobj) theta)) in
      let tyenv1    = Subst.apply_to_type_environment thetapat tyenvpat in
      let (e1, ty1, theta1)       = typecheck qtfbl varntenv tyenv1 utast1 in
      let theta2    = Subst.compose (Subst.unify ty1 tyres) (Subst.compose theta1 thetapat) in
      let tyres_new = Subst.apply_to_type_struct theta2 tyres in
      let tyobj_new = Subst.apply_to_type_struct theta2 tyobj in
      let (pmctl, tytl, thetatl)  = typecheck_pattern_match_cons qtfbl varntenv tyenv tailcons tyobj_new theta2 tyres_new in
        (PatternMatchConsWhen(epat, eb, e1, pmctl), tytl, thetatl)


(* Typeenv.t * untyped_pattern_tree -> (pattern_tree * type_struct * Typeenv.t) *)
and typecheck_pattern qtfbl varntenv tyenv (rng, utpatmain) =
  match utpatmain with
  | UTPNumericConstant(nc) -> (PNumericConstant(nc), (rng, IntType), tyenv)
  | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BoolType), tyenv)
  | UTPStringConstant(ut1) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv ut1 in
      let theta_new = Subst.compose (Subst.unify (Range.dummy "pattern-string-constant", StringType) ty1) theta1 in
      let tyenv_new = Subst.apply_to_type_environment theta_new tyenv in
        (PStringConstant(e1), (rng, StringType), tyenv_new)

  | UTPUnitConstant        -> (PUnitConstant, (rng, UnitType), tyenv)

  | UTPListCons(utpat1, utpat2) ->
      let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
      let (epat2, typat2, tyenv2) = typecheck_pattern qtfbl varntenv tyenv1 utpat2 in
        let theta = Subst.unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
        let tyenv_result = Subst.apply_to_type_environment theta tyenv2 in
        let type_result = Subst.apply_to_type_struct theta typat2 in
          (PListCons(epat1, epat2), type_result, tyenv_result)

  | UTPEndOfList ->
      let ntv = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in (PEndOfList, (rng, ListType(ntv)), tyenv)

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
      let ntv = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in (PWildCard, ntv, tyenv)

  | UTPVariable(varnm) ->
      let ntv = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
        (PVariable(varnm), ntv, Typeenv.add tyenv varnm ntv)

  | UTPAsVariable(varnm, utpat1) ->
      let ntv = (rng, TypeVariable(Tyvarid.fresh qtfbl)) in
      let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
        (PAsVariable(varnm, epat1), typat1, Typeenv.add tyenv varnm ntv)

  | UTPConstructor(constrnm, utpat1) ->
      begin
        try
          let (varntnm, tyforall) = Variantenv.find varntenv constrnm in
          let (tyfree, tyarglist) = Typeenv.make_bounded_free qtfbl tyforall in
          let (epat1, typat1, tyenv1) = typecheck_pattern qtfbl varntenv tyenv utpat1 in
          let theta = Subst.unify tyfree typat1 in
          let tyenv_new = Subst.apply_to_type_environment theta tyenv1 in
          let type_result = Subst.apply_to_type_struct theta (rng, VariantType(tyarglist, varntnm)) in
            (PConstructor(constrnm, epat1), type_result, tyenv_new)
        with
        | Not_found -> report_error_with_range rng ("undefined constructor '" ^ constrnm ^ "'")
      end


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons ->
    (Typeenv.t * (var_name * type_struct) list * mutual_let_cons * Subst.t) *)
and make_type_environment_by_let qtfbl (varntenv : Variantenv.t) (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) =
  let (tyenv_for_rec, tvtylst_for_rec) = add_mutual_variables qtfbl varntenv tyenv utmutletcons in
  let (tyenv_new, mutletcons, thetain, thetaout, tvtylstout) = typecheck_mutual_contents qtfbl varntenv tyenv_for_rec utmutletcons tvtylst_for_rec in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual varntenv tyenv_new tyenv thetain tvtylstout [] in
    (tyenv_forall, tvtylst_forall, mutletcons, thetain, thetaout)


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * ((var_name * type_struct) list)) *)
and add_mutual_variables qtfbl varntenv tyenv (mutletcons : untyped_mutual_let_cons) =
  match mutletcons with
  | UTEndOfMutualLet                             -> (tyenv, [])
  | UTMutualLetCons(_, varnm, astdef, tailcons)  ->
      let ntv = (get_range astdef, TypeVariable(Tyvarid.fresh qtfbl)) in
      let (tyenv_tail, tvtylst) = add_mutual_variables qtfbl varntenv (Typeenv.add tyenv varnm ntv) tailcons in
        (tyenv_tail, ((varnm, ntv) :: tvtylst))


(* Variantenv.t -> Typeenv.t -> untyped_mutual_let_cons -> ((var_name * type_struct) list)
  -> (Typeenv.t * mutual_let_cons * Subst.t * Subst.t) *)
and typecheck_mutual_contents qtfbl (varntenv : Variantenv.t) (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) (tvtylst : (var_name * type_struct) list) =
  match (utmutletcons, tvtylst) with
  | (UTEndOfMutualLet, []) -> (tyenv, EndOfMutualLet, Subst.empty, Subst.empty, [])

  | (UTMutualLetCons(tyopt, varnm, utast1, tailcons), (_, tvty) :: tvtytail) ->
      let (e1, ty1, theta1) = typecheck qtfbl varntenv tyenv utast1 in
        begin
          match tyopt with
          | None            ->
              let theta1in  = Subst.compose (Subst.unify ty1 tvty) theta1 in
              let theta1out = theta1 in
                let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1in tyenv) varnm ty1 in
                let (tyenv_tail, mutletcons_tail, thetain_tail, thetaout_tail, tvtylstout_tail) = typecheck_mutual_contents qtfbl varntenv tyenv_new tailcons tvtytail in
                let thetain_result  = Subst.compose thetain_tail theta1in in
                let thetaout_result = Subst.compose thetaout_tail theta1out in
                let tyenv_result    = Subst.apply_to_type_environment thetain_result tyenv_tail in
                let tvtylstout_result = (varnm, tvty) :: tvtylstout_tail in
                  (tyenv_result, MutualLetCons(varnm, e1, mutletcons_tail), thetain_result, thetaout_result, tvtylstout_result)
          | Some(tystrmanu) ->
              let (tystrforin, tystrforout) = Variantenv.fix_manual_type_for_inner_and_outer qtfbl varntenv tystrmanu in
              let theta1in  = Subst.compose (Subst.unify tystrforin tvty) (Subst.compose (Subst.unify ty1 tvty) theta1) in
              let theta1out = theta1 in
                let tyenv_new = Typeenv.add (Subst.apply_to_type_environment theta1in tyenv) varnm ty1 in
                let (tyenv_tail, mutletcons_tail, thetain_tail, thetaout_tail, tvtylstout_tail) = typecheck_mutual_contents qtfbl varntenv tyenv_new tailcons tvtytail in
                  let thetain_result  = Subst.compose thetain_tail theta1in in
                  let thetaout_result = Subst.compose thetaout_tail theta1out in
                  let tyenv_result    = Subst.apply_to_type_environment thetain_result tyenv_tail in
                  let tvtylstout_result = (varnm, tystrforout) :: tvtylstout_tail in
                    (tyenv_result, MutualLetCons(varnm, e1, mutletcons_tail), thetain_result, thetaout_result, tvtylstout_result)
        end

  | _ -> assert false


(* Variantenv.t -> Typeenv.t -> Typeenv.t -> Subst.t -> (var_name * type_struct) list ->
    (var_name * type_struct) list -> (Typeenv.t * ((var_name * type_struct) list) *)
and make_forall_type_mutual varntenv tyenv tyenv_before_let theta tvtylst tvtylst_forall =
  match tvtylst with
  | []                        -> (tyenv, tvtylst_forall)
  | (varnm, tvty) :: tvtytail ->
      let prety = Subst.apply_to_type_struct theta tvty in
        begin                                                                                               (* for debug *)
          print_for_debug_typecheck (Subst.string_of_subst theta) ;                                         (* for debug *)
          print_for_debug_typecheck (Typeenv.string_of_type_environment tyenv "MakeForall") ;               (* for debug *)
          print_for_debug_typecheck ("#M " ^ varnm ^ " : " ^ (string_of_type_struct_basic prety) ^ "\n") ;  (* for debug *)
          let forallty  = Typeenv.erase_range_of_type (Typeenv.make_forall_type prety tyenv_before_let) in
(*          let forallty  = Typeenv.make_forall_type prety tyenv_before_let in                              (* for test *) *)
          let tyenv_new = Typeenv.add tyenv varnm forallty in
          let tvtylst_forall_new = (varnm, forallty) :: tvtylst_forall in
            make_forall_type_mutual varntenv tyenv_new tyenv_before_let theta tvtytail tvtylst_forall_new
        end                                                                                       (* for debug *)


(* Variantenv.t -> Typeenv.t -> code_range -> var_name -> untyped_abstract_tree ->
    (Typeenv.t * abstract_tree * type_struct * Subst.t) *)
and make_type_environment_by_let_mutable varntenv tyenv varrng varnm utastini =
  let (eini, tyini, thetaini) = typecheck Tyvarid.Unquantifiable varntenv tyenv utastini in
    begin                                                                               (* for debug *)
      print_for_debug_typecheck ("#LM " ^ (string_of_type_struct_basic tyini) ^ "\n") ; (* for debug *)
      let tyenv_new = Subst.apply_to_type_environment thetaini (Typeenv.add tyenv varnm (varrng, RefType(tyini))) in
        (tyenv_new, eini, tyini, thetaini)
    end                                                                                 (* for debug *)


(* untyped_abstract_tree -> (type_struct * Variantenv.t * Typeenv.t) *)
let main varntenv tyenv utast =
    begin
      final_varntenv := varntenv ;
      final_tyenv := tyenv ;
      let (e, ty, theta) = typecheck Tyvarid.Quantifiable varntenv tyenv utast in
        (ty, !final_varntenv, !final_tyenv, e)
    end
