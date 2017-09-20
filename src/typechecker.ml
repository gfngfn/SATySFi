open Types
open Display

exception UndefinedVariable    of Range.t * var_name
exception UndefinedConstructor of Range.t * var_name
exception InclusionError       of Typeenv.t * mono_type * mono_type
exception ContradictionError   of Typeenv.t * mono_type * mono_type
exception InternalInclusionError
exception InternalContradictionError
exception InvalidArityOfCommand of Range.t * int * int


let print_for_debug_typecheck msg =
(*
  print_endline msg;
*)
  ()

let flatten_type ty =
  let rec aux acc ty =
    let (rng, tymain) = ty in
      match tymain with
      | FuncType(tydom, tycod) -> aux (tydom :: acc) tycod
      | _                      -> (List.rev acc, ty)
  in
    aux [] ty


let rec occurs (tvid : FreeID.t) ((_, tymain) : mono_type) =
  let iter = occurs tvid in
  let iter_list = List.fold_left (fun b ty -> b || iter ty) false in
  match tymain with
  | TypeVariable(tvref) ->
      begin
        match !tvref with
        | Link(tyl)   -> iter tyl
        | Bound(_)    -> false
        | Free(tvidx) ->
            if FreeID.equal tvidx tvid then true else
              let lev = FreeID.get_level tvid in
              let levx = FreeID.get_level tvidx in
              let () =
                (* -- update level -- *)
                if FreeID.less_than lev levx then
                  tvref := Free(FreeID.set_level tvidx lev)
                else
                  ()
              in
                false
      end
  | FuncType(tydom, tycod)         -> iter tydom || iter tycod
  | ProductType(tylist)            -> iter_list tylist
  | ListType(tysub)                -> iter tysub
  | RefType(tysub)                 -> iter tysub
  | VariantType(tylist, _)         -> iter_list tylist
  | SynonymType(tylist, _, tyreal) -> iter_list tylist || iter tyreal
  | RecordType(tyasc)              -> iter_list (Assoc.to_value_list tyasc)
  | BaseType(_)                    -> false
  | HorzCommandType(tylist)        -> iter_list tylist
  | VertCommandType(tylist)        -> iter_list tylist


let rec unify_sub ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) =
  let unify_list = List.iter (fun (t1, t2) -> unify_sub t1 t2) in
  let () = print_for_debug_typecheck ("| unify " ^ (string_of_mono_type_basic ty1) ^ " == " ^ (string_of_mono_type_basic ty2)) in (* for debug *)
    match (tymain1, tymain2) with

    | (SynonymType(_, _, tyreal1), _) -> unify_sub tyreal1 ty2
    | (_, SynonymType(_, _, tyreal2)) -> unify_sub ty1 tyreal2

    | (BaseType(bsty1), BaseType(bsty2))  when bsty1 = bsty2 -> ()

    | (FuncType(tydom1, tycod1), FuncType(tydom2, tycod2)) ->
        begin
          unify_sub tydom1 tydom2;
          unify_sub tycod1 tycod2;
        end

    | ( (ProductType(tylist1), ProductType(tylist2))
      | (HorzCommandType(tylist1), HorzCommandType(tylist2))
      | (VertCommandType(tylist1), VertCommandType(tylist2)) ) ->
        begin
          try
            unify_list (List.combine tylist1 tylist2)
          with
          | Invalid_argument(_) -> (* -- not of the same length -- *)
              raise InternalContradictionError
        end

    | (RecordType(tyasc1), RecordType(tyasc2)) ->
        if not (Assoc.domain_same tyasc1 tyasc2) then
          raise InternalContradictionError
        else
          unify_list (Assoc.combine_value tyasc1 tyasc2)

    | (VariantType(tyarglist1, tyid1), VariantType(tyarglist2, tyid2))
        when tyid1 = tyid2 ->
          unify_list (List.combine tyarglist1 tyarglist2)

    | (ListType(tysub1), ListType(tysub2)) -> unify_sub tysub1 tysub2

    | (RefType(tysub1), RefType(tysub2))   -> unify_sub tysub1 tysub2

    | (TypeVariable({contents= Link(tyl1)}), _) -> unify_sub tyl1 (rng2, tymain2)

    | (_, TypeVariable({contents= Link(tyl2)})) -> unify_sub (rng1, tymain1) tyl2

    | ( (TypeVariable({contents= Bound(_)}), _)
      | (_, TypeVariable({contents= Bound(_)})) ) ->
          failwith ("unify_sub: bound type variable in " ^ (string_of_mono_type_basic ty1) ^ " (" ^ (Range.to_string rng1) ^ ")" ^ " or " ^ (string_of_mono_type_basic ty2) ^ " (" ^ (Range.to_string rng2) ^ ")")

    | (TypeVariable({contents= Free(tvid1)} as tvref1), TypeVariable({contents= Free(tvid2)} as tvref2)) ->
        if FreeID.equal tvid1 tvid2 then
          ()
        else
          let (tvid1q, tvid2q) =
            if FreeID.is_quantifiable tvid1 && FreeID.is_quantifiable tvid2 then
              (tvid1, tvid2)
            else
              (FreeID.set_quantifiability Unquantifiable tvid1, FreeID.set_quantifiability Unquantifiable tvid2)
          in
          let (tvid1l, tvid2l) =
            let lev1 = FreeID.get_level tvid1q in
            let lev2 = FreeID.get_level tvid2q in
              if FreeID.less_than lev1 lev2 then
                (tvid1q, FreeID.set_level tvid2q lev1)
              else if FreeID.less_than lev2 lev1 then
                (FreeID.set_level tvid1q lev2, tvid2q)
              else
                (tvid1q, tvid2q)
          in
          let () =
            begin
              tvref1 := Free(tvid1l);
              tvref2 := Free(tvid2l);
            end
          in
          let (oldtvref, newtvref, newtvid, newty) =
            if Range.is_dummy rng1 then (tvref1, tvref2, tvid2l, ty2) else (tvref2, tvref1, tvid1l, ty1)
          in
                let _ = print_for_debug_typecheck                                                                 (* for debug *)
                  ("    substituteVV " ^ (string_of_mono_type_basic (Range.dummy "", TypeVariable(oldtvref)))     (* for debug *)
                   ^ " with " ^ (string_of_mono_type_basic newty)) in                                             (* for debug *)
          let () = ( oldtvref := Link(newty) ) in
          let kd1 = FreeID.get_kind tvid1l in
          let kd2 = FreeID.get_kind tvid2l in
          let (eqnlst, kdunion) =
            match (kd1, kd2) with
            | (UniversalKind, UniversalKind)       -> ([], UniversalKind)
            | (RecordKind(asc1), UniversalKind)    -> ([], RecordKind(asc1))
            | (UniversalKind, RecordKind(asc2))    -> ([], RecordKind(asc2))
            | (RecordKind(asc1), RecordKind(asc2)) ->
                let kdunion = RecordKind(Assoc.union asc1 asc2) in
                  (Assoc.intersection asc1 asc2, kdunion)
          in
          begin
            unify_list eqnlst;
            newtvref := Free(FreeID.set_kind newtvid kdunion);
          end

      | (TypeVariable({contents= Free(tvid1)} as tvref1), RecordType(tyasc2)) ->
          let kd1 = FreeID.get_kind tvid1 in
          let binc =
            match kd1 with
            | UniversalKind      -> true
            | RecordKind(tyasc1) -> Assoc.domain_included tyasc1 tyasc2
          in
          let chk = occurs tvid1 ty2 in
            if chk then
              raise InternalInclusionError
            else if not binc then
              raise InternalContradictionError
            else
              let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
                    let _ = print_for_debug_typecheck                             (* for debug *)
                      ("    substituteVR " ^ (string_of_mono_type_basic ty1)      (* for debug *)
                       ^ " with " ^ (string_of_mono_type_basic newty2)) in        (* for debug *)
              let eqnlst =
                match kd1 with
                | UniversalKind      -> []
                | RecordKind(tyasc1) -> Assoc.intersection tyasc1 tyasc2
              in
              begin
                unify_list eqnlst;
                tvref1 := Link(newty2);
              end

      | (TypeVariable({contents= Free(tvid1)} as tvref1), _) ->
          let chk = occurs tvid1 ty2 in
            if chk then
              raise InternalInclusionError
            else
              let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
                      let _ = print_for_debug_typecheck                             (* for debug *)
                        ("    substituteVX " ^ (string_of_mono_type_basic ty1)      (* for debug *)
                         ^ " with " ^ (string_of_mono_type_basic newty2)) in        (* for debug *)
                tvref1 := Link(newty2)

      | (_, TypeVariable(_)) -> unify_sub ty2 ty1

      | _ -> raise InternalContradictionError


let unify_ (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  let () = print_for_debug_typecheck ("####UNIFY " ^ (string_of_mono_type_basic ty1) ^ " = " ^ (string_of_mono_type_basic ty2)) in  (* for debug *)
  try
    unify_sub ty1 ty2
  with
  | InternalInclusionError     -> raise (InclusionError(tyenv, ty1, ty2))
  | InternalContradictionError -> raise (ContradictionError(tyenv, ty1, ty2))


let final_tyenv    : Typeenv.t ref = ref (Typeenv.empty)


let append_module_names mdlnmlst varnm =
  (List.fold_right (fun mdlnm s -> mdlnm ^ "." ^ s) mdlnmlst "") ^ varnm


let rec typecheck
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?l:(l = lev) ?q:(q = qtfbl) t u = typecheck q l t u in
  let unify = unify_ tyenv in
  match utastmain with
  | UTStringEmpty         -> (StringEmpty        , (rng, BaseType(StringType)))
(*
  | UTBreakAndIndent      -> (SoftBreakAndIndent , (rng, BaseType(StringType)))
*)
  | UTIntegerConstant(nc) -> (IntegerConstant(nc), (rng, BaseType(IntType))   )
  | UTFloatConstant(nc)   -> (FloatConstant(nc)  , (rng, BaseType(FloatType)) )
  | UTStringConstant(sc)  -> (StringConstant(sc) , (rng, BaseType(StringType)))
  | UTBooleanConstant(bc) -> (BooleanConstant(bc), (rng, BaseType(BoolType))  )
  | UTUnitConstant        -> (UnitConstant       , (rng, BaseType(UnitType))  )
  | UTHorz(hblst)         -> (Horz(hblst)        , (rng, BaseType(BoxRowType)))
  | UTVert(imvblst)       -> (Vert(imvblst)      , (rng, BaseType(BoxColType)))

  | UTInputHorz(utihlst) ->
      let ihlst = typecheck_input_horz rng qtfbl lev tyenv utihlst in
      (InputHorz(ihlst), (rng, BaseType(TextRowType)))

  | UTInputVert(utivlst) ->
      let ivlst = typecheck_input_vert rng qtfbl lev tyenv utivlst in
      (InputVert(ivlst), (rng, BaseType(TextColType)))
      
  | UTFinishStruct ->
      begin
        final_tyenv := tyenv;
        (FinishStruct, (Range.dummy "finish-struct", BaseType(UnitType)))
      end

  | UTFinishHeaderFile ->
      begin
        final_tyenv := tyenv;
        (FinishHeaderFile, (Range.dummy "finish-header-file", BaseType(UnitType)))
      end

  | UTContentOf(mdlnmlst, varnm) ->
      begin
        try
          let (pty, evid) = Typeenv.find tyenv mdlnmlst varnm in
          let tyfree = instantiate lev qtfbl pty in
          let tyres = overwrite_range_of_type tyfree rng in
          let () = print_for_debug_typecheck ("#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty) ^ " = " ^ (string_of_mono_type_basic tyres) ^ " (" ^ (Range.to_string rng) ^ ")") in (* for debug *)
              (ContentOf(evid), tyres)
        with
        | Not_found -> raise (UndefinedVariable(rng, append_module_names mdlnmlst varnm))
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        try
          let (tyarglist, tyid, tyc) = Typeenv.find_constructor qtfbl tyenv lev constrnm in
          let () = print_for_debug_typecheck ("#Constructor " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc) ^ " in ... " ^ (string_of_mono_type_basic (rng, VariantType([], tyid))) ^ "(" ^ (Typeenv.find_type_name tyenv tyid) ^ ")") in (* for debug *)
          let (e1, ty1) = typecheck_iter tyenv utast1 in
          let () = unify ty1 tyc in
          let tyres = (rng, VariantType(tyarglist, tyid)) in
            (Constructor(constrnm, e1), tyres)
        with
        | Not_found -> raise (UndefinedConstructor(rng, constrnm))
      end

  | UTHorzConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, BaseType(BoxRowType)) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let () = unify ty2 (get_range utast2, BaseType(BoxRowType)) in
        (HorzConcat(e1, e2), (rng, BaseType(BoxRowType)))

  | UTVertConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, BaseType(BoxColType)) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let () = unify ty2 (get_range utast2, BaseType(BoxColType)) in
        (VertConcat(e1, e2), (rng, BaseType(BoxColType)))

  | UTConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, BaseType(TextRowType)) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let () = unify ty2 (get_range utast2, BaseType(TextRowType)) in
        (Concat(e1, e2), (rng, BaseType(TextRowType)))

  | UTLambdaHorz(varrng, varnmctx, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnmctx in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnmctx (Poly(beta), evid)) utast1 in
      let (tyarglst, tyret) = flatten_type ty1 in
      let () = unify tyret (Range.dummy "lambda-horz-return", BaseType(BoxRowType)) in
        (LambdaHorz(evid, e1), (rng, HorzCommandType(tyarglst)))

  | UTLambdaVert(varrng, varnmctx, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnmctx in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnmctx (Poly(beta), evid)) utast1 in
      let (tyarglst, tyret) = flatten_type ty1 in
      let () = unify tyret (Range.dummy "lambda-vert-return", BaseType(BoxColType)) in
        (LambdaVert(evid, e1), (rng, VertCommandType(tyarglst)))

  | UTApply(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let _ = print_for_debug_typecheck ("#Apply " ^ (string_of_utast (rng, utastmain))) in (* for debug *)
      begin
        match ty1 with
        | (_, FuncType(tydom, tycod)) ->
            let () = unify tydom ty2 in
            let _ = print_for_debug_typecheck ("1 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " (* for debug *)
                                               ^ (string_of_mono_type_basic tycod)) in        (* for debug *)
            let tycodnew = overwrite_range_of_type tycod rng in
              (Apply(e1, e2), tycodnew)
        | _ ->
            let tvid = FreeID.fresh UniversalKind qtfbl lev () in
            let beta = (rng, TypeVariable(ref (Free(tvid)))) in
            let () = unify ty1 (get_range utast1, FuncType(ty2, beta)) in
            let _ = print_for_debug_typecheck ("2 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " ^ (string_of_mono_type_basic beta) ^ " = " ^ (string_of_mono_type_basic beta)) in (* for debug *)
                (Apply(e1, e2), beta)
      end

  | UTLambdaAbstract(varrng, varnm, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnm in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnm (Poly(beta), evid)) utast1 in
        let tydom = beta in
        let tycod = ty1 in
          (LambdaAbstract(evid, e1), (rng, FuncType(tydom, tycod)))

  | UTLetIn(utmutletcons, utast2) ->
      let (tyenvnew, _, mutletcons) = make_type_environment_by_let qtfbl lev tyenv utmutletcons in
      let (e2, ty2) = typecheck_iter tyenvnew utast2 in
        (LetIn(mutletcons, e2), ty2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      let () = unify tyB (Range.dummy "if-bool", BaseType(BoolType)) in
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let () = unify ty2 ty1 in
        (IfThenElse(eB, e1, e2), ty1)

(* ---- imperatives ---- *)

  | UTLetMutableIn(varrng, varnm, utastI, utastA) ->
      let (tyenvI, evid, eI, tyI) = make_type_environment_by_let_mutable lev tyenv varrng varnm utastI in
      let (eA, tyA) = typecheck_iter tyenvI utastA in
        (LetMutableIn(evid, eI, eA), tyA)

  | UTOverwrite(varrng, varnm, utastN) ->
      begin
        match typecheck_iter tyenv (varrng, UTContentOf([], varnm)) with
        | (ContentOf(evid), tyvar) ->
            let (eN, tyN) = typecheck_iter tyenv utastN in
            let () = unify tyvar (get_range utastN, RefType(tyN)) in
              (* --
                 actually 'get_range utastnew' is not good
                 since the right side expression has type 't, not 't ref 
              -- *)
              (Overwrite(evid, eN), (rng, BaseType(UnitType)))

        | _ -> assert false
      end

  | UTSequential(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (get_range utast1, BaseType(UnitType)) in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
        (Sequential(e1, e2), ty2)

  | UTWhileDo(utastB, utastC) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      let () = unify tyB (get_range utastB, BaseType(BoolType)) in
      let (eC, tyC) = typecheck_iter tyenv utastC in
      let () = unify tyC (get_range utastC, BaseType(UnitType)) in
        (WhileDo(eB, eC), (rng, BaseType(UnitType)))

(*
(* ---- final reference ---- *)

  | UTDeclareGlobalHash(utastK, utastI) ->
      let (eK, tyK) = typecheck_iter tyenv utastK in
      let () = (unify tyK (get_range utastK, BaseType(StringType))) in
      let (eI, tyI) = typecheck_iter tyenv utastI in
      let () = unify tyI (get_range utastI, BaseType(StringType)) in
        (DeclareGlobalHash(eK, eI), (rng, BaseType(UnitType)))

  | UTOverwriteGlobalHash(utastK, utastN) ->
      let (eK, tyK) = typecheck_iter tyenv utastK in
      let () = unify tyK (get_range utastK, BaseType(StringType)) in
      let (eN, tyN) = typecheck_iter tyenv utastN in
      let () = unify tyN (get_range utastN, BaseType(StringType)) in
        (OverwriteGlobalHash(eK, eN), (rng, BaseType(UnitType)))

  | UTReferenceFinal(utast1) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let () = unify ty1 (rng, BaseType(StringType)) in
        (ReferenceFinal(e1), (rng, BaseType(StringType)))

(* ---- class/id option ---- *)

  | UTApplyClassAndID(utastcls, utastid, utast1) ->
      let dr = Range.dummy "ut-apply-class-and-id" in
      let evidcls = EvalVarID.for_class_name in
      let tyenvmid = Typeenv.add tyenv "class-name" (Poly((dr, VariantType([(dr, BaseType(StringType))], Typeenv.find_type_id tyenv "maybe"))), evidcls) in (* temporary; `find_type_id` is vulnerable to the re-definition of a type named 'maybe' *)
      let evidid = EvalVarID.for_id_name in
      let tyenvnew = Typeenv.add tyenvmid "id-name" (Poly((dr, VariantType([(dr, BaseType(StringType))], Typeenv.find_type_id tyenv "maybe"))), evidid) in (* temporary; `find_type_id` is vulnerable to the re-definition of a type named 'maybe' *)
      let (ecls, _) = typecheck_iter tyenv utastcls in
      let (eid, _)  = typecheck_iter tyenv utastid in
      let (e1, ty1) = typecheck_iter tyenvnew utast1 in
        (ApplyClassAndID(evidcls, evidid, ecls, eid, e1), ty1)

  | UTClassAndIDRegion(utast1) ->
      let dr = Range.dummy "ut-class-and-id-region" in
      let evidcls = EvalVarID.for_class_name in
      let tyenvmid = Typeenv.add tyenv "class-name" (Poly((dr, VariantType([(dr, BaseType(StringType))], Typeenv.find_type_id tyenv "maybe"))), evidcls) in (* temporary; `find_type_id` is vulnerable to the re-definition of a type named 'maybe' *)
      let evidid = EvalVarID.for_id_name in
      let tyenvnew = Typeenv.add tyenvmid "id-name" (Poly((dr, VariantType([(dr, BaseType(StringType))], Typeenv.find_type_id tyenv "maybe"))), evidid) in (* temporary; `find_type_id` is vulnerable to the re-definition of a type named 'maybe' *)
      let (e1, ty1) = typecheck_iter tyenvnew utast1 in
        (e1, ty1)
*)

(* ---- lightweight itemize ---- *)

  | UTItemize(utitmz) ->
      let eitmz = typecheck_itemize qtfbl lev tyenv utitmz in
        (eitmz, (rng, VariantType([], Typeenv.find_type_id tyenv "itemize"))) (* temporary *)

(* ---- list ---- *)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      let () = unify tyT (Range.dummy "list-cons", ListType(tyH)) in
      let tyres = (rng, ListType(tyH)) in
        (ListCons(eH, eT), tyres)

  | UTEndOfList ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
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

  | UTRecord(flutlst) -> typecheck_record qtfbl lev tyenv flutlst rng

  | UTAccessField(utast1, fldnm) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let tvidF = FreeID.fresh UniversalKind qtfbl lev () in
      let betaF = (rng, TypeVariable(ref (Free(tvidF)))) in
      let tvid1 = FreeID.fresh (RecordKind(Assoc.of_list [(fldnm, betaF)])) qtfbl lev () in
      let beta1 = (get_range utast1, TypeVariable(ref (Free(tvid1)))) in
      let () = unify beta1 ty1 in
        (AccessField(e1, fldnm), betaF)

(* ---- other fundamentals ---- *)

  | UTPatternMatch(utastO, utpmcons) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(ref (Free(tvid)))) in
      let (pmcons, tyP) =
            typecheck_pattern_match_cons qtfbl lev tyenv utpmcons tyO beta in
        (PatternMatch(eO, pmcons), tyP)

  | UTDeclareVariantIn(mutvarntcons, utastA) ->
      let tyenvnew = Typeenv.add_mutual_cons tyenv lev mutvarntcons in
        typecheck_iter tyenvnew utastA

  | UTModule(mdlrng, mdlnm, sigopt, utastM, utastA) ->
      let tyenvinner = Typeenv.enter_new_module tyenv mdlnm in
      let (eM, _) = typecheck_iter tyenvinner utastM in
      let tyenvmid = Typeenv.sigcheck mdlrng qtfbl lev (!final_tyenv) tyenv sigopt in
      let tyenvouter = Typeenv.leave_module tyenvmid in
      let (eA, tyA) = typecheck_iter tyenvouter utastA in
        (Module(eM, eA), tyA)


and typecheck_input_vert (rng : Range.t) (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t) (utivlst : untyped_input_vert_element list) =
  let rec aux (acc : input_vert_element list) (lst : untyped_input_vert_element list) =
    match lst with
    | [] -> List.rev acc
    | (_, UTInputVertEmbedded(utastcmd, utastarglst)) :: tail ->
        let (ecmd, (_, tycmdmain)) = typecheck qtfbl lev tyenv utastcmd in
        begin
          match tycmdmain with

          | VertCommandType(tylstreq) ->
              let etylst = List.map (typecheck qtfbl lev tyenv) utastarglst in
              let tyarglst = etylst |> List.map (fun (e, ty) -> ty) in
              let earglst = etylst |> List.map (fun (e, ty) -> e) in
              let () =
                try List.iter2 (unify_ tyenv) tyarglst tylstreq with
                | Invalid_argument(_) ->
                    let lenreq  = List.length tylstreq in
                    let lenreal = List.length tyarglst in
                    raise (InvalidArityOfCommand(rng, lenreq, lenreal))
              in
                aux (InputVertEmbedded(ecmd, earglst) :: acc) tail

          | _ -> failwith "vertical command of type other than VertCommandType(_)"
        end
  in
    aux [] utivlst
        


and typecheck_input_horz (rng : Range.t) (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t) (utihlst : untyped_input_horz_element list) =
  let rec aux (acc : input_horz_element list) (lst : untyped_input_horz_element list) =
    match lst with
    | [] -> List.rev acc

    | (_, UTInputHorzEmbedded(utastcmd, utastarglst)) :: tail ->
        let (ecmd, (_, tycmdmain)) = typecheck qtfbl lev tyenv utastcmd in
        begin
          match tycmdmain with

          | HorzCommandType(tylstreq) ->
              let etylst = List.map (typecheck qtfbl lev tyenv) utastarglst in
              let tyarglst = etylst |> List.map (fun (e, ty) -> ty) in
              let earglst = etylst |> List.map (fun (e, ty) -> e) in
              let () =
                try List.iter2 (unify_ tyenv) tyarglst tylstreq with
                | Invalid_argument(_) ->
                    let lenreq  = List.length tylstreq in
                    let lenreal = List.length tyarglst in
                    raise (InvalidArityOfCommand(rng, lenreq, lenreal))
              in
                aux (InputHorzEmbedded(ecmd, earglst) :: acc) tail

          | _ -> failwith "horizontal command of type other than HorzCommandType(_)"
        end

    | (_, UTInputHorzText(s)) :: tail ->
        aux (InputHorzText(s) :: acc) tail
  in
    aux [] utihlst


and typecheck_record
    (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t)
    (flutlst : (field_name * untyped_abstract_tree) list) (rng : Range.t)
=
  let rec aux
      (tyenv : Typeenv.t) (lst : (field_name * untyped_abstract_tree) list)
      (accelst : (field_name * abstract_tree) list) (acctylst : (field_name * mono_type) list)
  =
    match lst with
    | []                       -> (List.rev accelst, List.rev acctylst)
    | (fldnmX, utastX) :: tail ->
        let (eX, tyX) = typecheck qtfbl lev tyenv utastX in
          aux tyenv tail ((fldnmX, eX) :: accelst) ((fldnmX, tyX) :: acctylst)
  in
  let (elst, tylst) = aux tyenv flutlst [] [] in
  let tylstfinal = List.map (fun (fldnm, ty) -> (fldnm, ty)) tylst in
    (Record(Assoc.of_list elst), (rng, RecordType(Assoc.of_list tylstfinal)))


and typecheck_itemize (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) =
  let (e1, ty1) = typecheck qtfbl lev tyenv utast1 in
  let () = unify_ tyenv ty1 (Range.dummy "typecheck_itemize_string", BaseType(StringType)) in
  let elst = typecheck_itemize_list qtfbl lev tyenv utitmzlst in
    (Constructor("Item", TupleCons(e1, TupleCons(elst, EndOfTuple))))


and typecheck_itemize_list
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utitmzlst : untyped_itemize list) =
  match utitmzlst with
  | []                  -> EndOfList
  | hditmz :: tlitmzlst ->
      let ehd = typecheck_itemize qtfbl lev tyenv hditmz in
      let etl = typecheck_itemize_list qtfbl lev tyenv tlitmzlst in
        ListCons(ehd, etl)


and typecheck_pattern_match_cons
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utpmcons : untyped_pattern_match_cons) (tyobj : mono_type) (tyres : mono_type) =
  let iter = typecheck_pattern_match_cons qtfbl lev in
  let unify = unify_ tyenv in
    match utpmcons with
    | UTEndOfPatternMatch -> (EndOfPatternMatch, tyres)

    | UTPatternMatchCons(utpat, utast1, tailcons) ->
        let (epat, typat, tyenvpat) = typecheck_pattern qtfbl lev tyenv utpat in
        let () = unify typat tyobj in
        let (e1, ty1) = typecheck qtfbl lev tyenvpat utast1 in
        let () = unify ty1 tyres in
        let (pmctl, tytl) =
              iter tyenv tailcons tyobj tyres in
          (PatternMatchCons(epat, e1, pmctl), tytl)

    | UTPatternMatchConsWhen(utpat, utastb, utast1, tailcons) ->
        let (epat, typat, tyenvpat) = typecheck_pattern qtfbl lev tyenv utpat in
        let () = unify typat tyobj in
        let (eB, tyB) = typecheck qtfbl lev tyenvpat utastb in
        let () = unify tyB (Range.dummy "pattern-match-cons-when", BaseType(BoolType)) in
        let (e1, ty1) = typecheck qtfbl lev tyenvpat utast1 in
        let () = unify ty1 tyres in
        let (pmctl, tytl) =
              iter tyenv tailcons tyobj tyres in
          (PatternMatchConsWhen(epat, eB, e1, pmctl), tytl)


and typecheck_pattern
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (rng, utpatmain) =
  let iter = typecheck_pattern qtfbl lev in
  let unify = unify_ tyenv in
    match utpatmain with
    | UTPIntegerConstant(nc) -> (PIntegerConstant(nc), (rng, BaseType(IntType)), tyenv)
    | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BaseType(BoolType)), tyenv)
    | UTPStringConstant(ut1) ->
        let (e1, ty1) = typecheck qtfbl lev tyenv ut1 in
        let () = unify (Range.dummy "pattern-string-constant", BaseType(StringType)) ty1 in
          (PStringConstant(e1), (rng, BaseType(StringType)), tyenv)

    | UTPUnitConstant        -> (PUnitConstant, (rng, BaseType(UnitType)), tyenv)

    | UTPListCons(utpat1, utpat2) ->
        let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
        let (epat2, typat2, tyenv2) = iter tyenv1 utpat2 in
        let () = unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
          (PListCons(epat1, epat2), typat2, tyenv2)

    | UTPEndOfList ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
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
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
          (PWildCard, beta, tyenv)

    | UTPVariable(varnm) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        let evid = EvalVarID.fresh varnm in
          (PVariable(evid), beta, Typeenv.add tyenv varnm (Poly(beta), evid))

    | UTPAsVariable(varnm, utpat1) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
        let evid = EvalVarID.fresh varnm in
          (PAsVariable(evid, epat1), typat1, Typeenv.add tyenv varnm (Poly(beta), evid))

    | UTPConstructor(constrnm, utpat1) ->
        begin
          try
            let (tyarglist, tyid, tyc) = Typeenv.find_constructor qtfbl tyenv lev constrnm in
            let () = print_for_debug_typecheck ("P-find " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc)) in (* for debug *)
            let (epat1, typat1, tyenv1) = iter tyenv utpat1 in
            let () = unify tyc typat1 in
              (PConstructor(constrnm, epat1), (rng, VariantType(tyarglist, tyid)), tyenv1)
          with
          | Not_found -> raise (UndefinedConstructor(rng, constrnm))
        end


and make_type_environment_by_let
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) =

  let rec add_mutual_variables (acctyenv : Typeenv.t) (mutletcons : untyped_mutual_let_cons) : (Typeenv.t * (var_name * mono_type * EvalVarID.t) list) =
    let iter = add_mutual_variables in
      match mutletcons with
      | []                             -> (acctyenv, [])
      | (_, varnm, astdef) :: tailcons ->
          let tvid = FreeID.fresh UniversalKind qtfbl (FreeID.succ_level lev) () in
          let beta = (get_range astdef, TypeVariable(ref (Free(tvid)))) in
          let _ = print_for_debug_typecheck ("#AddMutualVar " ^ varnm ^ " : '" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ " :: U") in (* for debug *)
          let evid = EvalVarID.fresh varnm in
          let (tyenvfinal, tvtylst) = iter (Typeenv.add acctyenv varnm (Poly(beta), evid)) tailcons in
            (tyenvfinal, ((varnm, beta, evid) :: tvtylst))
  in

  let rec typecheck_mutual_contents
      (lev : FreeID.level)
      (tyenvforrec : Typeenv.t) (utmutletcons : untyped_mutual_let_cons) (tvtylst : (var_name * mono_type * EvalVarID.t) list)
      (acctvtylstout : (var_name * mono_type * EvalVarID.t) list)
  =
    let iter = typecheck_mutual_contents lev in
    let unify = unify_ tyenv in
    match (utmutletcons, tvtylst) with
    | ([], []) -> (tyenvforrec, EndOfMutualLet, List.rev acctvtylstout)

    | ((mntyopt, varnm, utast1) :: tailcons, (_, beta, evid) :: tvtytail) ->
        let (e1, ty1) = typecheck qtfbl (FreeID.succ_level lev) tyenvforrec utast1 in
        begin
          match mntyopt with
          | None ->
              let () = unify ty1 beta in
                let (tyenvfinal, mutletcons_tail, tvtylstoutfinal) = iter tyenvforrec tailcons tvtytail ((varnm, beta, evid) :: acctvtylstout) in
                  (tyenvfinal, MutualLetCons(evid, e1, mutletcons_tail), tvtylstoutfinal)

          | Some(mnty) ->
              let tyin = Typeenv.fix_manual_type_free qtfbl tyenv lev mnty [] in
              let () = unify ty1 beta in
              let () = unify tyin beta in
                let (tyenvfinal, mutletconstail, tvtylstoutfinal) =
                      iter tyenvforrec tailcons tvtytail ((varnm, beta, evid) :: acctvtylstout) in
                    (tyenvfinal, MutualLetCons(evid, e1, mutletconstail), tvtylstoutfinal)

        end

    | _ -> assert false
  in

  let rec make_forall_type_mutual (tyenv : Typeenv.t) (tyenv_before_let : Typeenv.t) tvtylst tvtylst_forall =
    match tvtylst with
    | []                              -> (tyenv, tvtylst_forall)
    | (varnm, tvty, evid) :: tvtytail ->
        let prety = tvty in
          let () = print_for_debug_typecheck ("#Generalize1 " ^ varnm ^ " : " ^ (string_of_mono_type_basic prety)) in  (* for debug *)
          let pty = poly_extend erase_range_of_type (generalize lev prety) in
          let () = print_for_debug_typecheck ("#Generalize2 " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty)) in (* for debug *)
          let tvtylst_forall_new = (varnm, pty, evid) :: tvtylst_forall in
            make_forall_type_mutual (Typeenv.add tyenv varnm (pty, evid)) tyenv_before_let tvtytail tvtylst_forall_new
  in

  let (tyenvforrec, tvtylstforrec) = add_mutual_variables tyenv utmutletcons in
  let (tyenv_new, mutletcons, tvtylstout) =
        typecheck_mutual_contents lev tyenvforrec utmutletcons tvtylstforrec [] in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual tyenv_new tyenv tvtylstout [] in
    (tyenv_forall, tvtylst_forall, mutletcons)


and make_type_environment_by_let_mutable (lev : FreeID.level) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI) = typecheck Unquantifiable lev tyenv utastI in
  let () = print_for_debug_typecheck ("#AddMutable " ^ varnm ^ " : " ^ (string_of_mono_type_basic (varrng, RefType(tyI)))) in (* for debug *)
  let evid = EvalVarID.fresh varnm in
  let tyenvI = Typeenv.add tyenv varnm (Poly((varrng, RefType(tyI))), evid) in
    (tyenvI, evid, eI, tyI)


let main (tyenv : Typeenv.t) (utast : untyped_abstract_tree) =
  begin
    final_tyenv := tyenv;
    let (e, ty) = typecheck Quantifiable FreeID.bottom_level tyenv utast in
      (ty, !final_tyenv, e)
  end
