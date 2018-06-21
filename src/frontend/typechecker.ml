
module Types = Types_
module Primitives = Primitives_
open MyUtil
open Types
open Display

exception UndefinedVariable     of Range.t * module_name list * var_name
exception UndefinedConstructor  of Range.t * var_name
exception InclusionError        of Typeenv.t * mono_type * mono_type
exception ContradictionError    of Typeenv.t * mono_type * mono_type
exception UnknownUnitOfLength   of Range.t * length_unit_name
exception HorzCommandInMath     of Range.t
exception MathCommandInHorz     of Range.t
exception BreaksValueRestriction of Range.t
exception MultiplePatternVariable of Range.t * Range.t * var_name
exception InvalidOptionalCommandArgument of Typeenv.t * mono_type * Range.t
exception NeedsMoreArgument              of Range.t * Typeenv.t * mono_type * mono_type
exception TooManyArgument                of Range.t * Typeenv.t * mono_type

exception InternalInclusionError
exception InternalContradictionError


let print_for_debug_typecheck msg =
(*
  print_endline msg;
*)
  ()


let add_optionals_to_type_environment (tyenv : Typeenv.t) qtfbl lev (optargs : (Range.t * var_name) list) =
  let (tyenvnew, tyacc, evidacc) =
    optargs |> List.fold_left (fun (tyenv, tyacc, evidacc) (rng, varnm) ->
      let evid = EvalVarID.fresh varnm in
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
      let tyenvnew = Typeenv.add tyenv varnm (Poly(Primitives.option_type beta), evid) in
        (tyenvnew, Alist.extend tyacc beta, Alist.extend evidacc evid)
    ) (tyenv, Alist.empty, Alist.empty)
  in
    (Alist.to_list tyacc, Alist.to_list evidacc, tyenvnew)


let append_optional_ids (evidlst : EvalVarID.t list) (ast : abstract_tree) =
  List.fold_right (fun evid ast -> Function([PatternBranch(PVariable(evid), ast)])) evidlst ast


let rec is_nonexpansive_expression e =
  let iter = is_nonexpansive_expression in
    match e with
    | Value(_)
    | Function(_)
    | ContentOf(_)
        -> true

    | NonValueConstructor(constrnm, e1) -> iter e1
    | PrimitiveListCons(e1, e2)         -> iter e1 && iter e2
    | PrimitiveTupleCons(e1, e2)        -> iter e1 && iter e2
    | Record(asc)                       -> Assoc.fold_value (fun b e -> b && iter e) true asc
    | LetRecIn(_, e2)                   -> iter e2
    | LetNonRecIn(_, e1, e2)            -> iter e1 && iter e2
    | _                                 -> false


module PatternVarMap = Map.Make
  (struct
    type t = var_name
    let compare = Pervasives.compare
  end)


type pattern_var_map = (Range.t * EvalVarID.t * mono_type) PatternVarMap.t


let unite_pattern_var_map (patvarmap1 : pattern_var_map) (patvarmap2 : pattern_var_map) : pattern_var_map =
  PatternVarMap.union (fun varnm (rng1, _, _) (rng2, _, _) ->
    raise (MultiplePatternVariable(rng1, rng2, varnm))
  ) patvarmap1 patvarmap2


let add_pattern_var_mono (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = poly_extend erase_range_of_type (Poly(ty)) in
      Typeenv.add tyenvacc varnm (pty, evid)
  ) patvarmap tyenv


let add_pattern_var_poly lev (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = poly_extend erase_range_of_type (generalize lev ty) in
      Typeenv.add tyenvacc varnm (pty, evid)
  ) patvarmap tyenv


let point_type_main =
  (ProductType([
    (Range.dummy "point-type-1", BaseType(LengthType));
    (Range.dummy "point-type-2", BaseType(LengthType));
  ]))


(* -- 'apply_tree_of_list': converts e0 and [e1; ...; eN] into (e0 e1 ... eN) -- *)
let apply_tree_of_list astfunc astlst =
  let rec aux astacc astlst =
    match astlst with
    | []                 -> astacc
    | asthead :: asttail -> aux (Apply(astacc, asthead)) asttail
  in
    aux astfunc astlst


(* -- 'flatten_type': converts type (t1 -> ... -> tN -> t) into ([t1; ...; tN], t) -- *)
let flatten_type (ty : mono_type) : command_argument_type list * mono_type =
  let rec aux acc ty =
    let (rng, tymain) = normalize_mono_type ty in
      match tymain with
      | FuncType(tyoptsr, tydom, tycod) ->
          let accnew =
            Alist.append acc (List.append (List.map (fun ty -> OptionalArgumentType(ty)) (!tyoptsr)) [MandatoryArgumentType(tydom)])
          in
            aux accnew tycod

      | _ -> (Alist.to_list acc, ty)
  in
    aux Alist.empty ty


let eliminate_optionals (ty : mono_type) (e : abstract_tree) : mono_type * abstract_tree =
  let rec aux ty e =
    match ty with
    | (rng, FuncType({contents = _ :: tyopttail}, tydom, tycod)) ->
        aux (rng, FuncType({contents = tyopttail}, tydom, tycod)) (Apply(e, Value(Constructor("None", UnitConstant))))

    | _ ->
        (ty, e)
  in
  aux ty e


let rec occurs (tvid : FreeID.t) ((_, tymain) : mono_type) =
  let iter = occurs tvid in
  let iter_list = List.fold_left (fun b ty -> b || iter ty) false in
  let iter_cmd_list =
    List.fold_left (fun b caty ->
      match caty with
      | MandatoryArgumentType(ty) -> b || iter ty
      | OptionalArgumentType(ty)  -> b || iter ty
    ) false
  in
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
  | FuncType(tyoptsr, tydom, tycod) -> iter_list (!tyoptsr) || iter tydom || iter tycod
  | ProductType(tylist)            -> iter_list tylist
  | ListType(tysub)                -> iter tysub
  | RefType(tysub)                 -> iter tysub
  | VariantType(tylist, _)         -> iter_list tylist
  | SynonymType(tylist, _, tyreal) -> iter_list tylist || iter tyreal
  | RecordType(tyasc)              -> iter_list (Assoc.to_value_list tyasc)
  | BaseType(_)                    -> false
  | HorzCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
  | VertCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
  | MathCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist


let rec unify_sub ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) =
  let unify_list = List.iter (fun (t1, t2) -> unify_sub t1 t2) in
  let () = print_for_debug_typecheck ("    | unify " ^ (string_of_mono_type_basic ty1) ^ " == " ^ (string_of_mono_type_basic ty2)) in (* for debug *)
    match (tymain1, tymain2) with

    | (SynonymType(_, _, tyreal1), _) -> unify_sub tyreal1 ty2
    | (_, SynonymType(_, _, tyreal2)) -> unify_sub ty1 tyreal2

    | (BaseType(bsty1), BaseType(bsty2))  when bsty1 = bsty2 -> ()

    | (FuncType(tyopts1r, tydom1, tycod1), FuncType(tyopts2r, tydom2, tycod2))
      ->
        begin
          unify_options tyopts1r tyopts2r;
          unify_sub tydom1 tydom2;
          unify_sub tycod1 tycod2;
        end

    | (HorzCommandType(cmdargtylist1), HorzCommandType(cmdargtylist2))
    | (VertCommandType(cmdargtylist1), VertCommandType(cmdargtylist2))
    | (MathCommandType(cmdargtylist1), MathCommandType(cmdargtylist2))
      ->
        begin
          try
            List.iter2 (fun cmdargty1 cmdargty2 ->
              match (cmdargty1, cmdargty2) with
              | (MandatoryArgumentType(ty1), MandatoryArgumentType(ty2)) -> unify_sub ty1 ty2
              | (OptionalArgumentType(ty1) , OptionalArgumentType(ty2) ) -> unify_sub ty1 ty2
              | _ -> raise InternalContradictionError
            ) cmdargtylist1 cmdargtylist2
          with
          | Invalid_argument(_) ->
              raise InternalContradictionError
        end

    | (ProductType(tylist1), ProductType(tylist2))
      ->
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
        begin
          try
            unify_list (List.combine tyarglist1 tyarglist2)
          with
          | Invalid_argument(_) ->
              raise InternalContradictionError
        end

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
                  ("        substituteVV " ^ (string_of_mono_type_basic (Range.dummy "", TypeVariable(oldtvref)))     (* for debug *)
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
                      ("        substituteVR " ^ (string_of_mono_type_basic ty1)      (* for debug *)
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
                        ("        substituteVX " ^ (string_of_mono_type_basic ty1)      (* for debug *)
                         ^ " with " ^ (string_of_mono_type_basic newty2)) in        (* for debug *)
                tvref1 := Link(newty2)

      | (_, TypeVariable(_)) -> unify_sub ty2 ty1

      | _ -> raise InternalContradictionError


and unify_options tyopts1r tyopts2r =
  let rec aux tyopts1 tyopts2 =
    match (tyopts1, tyopts2) with
    | (_, []) ->
        tyopts2r := tyopts1

    | ([], _) ->
        tyopts1r := tyopts2

    | (ty1 :: tytail1, ty2 :: tytail2) ->
        unify_sub ty1 ty2;
        aux tytail1 tytail2
  in
    aux (!tyopts1r) (!tyopts2r)


let unify_ (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  let () = print_for_debug_typecheck ("    ####UNIFY " ^ (string_of_mono_type_basic ty1) ^ " = " ^ (string_of_mono_type_basic ty2)) in  (* for debug *)
  try
    unify_sub ty1 ty2
  with
  | InternalInclusionError     -> raise (InclusionError(tyenv, ty1, ty2))
  | InternalContradictionError -> raise (ContradictionError(tyenv, ty1, ty2))


let final_tyenv    : Typeenv.t ref = ref (Typeenv.empty)


let rec typecheck
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?l:(l = lev) ?q:(q = qtfbl) t u = typecheck q l t u in
  let unify = unify_ tyenv in
  match utastmain with
  | UTStringEmpty         -> (Value(StringEmpty)        , (rng, BaseType(StringType)))
  | UTIntegerConstant(nc) -> (Value(IntegerConstant(nc)), (rng, BaseType(IntType))   )
  | UTFloatConstant(nc)   -> (Value(FloatConstant(nc))  , (rng, BaseType(FloatType)) )
  | UTStringConstant(sc)  -> (Value(StringConstant(sc)) , (rng, BaseType(StringType)))
  | UTBooleanConstant(bc) -> (Value(BooleanConstant(bc)), (rng, BaseType(BoolType))  )
  | UTUnitConstant        -> (Value(UnitConstant)       , (rng, BaseType(UnitType))  )
  | UTHorz(hblst)         -> (Value(Horz(hblst))        , (rng, BaseType(BoxRowType)))
  | UTVert(imvblst)       -> (Value(Vert(imvblst))      , (rng, BaseType(BoxColType)))

  | UTLengthDescription(flt, unitnm) ->
      begin
        match unitnm with  (* temporary; ad-hoc validation *)
        | ( "pt" | "cm" | "mm" | "inch" ) ->
            (LengthDescription(flt, unitnm), (rng, BaseType(LengthType)))

        | _ -> raise (UnknownUnitOfLength(rng, unitnm))
      end

  | UTInputHorz(utihlst) ->
      let ihlst = typecheck_input_horz rng qtfbl lev tyenv utihlst in
      (InputHorz(ihlst), (rng, BaseType(TextRowType)))

  | UTInputVert(utivlst) ->
      let ivlst = typecheck_input_vert rng qtfbl lev tyenv utivlst in
      (InputVert(ivlst), (rng, BaseType(TextColType)))

  | UTPath(utastpt0, utpathcomplst, utcycleopt) ->
      let (ept0, typt0) = typecheck_iter tyenv utastpt0 in
      let () = unify typt0 (Range.dummy "ut-path", point_type_main) in
      let (pathcomplst, cycleopt) = typecheck_path qtfbl lev tyenv utpathcomplst utcycleopt in
      (Path(ept0, pathcomplst, cycleopt), (rng, BaseType(PathType)))

  | UTFinishStruct ->
      begin
        final_tyenv := tyenv;
        (FinishStruct, (Range.dummy "finish-struct", BaseType(EnvType)))
      end

  | UTFinishHeaderFile ->
      begin
        final_tyenv := tyenv;
        (FinishHeaderFile, (Range.dummy "finish-header-file", BaseType(EnvType)))
      end

  | UTOpenIn(rngtok, mdlnm, utast1) ->
      let tyenvnew = Typeenv.open_module tyenv rngtok mdlnm in
        typecheck_iter tyenvnew utast1

  | UTContentOf(mdlnmlst, varnm) ->
      begin
        match Typeenv.find tyenv mdlnmlst varnm rng with
        | None ->
            begin match Typeenv.find_candidate tyenv mdlnmlst varnm rng with
            | None ->
              raise (UndefinedVariable(rng, mdlnmlst, varnm))
            | Some(candidates) ->
              Format.printf "Did you mean %s?\n" (List.hd candidates);
              raise (UndefinedVariable(rng, mdlnmlst, varnm))
            end

        | Some((pty, evid)) ->
            let tyfree = instantiate lev qtfbl pty in
            let tyres = overwrite_range_of_type tyfree rng in
            let () = print_for_debug_typecheck ("\n#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty) ^ " = " ^ (string_of_mono_type_basic tyres) ^ "\n  (" ^ (Range.to_string rng) ^ ")") in (* for debug *)
                (ContentOf(rng, evid), tyres)
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        match Typeenv.find_constructor qtfbl tyenv lev constrnm with
        | None -> raise (UndefinedConstructor(rng, constrnm))
        | Some((tyarglist, tyid, tyc)) ->
            let () = print_for_debug_typecheck ("\n#Constructor " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc) ^ " in ... " ^ (string_of_mono_type_basic (rng, VariantType([], tyid))) ^ "(" ^ (Typeenv.find_type_name tyenv tyid) ^ ")") in (* for debug *)
            let (e1, ty1) = typecheck_iter tyenv utast1 in
            let () = unify ty1 tyc in
            let tyres = (rng, VariantType(tyarglist, tyid)) in
              (NonValueConstructor(constrnm, e1), tyres)
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
      let (cmdargtylist, tyret) = flatten_type ty1 in
      let () = unify tyret (Range.dummy "lambda-horz-return", BaseType(BoxRowType)) in
        (LambdaHorz(evid, e1), (rng, HorzCommandType(cmdargtylist)))

  | UTLambdaVert(varrng, varnmctx, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnmctx in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnmctx (Poly(beta), evid)) utast1 in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      let () = unify tyret (Range.dummy "lambda-vert-return", BaseType(BoxColType)) in
        (LambdaVert(evid, e1), (rng, VertCommandType(cmdargtylist)))

  | UTLambdaMath(utastF) ->
      let (eF, tyF) = typecheck_iter tyenv utastF in
      let (cmdargtylist, tyret) = flatten_type tyF in
      let () = unify tyret (Range.dummy "lambda-math-return", BaseType(MathType)) in
        (eF, (rng, MathCommandType(cmdargtylist)))
(*
  | UTLambdaOptional(varrng, varnmctx, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnmctx in
      let ty = overwrite_range_of_type (Primitives.option_type beta) rng in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnmctx (Poly(ty), evid)) utast1 in
        (Function([PatternBranch(PVariable(evid), e1)]), (rng, OptFuncType(beta, ty1)))
*)
  | UTApply(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
(*
      let _ = print_for_debug_typecheck ("#Apply " ^ (string_of_utast (rng, utastmain))) in (* for debug *)
*)
      let (ty1sub, e1sub) = eliminate_optionals ty1 e1 in
      let eret = Apply(e1sub, e2) in
      begin
        match ty1sub with
        | (_, FuncType(_, tydom, tycod)) ->
            let () = unify tydom ty2 in
(*
            let _ = print_for_debug_typecheck ("1 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " (* for debug *)
                                               ^ (string_of_mono_type_basic tycod)) in        (* for debug *)
*)
            let tycodnew = overwrite_range_of_type tycod rng in
              (eret, tycodnew)

        | _ ->
            let tvid = FreeID.fresh UniversalKind qtfbl lev () in
            let beta = (rng, TypeVariable(ref (Free(tvid)))) in
            let () = unify ty1sub (get_range utast1, FuncType(ref [], ty2, beta)) in
(*
            let _ = print_for_debug_typecheck ("2 " ^ (string_of_ast (Apply(e1, e2))) ^ " : " ^ (string_of_mono_type_basic beta) ^ " = " ^ (string_of_mono_type_basic beta)) in (* for debug *)
*)
                (eret, beta)
      end

  | UTApplyOptional(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      begin
        match ty1 with
        | (_, FuncType({contents = tyopt :: tyopttail}, tydom, tycod)) ->
            let () = unify tyopt ty2 in
            let tynew = (rng, FuncType(ref tyopttail, tydom, tycod)) in
              (Apply(e1, NonValueConstructor("Some", e2)), tynew)

        | _ ->
            let tvid1 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta1 = (rng, TypeVariable(ref (Free(tvid1)))) in
            let tvid2 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta2 = (rng, TypeVariable(ref (Free(tvid2)))) in
            let () = unify ty1 (get_range utast1, FuncType(ref [ty2], beta1, beta2)) in
              (Apply(e1, NonValueConstructor("Some", e2)), (rng, FuncType(ref [], beta1, beta2)))
                (* doubtful *)
      end

  | UTApplyOmission(utast1) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let eret = Apply(e1, Value(Constructor("None", UnitConstant))) in
      begin
        match ty1 with
        | (_, FuncType({contents = _ :: tyopttail}, _, tycod)) ->
            (eret, tycod)

        | _ ->
            let tvid0 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta0 = (rng, TypeVariable(ref (Free(tvid0)))) in
            let tvid1 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta1 = (rng, TypeVariable(ref (Free(tvid1)))) in
            let tvid2 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta2 = (rng, TypeVariable(ref (Free(tvid2)))) in
            let () = unify ty1 (get_range utast1, FuncType(ref [beta0], beta1, beta2)) in
              (eret, (rng, FuncType(ref [], beta1, beta2)))
      end

  | UTFunction(optargs, utpatbrs) ->
      let (tyopts, evids, tyenvnew) = add_optionals_to_type_environment tyenv qtfbl lev optargs in
      let tvidO = FreeID.fresh UniversalKind qtfbl lev () in
      let betaO = (Range.dummy "UTFunction:dom", TypeVariable(ref (Free(tvidO)))) in
      let tvidR = FreeID.fresh UniversalKind qtfbl lev () in
      let betaR = (Range.dummy "UTFunction:cod", TypeVariable(ref (Free(tvidR)))) in
      let (patbrs, _) = typecheck_pattern_branch_list qtfbl lev tyenvnew utpatbrs betaO betaR in
      let e = append_optional_ids evids (Function(patbrs)) in
        (e, (rng, FuncType(ref tyopts, betaO, betaR)))
(*
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(ref (Free(tvid)))) in
      let evid = EvalVarID.fresh varnm in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnm (Poly(beta), evid)) utast1 in
        let tydom = beta in
        let tycod = ty1 in
          (LambdaAbstract(evid, e1), (rng, FuncType(tydom, tycod)))
*)

  | UTPatternMatch(utastO, utpatbrs) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(ref (Free(tvid)))) in
      let (patbrs, tyP) = typecheck_pattern_branch_list qtfbl lev tyenv utpatbrs tyO beta in
        (PatternMatch(rng, eO, patbrs), tyP)

  | UTLetNonRecIn(mntyopt, utpat, utast1, utast2) ->
      let (pat, tyP, patvarmap) = typecheck_pattern qtfbl (FreeID.succ_level lev) tyenv utpat in
      let (e1, ty1) = typecheck qtfbl (FreeID.succ_level lev) tyenv utast1 in
      let () = unify ty1 tyP in
      let tyenvnew =
        if is_nonexpansive_expression e1 then
        (* -- if 'e1' is polymorphically typeable -- *)
          add_pattern_var_poly lev tyenv patvarmap
        else
        (* -- 'e1' should be typed monomorphically -- *)
          add_pattern_var_mono tyenv patvarmap
      in
      let (e2, ty2) = typecheck_iter tyenvnew utast2 in
        (LetNonRecIn(pat, e1, e2), ty2)
(*
      failwith "let nonrec"
*)

  | UTLetRecIn(utrecbinds, utast2) ->
      let (tyenvnew, _, recbinds) = make_type_environment_by_letrec qtfbl lev tyenv utrecbinds in
      let (e2, ty2) = typecheck_iter tyenvnew utast2 in
        (LetRecIn(recbinds, e2), ty2)

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
        | (ContentOf(_, evid), tyvar) ->
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
      let ty = overwrite_range_of_type Primitives.itemize_type rng in
        (eitmz, ty)

(* ---- list ---- *)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      let () = unify tyT (Range.dummy "list-cons", ListType(tyH)) in
      let tyres = (rng, ListType(tyH)) in
        (PrimitiveListCons(eH, eT), tyres)

  | UTEndOfList ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        (Value(EndOfList), (rng, ListType(beta)))

(* ---- tuple ---- *)

  | UTTupleCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      let tyres =
        match tyT with
        | (_, ProductType(tylist)) -> (rng, ProductType(tyH :: tylist))
        | _                        -> assert false
      in
        (PrimitiveTupleCons(eH, eT), tyres)

  | UTEndOfTuple ->
      (Value(EndOfTuple), (rng, ProductType([])))

(* ---- records ---- *)

  | UTRecord(flutlst) -> typecheck_record qtfbl lev tyenv flutlst rng

  | UTAccessField(utast1, fldnm) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let tvidF = FreeID.fresh UniversalKind qtfbl lev () in
      let betaF = (rng, TypeVariable(ref (Free(tvidF)))) in
      let tvid1 = FreeID.fresh (normalize_kind (RecordKind(Assoc.of_list [(fldnm, betaF)]))) qtfbl lev () in
      let beta1 = (get_range utast1, TypeVariable(ref (Free(tvid1)))) in
      let () = unify beta1 ty1 in
        (AccessField(e1, fldnm), betaF)

(* -- math -- *)

  | UTMath(utmath) ->
      let tymath = (rng, BaseType(MathType)) in
      let utast = typecheck_math qtfbl lev tyenv utmath in
        (utast, tymath)

(* ---- other fundamentals ---- *)

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

(* ---- for lightweight command definition ---- *)
  | UTLexHorz(utastctx, utasth) ->
      let (ectx, tyctx) = typecheck_iter tyenv utastctx in
      let (eh, tyh) = typecheck_iter tyenv utasth in
      let () = unify tyctx (Range.dummy "ut-lex-horz-1", BaseType(ContextType)) in
      let () = unify tyh (Range.dummy "ut-lex-horz-2", BaseType(TextRowType)) in
        (HorzLex(ectx, eh), (rng, BaseType(BoxRowType)))

  | UTLexVert(utastctx, utastv) ->
      let (ectx, tyctx) = typecheck_iter tyenv utastctx in
      let (ev, tyv) = typecheck_iter tyenv utastv in
      let () = unify tyctx (Range.dummy "ut-lex-vert-1", BaseType(ContextType)) in
      let () = unify tyv (Range.dummy "ut-lex-vert-2", BaseType(TextColType)) in
        (HorzLex(ectx, ev), (rng, BaseType(BoxColType)))


and typecheck_command_arguments (tycmd : mono_type) (rngcmdapp : Range.t) qtfbl lev tyenv (utcmdarglst : untyped_command_argument list) (cmdargtylst : command_argument_type list) : abstract_tree list =
  let rec aux eacc utcmdarglst cmdargtylst =
    match (utcmdarglst, cmdargtylst) with
    | ([], _) ->
        let eaccnew =
          cmdargtylst |> List.fold_left (fun eacc cmdargty ->
            match cmdargty with
            | MandatoryArgumentType(ty) -> raise (NeedsMoreArgument(rngcmdapp, tyenv, tycmd, ty))
            | OptionalArgumentType(_)   -> Alist.extend eacc (Value(Constructor("None", UnitConstant)))
          ) eacc
        in
          Alist.to_list eaccnew

    | (_ :: _, []) ->
        raise (TooManyArgument(rngcmdapp, tyenv, tycmd))

    | (UTMandatoryArgument(_) :: _, OptionalArgumentType(_) :: cmdargtytail) ->
        let enone = Value(Constructor("None", UnitConstant)) in
          aux (Alist.extend eacc enone) utcmdarglst cmdargtytail

    | (UTMandatoryArgument(utastA) :: utcmdargtail, MandatoryArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
        let () = unify_ tyenv tyA tyreq in
          aux (Alist.extend eacc eA) utcmdargtail cmdargtytail

    | (UTOptionalArgument(utastA) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
        let () = unify_ tyenv tyA tyreq in
        let esome = NonValueConstructor("Some", eA) in
          aux (Alist.extend eacc esome) utcmdargtail cmdargtytail

    | (UTOptionalArgument((rngA, _)) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tyenv, tycmd, rngA))

    | (UTOmission(_) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        let enone = Value(Constructor("None", UnitConstant)) in
          aux (Alist.extend eacc enone) utcmdargtail cmdargtytail

    | (UTOmission(rngA) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tyenv, tycmd, rngA))
  in
  aux Alist.empty utcmdarglst cmdargtylst


and typecheck_math qtfbl lev tyenv ((rng, utmathmain) : untyped_math) : abstract_tree =
  let iter = typecheck_math qtfbl lev tyenv in
  let open HorzBox in
    match utmathmain with
    | UTMChar(s) ->
        Value(MathValue[MathPure(MathVariantChar(s))])

    | UTMList(utmathlst) ->
        let astlst = utmathlst |> List.map iter in
          BackendMathList(astlst)

    | UTMSubScript(utmathB, utmathS) ->
        let astB = iter utmathB in
        let astS = iter utmathS in
          BackendMathSubscript(astB, astS)

    | UTMSuperScript(utmathB, utmathS) ->
        let astB = iter utmathB in
        let astS = iter utmathS in
          BackendMathSuperscript(astB, astS)

    | UTMCommand(utastcmd, utcmdarglst) ->
        let (ecmd, tycmd) = typecheck qtfbl lev tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with
          | MathCommandType(cmdargtylstreq) ->
              let elstarg = typecheck_command_arguments tycmd rng qtfbl lev tyenv utcmdarglst cmdargtylstreq in
(*
              let trilst =
                utmatharglst |> List.map (function
                  | UTMMandatoryArgument((rngA, _) as utastA) ->
                      let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
                        (rngA, eA, MandatoryArgumentType(tyA))

                  | UTMOptionalArgument((rngA, _) as utastA) ->
                      let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
                        (rngA, NonValueConstructor("Some", eA), OptionalArgumentType(tyA))

                  | UTMOmission(rngomit) ->
                      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
                      let beta = (rngomit, TypeVariable(ref (Free(tvid)))) in
                        (rngomit, Value(Constructor("None", UnitConstant)), OptionalArgumentType(beta))
                ) in
              let cmdargtylist = trilst |> List.map (fun (_, _, cmdargty) -> cmdargty) in
              let () = unify_command_argument_types cmdargtylist cmdargtylistreq in
              let elstarg = trilst |> List.map (fun (_, e, _) -> e) in
*)
(*
                try
                  List.iter2 (fun caty catyreq ->
                    match (caty, catyreq) with
                    | (MandatoryArgumentType(ty), MandatoryArgumentType(tyreq)) -> unify_ tyenv ty tyreq
                    | (OptionalArgumentType(ty) , OptionalArgumentType(tyreq) ) -> unify_ tyenv ty tyreq
                    | _                                                         -> assert false  (* TEMPORARY *)
                  ) cmdargtylist cmdargtylistreq
                with
                | Invalid_argument(_) ->
                    let lenreq  = List.length cmdargtylistreq in
                    let lenreal = List.length cmdargtylist in
                    raise (InvalidArityOfCommand(rng, lenreq, lenreal))
              in
*)
                apply_tree_of_list ecmd elstarg

          | HorzCommandType(_) ->
              let (rngcmd, _) = utastcmd in
              raise (HorzCommandInMath(rngcmd))

          | _ -> assert false
        end

    | UTMEmbed(utast0) ->
        let (e0, ty0) = typecheck qtfbl lev tyenv utast0 in
        let () = unify_ tyenv ty0 (Range.dummy "math-embedded-var", BaseType(MathType)) in
          e0


and typecheck_path qtfbl lev tyenv (utpathcomplst : (untyped_abstract_tree untyped_path_component) list) (utcycleopt : (unit untyped_path_component) option) =

  let typecheck_anchor_point utastpt =
    let (ept, typt) = typecheck qtfbl lev tyenv utastpt in
    let () = unify_ tyenv typt (Range.dummy "typecheck-path", point_type_main) in
      ept
  in

  let pathcompacc =
    utpathcomplst |> List.fold_left (fun acc utpathcomp ->
      match utpathcomp with
      | UTPathLineTo(utastpt) ->
          let (ept, typt) = typecheck qtfbl lev tyenv utastpt in
          let () = unify_ tyenv typt (Range.dummy "typecheck-path-L", point_type_main) in
            PathLineTo(ept) :: acc

      | UTPathCubicBezierTo(utastpt1, utastpt2, utastpt) ->
          let ept1 = typecheck_anchor_point utastpt1 in
          let ept2 = typecheck_anchor_point utastpt2 in
          let ept = typecheck_anchor_point utastpt in
            PathCubicBezierTo(ept1, ept2, ept) :: acc
    ) []
  in
  let cycleopt =
    utcycleopt |> option_map (function
      | UTPathLineTo(()) -> PathLineTo(())

      | UTPathCubicBezierTo(utastpt1, utastpt2, ()) ->
          let ept1 = typecheck_anchor_point utastpt1 in
          let ept2 = typecheck_anchor_point utastpt2 in
            PathCubicBezierTo(ept1, ept2, ())
    )
  in
    (List.rev pathcompacc, cycleopt)


and typecheck_input_vert (rng : Range.t) (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t) (utivlst : untyped_input_vert_element list) =
  let rec aux (acc : input_vert_element list) (lst : untyped_input_vert_element list) =
    match lst with
    | [] ->
        List.rev acc

    | (_, UTInputVertEmbedded((rngcmd, _) as utastcmd, utcmdarglst)) :: tail ->
        let (ecmd, tycmd) = typecheck qtfbl lev tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with

          | VertCommandType(cmdargtylstreq) ->
              let rngcmdapp =
                match List.rev utcmdarglst with
                | []                                 -> rngcmd
                | UTMandatoryArgument((rng, _)) :: _ -> Range.unite rngcmd rng
                | UTOptionalArgument((rng, _)) :: _  -> Range.unite rngcmd rng
                | UTOmission(rng) :: _               -> Range.unite rngcmd rng
              in
              let elstarg = typecheck_command_arguments tycmd rngcmdapp qtfbl lev tyenv utcmdarglst cmdargtylstreq in
(*
              let trilst =
                List.map (function
                | UTMandatoryArgument(utastA) ->
                    let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
                      (eA, MandatoryArgumentType(tyA))

                | UTOptionalArgument(utastA) ->
                    let (eA, tyA) = typecheck qtfbl lev tyenv utastA in
                      (NonValueConstructor("Some", eA), OptionalArgumentType(tyA))

                | UTOmission(rngomit) ->
                    let tvid = FreeID.fresh UniversalKind qtfbl lev () in
                    let beta = (rngomit, TypeVariable(ref (Free(tvid)))) in
                      (Value(Constructor("None", UnitConstant)), OptionalArgumentType(beta))

                ) utcmdarglst
              in
              let tylstarg = etylst |> List.map (fun (e, ty) -> ty) in
              let () = unify_command_argument_types tyenv  in
              let elstarg = etylst |> List.map (fun (e, ty) -> e) in
*)
(*
              let () =
                try List.iter2 (unify_ tyenv) tylstarg tylstreq with
                | Invalid_argument(_) ->
                    let lenreq  = List.length tylstreq in
                    let lenreal = List.length tylstarg in
                    raise (InvalidArityOfCommand(rng, lenreq, lenreal))
              in
*)
                aux (InputVertEmbedded(ecmd, elstarg) :: acc) tail

          | _ -> assert false
        end

    | (_, UTInputVertContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck qtfbl lev tyenv utast0 in
        let () = unify_ tyenv ty0 (Range.dummy "UTInputVertContent", BaseType(TextColType)) in
          aux (InputVertContent(e0) :: acc) tail
  in
    aux [] utivlst



and typecheck_input_horz (rng : Range.t) (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : Typeenv.t) (utihlst : untyped_input_horz_element list) =
  let rec aux (acc : input_horz_element Alist.t) (lst : untyped_input_horz_element list) =
    match lst with
    | [] -> Alist.to_list acc

    | (_, UTInputHorzEmbedded((rngcmd, _) as utastcmd, utcmdarglst)) :: tail ->
        let rngcmdapp =
          match List.rev utcmdarglst with
          | []                                 -> rngcmd
          | UTMandatoryArgument((rng, _)) :: _ -> Range.unite rngcmd rng
          | UTOptionalArgument((rng, _)) :: _  -> Range.unite rngcmd rng
          | UTOmission(rng) :: _               -> Range.unite rngcmd rng
        in
        let (ecmd, tycmd) = typecheck qtfbl lev tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with

          | HorzCommandType(cmdargtylstreq) ->
              let elstarg = typecheck_command_arguments tycmd rngcmdapp qtfbl lev tyenv utcmdarglst cmdargtylstreq in
(*
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
*)
                aux (Alist.extend acc (InputHorzEmbedded(ecmd, elstarg))) tail

          | MathCommandType(_) ->
              let (rngcmd, _) = utastcmd in
              raise (MathCommandInHorz(rngcmd))

          | _ -> assert false
        end

    | (_, UTInputHorzEmbeddedMath(utastmath)) :: tail ->
        let (emath, tymath) = typecheck qtfbl lev tyenv utastmath in
        let () = unify_ tyenv tymath (Range.dummy "ut-input-horz-embedded-math", BaseType(MathType)) in
          aux (Alist.extend acc (InputHorzEmbeddedMath(emath))) tail

    | (_, UTInputHorzContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck qtfbl lev tyenv utast0 in
        let () = unify_ tyenv ty0 (Range.dummy "ut-input-horz-content", BaseType(TextRowType)) in
          aux (Alist.extend acc (InputHorzContent(e0))) tail

    | (_, UTInputHorzText(s)) :: tail ->
        aux (Alist.extend acc (InputHorzText(s))) tail
  in
    aux Alist.empty utihlst


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
  let () = unify_ tyenv ty1 (Range.dummy "typecheck_itemize_string", BaseType(TextRowType)) in
  let elst = typecheck_itemize_list qtfbl lev tyenv utitmzlst in
    (NonValueConstructor("Item", PrimitiveTupleCons(e1, PrimitiveTupleCons(elst, Value(EndOfTuple)))))


and typecheck_itemize_list
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utitmzlst : untyped_itemize list) =
  match utitmzlst with
  | []                  -> Value(EndOfList)
  | hditmz :: tlitmzlst ->
      let ehd = typecheck_itemize qtfbl lev tyenv hditmz in
      let etl = typecheck_itemize_list qtfbl lev tyenv tlitmzlst in
        PrimitiveListCons(ehd, etl)


and typecheck_pattern_branch_list
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utpatbrs : untyped_pattern_branch list) (tyobj : mono_type) (tyres : mono_type) =
  let iter = typecheck_pattern_branch_list qtfbl lev in
  let unify = unify_ tyenv in
    match utpatbrs with
    | [] -> ([], tyres)

    | UTPatternBranch(utpat, utast1) :: tail ->
        let (epat, typat, patvarmap) = typecheck_pattern qtfbl lev tyenv utpat in
        let () = unify typat tyobj in
        let tyenvpat = add_pattern_var_mono tyenv patvarmap in
        let (e1, ty1) = typecheck qtfbl lev tyenvpat utast1 in
        let () = unify ty1 tyres in
        let (patbrtail, tytail) = iter tyenv tail tyobj tyres in
          (PatternBranch(epat, e1) :: patbrtail, tytail)

    | UTPatternBranchWhen(utpat, utastB, utast1) :: tail ->
        let (epat, typat, patvarmap) = typecheck_pattern qtfbl lev tyenv utpat in
        let () = unify typat tyobj in
        let tyenvpat = add_pattern_var_mono tyenv patvarmap in
        let (eB, tyB) = typecheck qtfbl lev tyenvpat utastB in
        let () = unify tyB (Range.dummy "pattern-match-cons-when", BaseType(BoolType)) in
        let (e1, ty1) = typecheck qtfbl lev tyenvpat utast1 in
        let () = unify ty1 tyres in
        let (patbrtail, tytail) = iter tyenv tail tyobj tyres in
          (PatternBranchWhen(epat, eB, e1) :: patbrtail, tytail)


and typecheck_pattern
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t)
    ((rng, utpatmain) : untyped_pattern_tree) : pattern_tree * mono_type * pattern_var_map =
  let iter = typecheck_pattern qtfbl lev tyenv in
  let unify = unify_ tyenv in
    match utpatmain with
    | UTPIntegerConstant(nc) -> (PIntegerConstant(nc), (rng, BaseType(IntType)), PatternVarMap.empty)
    | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BaseType(BoolType)), PatternVarMap.empty)
    | UTPUnitConstant        -> (PUnitConstant, (rng, BaseType(UnitType)), PatternVarMap.empty)

    | UTPStringConstant(ut1) ->
        let (e1, ty1) = typecheck qtfbl lev tyenv ut1 in
        let () = unify (Range.dummy "pattern-string-constant", BaseType(StringType)) ty1 in
          (PStringConstant(e1), (rng, BaseType(StringType)), PatternVarMap.empty)

    | UTPListCons(utpat1, utpat2) ->
        let (epat1, typat1, patvarmap1) = iter utpat1 in
        let (epat2, typat2, patvarmap2) = iter utpat2 in
        let () = unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
        let patvarmap = unite_pattern_var_map patvarmap1 patvarmap2 in
          (PListCons(epat1, epat2), typat2, patvarmap)

    | UTPEndOfList ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
          (PEndOfList, (rng, ListType(beta)), PatternVarMap.empty)

    | UTPTupleCons(utpat1, utpat2) ->
        let (epat1, typat1, patvarmap1) = iter utpat1 in
        let (epat2, typat2, patvarmap2) = iter utpat2 in
        let tyres =
          match typat2 with
          | (rng, ProductType(tylist)) -> (rng, ProductType(typat1 :: tylist))
          | _                          -> assert false
        in
        let patvarmap = unite_pattern_var_map patvarmap1 patvarmap2 in
          (PTupleCons(epat1, epat2), tyres, patvarmap)

    | UTPEndOfTuple -> (PEndOfTuple, (rng, ProductType([])), PatternVarMap.empty)

    | UTPWildCard ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
          (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        let evid = EvalVarID.fresh varnm in
        let () = print_for_debug_typecheck ("\n#PAdd " ^ varnm ^ " : " ^ (string_of_mono_type_basic beta)) in  (* for debug *)
          (PVariable(evid), beta, PatternVarMap.empty |> PatternVarMap.add varnm (rng, evid, beta))

    | UTPAsVariable(varnm, utpat1) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (Free(tvid)))) in
        let (epat1, typat1, patvarmap1) = iter utpat1 in
        begin
          match PatternVarMap.find_opt varnm patvarmap1 with
          | Some((rngsub, _, _)) ->
            (* -- if 'varnm' also occurs in 'utpat1' -- *)
              raise (MultiplePatternVariable(rngsub, rng, varnm))

          | None ->
              let evid = EvalVarID.fresh varnm in
                (PAsVariable(evid, epat1), typat1, patvarmap1 |> PatternVarMap.add varnm (rng, evid, beta))
        end

    | UTPConstructor(constrnm, utpat1) ->
        begin
          match Typeenv.find_constructor qtfbl tyenv lev constrnm with
          | None -> raise (UndefinedConstructor(rng, constrnm))
          | Some((tyarglist, tyid, tyc)) ->
              let () = print_for_debug_typecheck ("P-find " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc)) in (* for debug *)
              let (epat1, typat1, tyenv1) = iter utpat1 in
              let () = unify tyc typat1 in
                (PConstructor(constrnm, epat1), (rng, VariantType(tyarglist, tyid)), tyenv1)
        end


and make_type_environment_by_letrec
    (qtfbl : quantifiability) (lev : FreeID.level)
    (tyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) =

  let rec add_mutual_variables (acctyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) : (Typeenv.t * (var_name * mono_type * EvalVarID.t) list) =
    let iter = add_mutual_variables in
      match utrecbinds with
      | [] ->
          (acctyenv, [])

      | UTLetRecBinding(_, varnm, astdef) :: tailcons ->
          let tvid = FreeID.fresh UniversalKind qtfbl (FreeID.succ_level lev) () in
          let beta = (get_range astdef, TypeVariable(ref (Free(tvid)))) in
          let _ = print_for_debug_typecheck ("#AddMutualVar " ^ varnm ^ " : '" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ " :: U") in (* for debug *)
          let evid = EvalVarID.fresh varnm in
          let (tyenvfinal, tvtylst) = iter (Typeenv.add acctyenv varnm (Poly(beta), evid)) tailcons in
            (tyenvfinal, ((varnm, beta, evid) :: tvtylst))
  in

  let rec typecheck_mutual_contents
      (lev : FreeID.level)
      (tyenvforrec : Typeenv.t) (utrecbinds : untyped_letrec_binding list) (tvtylst : (var_name * mono_type * EvalVarID.t) list)
      (acctvtylstout : (var_name * mono_type * EvalVarID.t) list)
  =
    let iter = typecheck_mutual_contents lev in
    let unify = unify_ tyenv in
    match (utrecbinds, tvtylst) with
    | ([], []) -> (tyenvforrec, [], List.rev acctvtylstout)

    | (UTLetRecBinding(mntyopt, varnm, utast1) :: tailcons, (_, beta, evid) :: tvtytail) ->
        let (e1, ty1) = typecheck qtfbl (FreeID.succ_level lev) tyenvforrec utast1 in
        begin
          match mntyopt with
          | None ->
              let () = unify ty1 beta in
              let (tyenvfinal, recbindtail, tvtylstoutfinal) =
                iter tyenvforrec tailcons tvtytail ((varnm, beta, evid) :: acctvtylstout)
              in
              begin
                match e1 with
                | Function(patbrs1) -> (tyenvfinal, LetRecBinding(evid, patbrs1) :: recbindtail, tvtylstoutfinal)
                | _                 -> let (rng1, _) = utast1 in raise (BreaksValueRestriction(rng1))
              end

          | Some(mnty) ->
              let tyin = Typeenv.fix_manual_type_free qtfbl tyenv lev mnty [] in
              let () = unify ty1 beta in
              let () = unify tyin beta in
              let (tyenvfinal, recbindtail, tvtylstoutfinal) =
                iter tyenvforrec tailcons tvtytail ((varnm, beta, evid) :: acctvtylstout)
              in
              begin
                match e1 with
                | Function(patbrs1) -> (tyenvfinal, LetRecBinding(evid, patbrs1) :: recbindtail, tvtylstoutfinal)
                | _                 -> let (rng1, _) = utast1 in raise (BreaksValueRestriction(rng1))
              end
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

  let (tyenvforrec, tvtylstforrec) = add_mutual_variables tyenv utrecbinds in
  let (tyenv_new, mutletcons, tvtylstout) =
        typecheck_mutual_contents lev tyenvforrec utrecbinds tvtylstforrec [] in
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
    (* Format.printf "%a" pp_untyped_abstract_tree utast; *)
    let (e, ty) = typecheck Quantifiable FreeID.bottom_level tyenv utast in
      (ty, !final_tyenv, e)
  end
