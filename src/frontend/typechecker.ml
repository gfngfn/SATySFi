
open MyUtil
open Types
open Display

exception UndefinedVariable              of Range.t * module_name list * var_name * var_name list
exception UndefinedConstructor           of Range.t * var_name * var_name list
exception InclusionError                 of Typeenv.t * mono_type * mono_type
exception ContradictionError             of Typeenv.t * mono_type * mono_type
exception UnknownUnitOfLength            of Range.t * length_unit_name
exception HorzCommandInMath              of Range.t
exception MathCommandInHorz              of Range.t
exception BreaksValueRestriction         of Range.t
exception MultiplePatternVariable        of Range.t * Range.t * var_name
exception InvalidOptionalCommandArgument of Typeenv.t * mono_type * Range.t
exception NeedsMoreArgument              of Range.t * Typeenv.t * mono_type * mono_type
exception TooManyArgument                of Range.t * Typeenv.t * mono_type
exception MultipleFieldInRecord          of Range.t * field_name
exception ApplicationOfNonFunction       of Range.t * Typeenv.t * mono_type
exception InvalidExpressionAsToStaging   of Range.t * stage
exception InvalidOccurrenceAsToStaging   of Range.t * var_name * stage
exception UndefinedHorzMacro             of Range.t * ctrlseq_name
exception InvalidNumberOfMacroArguments  of Range.t * macro_parameter_type list
exception LateMacroArgumentExpected      of Range.t * mono_type
exception EarlyMacroArgumentExpected     of Range.t * mono_type

exception InternalInclusionError
exception InternalContradictionError of bool


let abstraction evid ast =
  Function([], PatternBranch(PVariable(evid), ast))


let abstraction_list evids ast =
  List.fold_right abstraction evids ast


let add_optionals_to_type_environment (tyenv : Typeenv.t) (pre : pre) (optargs : (Range.t * var_name) list) : mono_option_row * EvalVarID.t list * Typeenv.t =
  let qtfbl = pre.quantifiability in
  let lev = pre.level in
  let (tyenvnew, tyacc, evidacc) =
    optargs |> List.fold_left (fun (tyenv, tyacc, evidacc) (rng, varnm) ->
      let evid = EvalVarID.fresh (rng, varnm) in
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let tvref = ref (MonoFree(tvid)) in
      let beta = (rng, TypeVariable(PolyFree(tvref))) in
      let tyenvnew = Typeenv.add tyenv varnm (Poly(Primitives.option_type beta), evid, pre.stage) in
        (tyenvnew, Alist.extend tyacc (rng, TypeVariable(tvref)), Alist.extend evidacc evid)
    ) (tyenv, Alist.empty, Alist.empty)
  in
  let optrow =
    List.fold_right (fun ty acc ->
      OptionRowCons(ty, acc)
    ) (Alist.to_list tyacc) OptionRowEmpty
  in
  (optrow, Alist.to_list evidacc, tyenvnew)


let add_macro_parameters_to_type_environment (tyenv : Typeenv.t) (pre : pre) (macparams : untyped_macro_parameter list) : Typeenv.t * EvalVarID.t list * macro_parameter_type list =
  let (tyenv, evidacc, macptyacc) =
    macparams |> List.fold_left (fun (tyenv, evidacc, macptyacc) macparam ->
      let (param, stage) =
        match macparam with
        | UTLateMacroParam(param) -> (param, Stage1)
        | UTEarlyMacroParam(param) -> (param, Stage0)
      in
      let (rng, varnm) = param in
      let evid = EvalVarID.fresh param in
      let (pty, beta) =
        let tvid = FreeID.fresh UniversalKind pre.quantifiability pre.level () in
        let tvref = ref (MonoFree(tvid)) in
        (Poly(rng, TypeVariable(PolyFree(tvref))), (rng, TypeVariable(tvref)))
      in
      let macpty =
        match macparam with
        | UTLateMacroParam(_)  -> LateMacroParameter(beta)
        | UTEarlyMacroParam(_) -> EarlyMacroParameter(beta)
      in
      (Typeenv.add tyenv varnm (pty, evid, Stage1), Alist.extend evidacc evid, Alist.extend macptyacc macpty)
    ) (tyenv, Alist.empty, Alist.empty)
  in
  (tyenv, Alist.to_list evidacc, Alist.to_list macptyacc)


let rec is_nonexpansive_expression e =
  let iter = is_nonexpansive_expression in
  match e with
  | ASTBaseConstant(_)
  | ASTEndOfList
  | ASTMath(_)
  | Function(_)
  | ContentOf(_) ->
      true

  | NonValueConstructor(constrnm, e1) -> iter e1
  | PrimitiveListCons(e1, e2)         -> iter e1 && iter e2
  | PrimitiveTuple(elst)              -> List.for_all iter elst
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


let add_pattern_var_mono (pre : pre) (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = lift_poly (erase_range_of_type ty) in
    Typeenv.add tyenvacc varnm (pty, evid, pre.stage)
  ) patvarmap tyenv


let add_pattern_var_poly (pre : pre) (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = (generalize pre.level (erase_range_of_type ty)) in
    Typeenv.add tyenvacc varnm (pty, evid, pre.stage)
  ) patvarmap tyenv


let point_type_main =
  (ProductType([
    (Range.dummy "point-type-1", BaseType(LengthType));
    (Range.dummy "point-type-2", BaseType(LengthType));
  ]))


(* -- 'apply_tree_of_list': converts e0 and [e1; ...; eN] into (e0 e1 ... eN) -- *)
let apply_tree_of_list astfunc astlst =
  List.fold_left (fun astf astx -> Apply(astf, astx)) astfunc astlst


(* -- 'flatten_type': converts type (t1 -> ... -> tN -> t) into ([t1; ...; tN], t) -- *)
let flatten_type (ty : mono_type) : mono_command_argument_type list * mono_type =

  let rec aux_or = function
    | OptionRowEmpty                                     -> []
    | OptionRowVariable({contents = MonoORFree(_)})      -> []
    | OptionRowVariable({contents = MonoORLink(optrow)}) -> aux_or optrow
    | OptionRowCons(ty, tail)                            -> OptionalArgumentType(ty) :: aux_or tail
  in

  let rec aux acc ty =
    let (rng, tymain) = ty in
      match tymain with
      | TypeVariable({contents= MonoLink(tylink)}) ->
          aux acc tylink

      | FuncType(optrow, tydom, tycod) ->
          let accnew =
            Alist.append acc (List.append (aux_or optrow) [MandatoryArgumentType(tydom)])
          in
          aux accnew tycod

      | _ -> (Alist.to_list acc, ty)
  in
  aux Alist.empty ty


let occurs (tvid : FreeID.t) (ty : mono_type) =

  let lev = FreeID.get_level tvid in

  let rec iter (_, tymain) =

    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | MonoLink(tyl) ->
              iter tyl

          | MonoFree(tvidx) ->
              if FreeID.equal tvidx tvid then
                true
              else
                let levx = FreeID.get_level tvidx in
                if Level.less_than lev levx then begin
                  FreeID.set_level tvidx lev
                    (* -- update level -- *)
                end;
                begin
                  match FreeID.get_kind tvidx with
                  | UniversalKind     -> false
                  | RecordKind(tyasc) -> Assoc.fold_value (fun b ty -> b || iter ty) false tyasc
                end
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_or optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tylist)            -> iter_list tylist
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub
    | VariantType(tylist, _)         -> iter_list tylist
    | SynonymType(tylist, _, tyact)  -> let b = iter_list tylist in let ba = iter tyact in b || ba
    | RecordType(tyasc)              -> iter_list (Assoc.to_value_list tyasc)
    | BaseType(_)                    -> false
    | HorzCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | VertCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | MathCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | CodeType(tysub)                -> iter tysub

  and iter_list tylst =
    List.exists iter tylst

  and iter_cmd_list cmdargtylist =
    List.exists (function
      | MandatoryArgumentType(ty) -> iter ty
      | OptionalArgumentType(ty)  -> iter ty
    ) cmdargtylist

  and iter_or optrow =
    match optrow with
    | OptionRowEmpty ->
        false

    | OptionRowCons(ty, tail) ->
        let b1 = iter ty in
        let b2 = iter_or tail in
        b1 || b2

    | OptionRowVariable(orviref) ->
        begin
          match !orviref with
          | MonoORLink(optrow) ->
              iter_or optrow

          | MonoORFree(orvx) ->
              let levx = OptionRowVarID.get_level orvx in
              if Level.less_than lev levx then begin
                orviref := MonoORFree(OptionRowVarID.set_level orvx lev)
                  (* -- update level -- *)
              end;
              false
        end

  in
  iter ty


let occurs_optional_row (orv : OptionRowVarID.t) (optrow : mono_option_row) =

  let lev = OptionRowVarID.get_level orv in

  let rec iter (_, tymain) =
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | MonoLink(tyl) ->
              iter tyl

          | MonoFree(tvidx) ->
              let levx = FreeID.get_level tvidx in
              if Level.less_than lev levx then begin
                FreeID.set_level tvidx lev
                  (* -- update level -- *)
              end;
              begin
                match FreeID.get_kind tvidx with
                | UniversalKind     -> false
                | RecordKind(tyasc) -> Assoc.fold_value (fun bacc ty -> let b = iter ty in bacc || b) false tyasc
              end
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_or optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tylist)            -> iter_list tylist
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub
    | VariantType(tylist, _)         -> iter_list tylist
    | SynonymType(tylist, _, tyact)  -> let b = iter_list tylist in let ba = iter tyact in b || ba
    | RecordType(tyasc)              -> iter_list (Assoc.to_value_list tyasc)
    | BaseType(_)                    -> false
    | HorzCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | VertCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | MathCommandType(cmdargtylist)  -> iter_cmd_list cmdargtylist
    | CodeType(tysub)                -> iter tysub

  and iter_list tylst =
    List.exists iter tylst

  and iter_cmd_list cmdargtylist =
    List.exists (function
      | MandatoryArgumentType(ty) -> iter ty
      | OptionalArgumentType(ty)  -> iter ty
    ) cmdargtylist

  and iter_or = function
    | OptionRowEmpty ->
        false

    | OptionRowCons(ty, tail) ->
        let b1 = iter ty in
        let b2 = iter_or tail in
        b1 || b2

    | OptionRowVariable(orviref) ->
        begin
          match !orviref with
          | MonoORLink(optrow) ->
              iter_or optrow

          | MonoORFree(orvx) ->
              if OptionRowVarID.equal orv orvx then
                true
              else
                let levx = OptionRowVarID.get_level orvx in
                if Level.less_than lev levx then begin
                  orviref := MonoORFree(OptionRowVarID.set_level orvx lev)
                    (* -- update level -- *)
                end;
                false
        end

  in
  iter_or optrow


let set_kind_with_occurs_check (tvid : FreeID.t) (kd : mono_kind) : unit =
  begin
    match kd with
    | UniversalKind ->
        ()

    | RecordKind(tyasc) ->
        let b = Assoc.fold_value (fun bacc ty -> let b = occurs tvid ty in bacc || b) false tyasc in
        if b then raise InternalInclusionError else ()
  end;
  FreeID.set_kind kd tvid


let rec unify_sub ~reversed:reversed ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) =
  let unify = unify_sub ~reversed:reversed in
(*
  (* begin: for debug *)
  let () =
    match (tymain1, tymain2) with
    | (TypeVariable({contents = MonoLink(_)}), _) -> ()
    | (_, TypeVariable({contents = MonoLink(_)})) -> ()
    | _ -> print_endline ("    | unify " ^ (string_of_mono_type_basic ty1) ^ " == " ^ (string_of_mono_type_basic ty2))
  in
  (* end: for debug *)
*)
  let unify_list = List.iter (fun (t1, t2) -> unify t1 t2) in

    match (tymain1, tymain2) with

    | (SynonymType(_, _, tyreal1), _) -> unify tyreal1 ty2
    | (_, SynonymType(_, _, tyreal2)) -> unify ty1 tyreal2

    | (BaseType(bsty1), BaseType(bsty2))  when bsty1 = bsty2 -> ()

    | (FuncType(optrow1, tydom1, tycod1), FuncType(optrow2, tydom2, tycod2)) ->
        begin
          unify_option_row ~reversed optrow1 optrow2;
          unify tydom1 tydom2;
          unify tycod1 tycod2;
        end

    | (HorzCommandType(cmdargtylist1), HorzCommandType(cmdargtylist2))
    | (VertCommandType(cmdargtylist1), VertCommandType(cmdargtylist2))
    | (MathCommandType(cmdargtylist1), MathCommandType(cmdargtylist2)) ->
        begin
          try
            List.iter2 (fun cmdargty1 cmdargty2 ->
              match (cmdargty1, cmdargty2) with
              | (MandatoryArgumentType(ty1), MandatoryArgumentType(ty2)) -> unify ty1 ty2
              | (OptionalArgumentType(ty1) , OptionalArgumentType(ty2) ) -> unify ty1 ty2
              | _ -> raise (InternalContradictionError(reversed))
            ) cmdargtylist1 cmdargtylist2
          with
          | Invalid_argument(_) ->
              raise (InternalContradictionError(reversed))
        end

    | (ProductType(tylist1), ProductType(tylist2)) ->
        begin
          try
            unify_list (List.combine tylist1 tylist2)
          with
          | Invalid_argument(_) ->  (* -- not of the same length -- *)
              raise (InternalContradictionError(reversed))
        end

    | (RecordType(tyasc1), RecordType(tyasc2)) ->
        if not (Assoc.domain_same tyasc1 tyasc2) then
          raise (InternalContradictionError(reversed))
        else
          unify_list (Assoc.combine_value tyasc1 tyasc2)

    | (VariantType(tyarglist1, tyid1), VariantType(tyarglist2, tyid2))
        when tyid1 = tyid2 ->
        begin
          try
            unify_list (List.combine tyarglist1 tyarglist2)
          with
          | Invalid_argument(_) ->
              raise (InternalContradictionError(reversed))
        end

    | (ListType(tysub1), ListType(tysub2)) -> unify tysub1 tysub2
    | (RefType(tysub1), RefType(tysub2))   -> unify tysub1 tysub2
    | (CodeType(tysub1), CodeType(tysub2)) -> unify tysub1 tysub2

    | (TypeVariable({contents= MonoLink(tylinked1)}), _) -> unify tylinked1 ty2
    | (_, TypeVariable({contents= MonoLink(tylinked2)})) -> unify ty1 tylinked2

    | (TypeVariable({contents= MonoFree(tvid1)} as tvref1), TypeVariable({contents= MonoFree(tvid2)} as tvref2)) ->
        if FreeID.equal tvid1 tvid2 then
          ()
        else
          let b1 = occurs tvid1 ty2 in
          let b2 = occurs tvid2 ty1 in
          if b1 || b2 then
            raise InternalInclusionError
          else
            if FreeID.is_quantifiable tvid1 && FreeID.is_quantifiable tvid2 then
              ()
            else begin
              FreeID.set_quantifiability Unquantifiable tvid1;
              FreeID.set_quantifiability Unquantifiable tvid2;
            end;
            let lev1 = FreeID.get_level tvid1 in
            let lev2 = FreeID.get_level tvid2 in
            begin
              if Level.less_than lev1 lev2 then
                FreeID.set_level tvid2 lev1
              else if Level.less_than lev2 lev1 then
                FreeID.set_level tvid1 lev2
              else
                ()
            end;
            let (oldtvref, newtvref, newtvid, newty) =
              if Range.is_dummy rng1 then (tvref1, tvref2, tvid2, ty2) else (tvref2, tvref1, tvid1, ty1)
            in
            oldtvref := MonoLink(newty);
            let (eqnlst, kdunion) =
              let kd1 = FreeID.get_kind tvid1 in
              let kd2 = FreeID.get_kind tvid2 in
              match (kd1, kd2) with
              | (UniversalKind, UniversalKind)       -> ([], UniversalKind)
              | (RecordKind(asc1), UniversalKind)    -> ([], RecordKind(asc1))
              | (UniversalKind, RecordKind(asc2))    -> ([], RecordKind(asc2))

              | (RecordKind(asc1), RecordKind(asc2)) ->
                  let kdunion = RecordKind(Assoc.union asc1 asc2) in
                  (Assoc.intersection asc1 asc2, kdunion)
            in
            unify_list eqnlst;
            set_kind_with_occurs_check newtvid kdunion

      | (TypeVariable({contents= MonoFree(tvid1)} as tvref1), RecordType(tyasc2)) ->
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
            raise (InternalContradictionError(reversed))
          else
            let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
            let eqnlst =
              match kd1 with
              | UniversalKind      -> []
              | RecordKind(tyasc1) -> Assoc.intersection tyasc1 tyasc2
            in
            unify_list eqnlst;
            tvref1 := MonoLink(newty2)

      | (TypeVariable({contents= MonoFree(tvid1)} as tvref1), _) ->
          let kd1 = FreeID.get_kind tvid1 in
          let () =
            match kd1 with
            | UniversalKind -> ()
            | RecordKind(_) -> raise (InternalContradictionError(reversed))
                (* -- `ty2` is not a record type, a type variable, or a link,
                      and thereby cannot have a record kind -- *)
          in
          let chk = occurs tvid1 ty2 in
          if chk then
            raise InternalInclusionError
          else
            let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
            tvref1 := MonoLink(newty2)

      | (_, TypeVariable(_)) ->
          unify_sub ~reversed:(not reversed) ty2 ty1

      | _ ->
          raise (InternalContradictionError(reversed))


and unify_option_row ~reversed:reversed optrow1 optrow2 =
  match (optrow1, optrow2) with
  | (OptionRowCons(ty1, tail1), OptionRowCons(ty2, tail2)) ->
      unify_sub ~reversed ty1 ty2;
      unify_option_row ~reversed tail1 tail2

  | (OptionRowEmpty, OptionRowEmpty) ->
      ()

  | (OptionRowVariable({contents = MonoORLink(optrow1)}), _) -> unify_option_row ~reversed optrow1 optrow2
  | (_, OptionRowVariable({contents = MonoORLink(optrow2)})) -> unify_option_row ~reversed optrow1 optrow2

  | (OptionRowVariable({contents = MonoORFree(orv1)} as orviref1), OptionRowVariable({contents = MonoORFree(orv2)})) ->
      if OptionRowVarID.equal orv1 orv2 then () else
        orviref1 := MonoORLink(optrow2)

  | (OptionRowVariable({contents = MonoORFree(orv1)} as orviref1), _) ->
      if occurs_optional_row orv1 optrow2 then
        raise InternalInclusionError
      else
        orviref1 := MonoORLink(optrow2)

  | (_, OptionRowVariable({contents = MonoORFree(orv2)} as orviref2)) ->
      if occurs_optional_row orv2 optrow1 then
        raise InternalInclusionError
      else
        orviref2 := MonoORLink(optrow1)

  | (OptionRowEmpty, OptionRowCons(_, _))
  | (OptionRowCons(_, _), OptionRowEmpty) ->
      raise (InternalContradictionError(reversed))


let unify_ (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  try
    unify_sub ~reversed:false ty1 ty2
  with
  | InternalInclusionError ->
      raise (InclusionError(tyenv, ty1, ty2))

  | InternalContradictionError(reversed) ->
      if reversed then
        raise (ContradictionError(tyenv, ty2, ty1))
      else
        raise (ContradictionError(tyenv, ty1, ty2))


let final_tyenv : Typeenv.t ref = ref (Typeenv.empty)


let fresh_type_variable rng pre kd =
  let tvid = FreeID.fresh kd pre.quantifiability pre.level () in
  (rng, TypeVariable(ref (MonoFree(tvid))))


let base bc =
  ASTBaseConstant(bc)


let rec typecheck
    (pre : pre) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?s:(s = pre.stage) ?l:(l = pre.level) ?q:(q = pre.quantifiability) t u =
    typecheck { stage = s; quantifiability = q; level = l; } t u
  in
  let unify = unify_ tyenv in
  match utastmain with
  | UTStringEmpty         -> (base (BCString(""))   , (rng, BaseType(StringType)))
  | UTIntegerConstant(nc) -> (base (BCInt(nc))      , (rng, BaseType(IntType))   )
  | UTFloatConstant(nc)   -> (base (BCFloat(nc))    , (rng, BaseType(FloatType)) )
  | UTStringConstant(sc)  -> (base (BCString(sc))   , (rng, BaseType(StringType)))
  | UTBooleanConstant(bc) -> (base (BCBool(bc))     , (rng, BaseType(BoolType))  )
  | UTUnitConstant        -> (base BCUnit           , (rng, BaseType(UnitType))  )
  | UTHorz(hblst)         -> (base (BCHorz(hblst))  , (rng, BaseType(BoxRowType)))
  | UTVert(imvblst)       -> (base (BCVert(imvblst)), (rng, BaseType(BoxColType)))

  | UTLengthDescription(flt, unitnm) ->
        let len =
          match unitnm with  (* temporary; ad-hoc handling of unit names *)
          | "pt"   -> Length.of_pdf_point flt
          | "cm"   -> Length.of_centimeter flt
          | "mm"   -> Length.of_millimeter flt
          | "inch" -> Length.of_inch flt
          | _      -> raise (UnknownUnitOfLength(rng, unitnm))
        in
        (base (BCLength(len)), (rng, BaseType(LengthType)))

  | UTInputHorz(utihlst) ->
      let ihlst = typecheck_input_horz rng pre tyenv utihlst in
      (InputHorz(ihlst), (rng, BaseType(TextRowType)))

  | UTInputVert(utivlst) ->
      let ivlst = typecheck_input_vert rng pre tyenv utivlst in
      (InputVert(ivlst), (rng, BaseType(TextColType)))

  | UTPath(utastpt0, utpathcomplst, utcycleopt) ->
      let (ept0, typt0) = typecheck_iter tyenv utastpt0 in
      unify typt0 (Range.dummy "ut-path", point_type_main);
      let (pathcomplst, cycleopt) = typecheck_path pre tyenv utpathcomplst utcycleopt in
      (Path(ept0, pathcomplst, cycleopt), (rng, BaseType(PathType)))

  | UTFinishStruct ->
      final_tyenv := tyenv;  (* ad hoc *)
      (FinishStruct, (Range.dummy "finish-struct", BaseType(EnvType)))

  | UTFinishHeaderFile ->
      final_tyenv := tyenv;  (* ad hoc *)
      (FinishHeaderFile, (Range.dummy "finish-header-file", BaseType(EnvType)))

  | UTOpenIn(rngtok, mdlnm, utast1) ->
      let tyenvnew = Typeenv.open_module tyenv rngtok mdlnm in
      typecheck_iter tyenvnew utast1

  | UTContentOf(mdlnmlst, varnm) ->
      begin
        match Typeenv.find tyenv mdlnmlst varnm rng with
        | None ->
            let cands = Typeenv.find_candidates tyenv mdlnmlst varnm rng in
            raise (UndefinedVariable(rng, mdlnmlst, varnm, cands))

        | Some((pty, evid, stage)) ->
            begin
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0)
              | (Stage0, Stage0)
              | (Stage1, Persistent0)
              | (Stage1, Stage1) ->
                  let tyfree = instantiate pre.level pre.quantifiability pty in
                  let tyres = overwrite_range_of_type tyfree rng in
(*
                  let () = print_endline ("\n#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty) ^ " = " ^ (string_of_mono_type_basic tyres) ^ "\n  (" ^ (Range.to_string rng) ^ ")") in (* for debug *)
*)
                  (ContentOf(rng, evid), tyres)

              | _ ->
                  raise (InvalidOccurrenceAsToStaging(rng, varnm, stage))
            end
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        match Typeenv.find_constructor pre tyenv constrnm with
        | None ->
            let cands = Typeenv.find_constructor_candidates pre tyenv constrnm in
            raise (UndefinedConstructor(rng, constrnm, cands))

        | Some((tyarglist, tyid, tyc)) ->
(*
            let () = print_endline ("\n#Constructor " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc) ^ " in ... " ^ (string_of_mono_type_basic (rng, VariantType([], tyid))) ^ "(" ^ (Typeenv.find_type_name tyenv tyid) ^ ")") in (* for debug *)
*)
            let (e1, ty1) = typecheck_iter tyenv utast1 in
            unify ty1 tyc;
            let tyres = (rng, VariantType(tyarglist, tyid)) in
            (NonValueConstructor(constrnm, e1), tyres)
      end

  | UTHorzConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 (get_range utast1, BaseType(BoxRowType));
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      unify ty2 (get_range utast2, BaseType(BoxRowType));
      (HorzConcat(e1, e2), (rng, BaseType(BoxRowType)))

  | UTVertConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 (get_range utast1, BaseType(BoxColType));
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      unify ty2 (get_range utast2, BaseType(BoxColType));
      (VertConcat(e1, e2), (rng, BaseType(BoxColType)))

  | UTConcat(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 (get_range utast1, BaseType(TextRowType));
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      unify ty2 (get_range utast2, BaseType(TextRowType));
      (Concat(e1, e2), (rng, BaseType(TextRowType)))

  | UTLambdaHorz(varrng, varnmctx, utast1) ->
      let (bstyvar, bstyret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BoxRowType)
      in
      let evid = EvalVarID.fresh (varrng, varnmctx) in
      let tyenvsub = Typeenv.add tyenv varnmctx (Poly(varrng, BaseType(bstyvar)), evid, pre.stage) in
      let (e1, ty1) = typecheck_iter tyenvsub utast1 in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      unify tyret (Range.dummy "lambda-horz-return", BaseType(bstyret));
        (abstraction evid e1, (rng, HorzCommandType(cmdargtylist)))

  | UTLambdaVert(varrng, varnmctx, utast1) ->
      let (bstyvar, bstyret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BoxColType)
      in
      let evid = EvalVarID.fresh (varrng, varnmctx) in
      let tyenvsub = Typeenv.add tyenv varnmctx (Poly(varrng, BaseType(bstyvar)), evid, pre.stage) in
      let (e1, ty1) = typecheck_iter tyenvsub utast1 in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      unify tyret (Range.dummy "lambda-vert-return", BaseType(bstyret));
      (abstraction evid e1, (rng, VertCommandType(cmdargtylist)))

  | UTLambdaMath(utastF) ->
      let (eF, tyF) = typecheck_iter tyenv utastF in
      let (cmdargtylist, tyret) = flatten_type tyF in
      unify tyret (Range.dummy "lambda-math-return", BaseType(MathType));
      (eF, (rng, MathCommandType(cmdargtylist)))

  | UTApply(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let eret = Apply(e1, e2) in
      begin
        match unlink ty1 with
        | (_, FuncType(_, tydom, tycod)) ->
            unify tydom ty2;
            let tycodnew = overwrite_range_of_type tycod rng in
            (eret, tycodnew)

        | (_, TypeVariable(_)) as ty1 ->
            let beta = fresh_type_variable rng pre UniversalKind in
            let orv = OptionRowVarID.fresh pre.level in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            unify ty1 (get_range utast1, FuncType(optrow, ty2, beta));
            (eret, beta)

        | ty1 ->
            let (rng1, _) = utast1 in
            raise (ApplicationOfNonFunction(rng1, tyenv, ty1))
      end

  | UTApplyOptional(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let eret = ApplyOptional(e1, e2) in
      begin
        match ty1 with
        | (_, FuncType(OptionRowCons(tyopt, optrow), tydom, tycod)) ->
            unify tyopt ty2;
            let tynew = (rng, FuncType(optrow, tydom, tycod)) in
              (eret, tynew)

        | _ ->
            let beta1 = fresh_type_variable (Range.dummy "UTApplyOptional:dom") pre UniversalKind in
            let beta2 = fresh_type_variable (Range.dummy "UTApplyOptional:cod") pre UniversalKind in
            let orv = OptionRowVarID.fresh pre.level in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            unify ty1 (get_range utast1, FuncType(OptionRowCons(ty2, optrow), beta1, beta2));
            (eret, (rng, FuncType(optrow, beta1, beta2)))
      end

  | UTApplyOmission(utast1) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let eret = ApplyOmission(e1) in
      begin
        match ty1 with
        | (_, FuncType(OptionRowCons(_, optrow), tydom, tycod)) ->
            (eret, (rng, FuncType(optrow, tydom, tycod)))

        | _ ->
            let beta0 = fresh_type_variable rng pre UniversalKind in
            let beta1 = fresh_type_variable rng pre UniversalKind in
            let beta2 = fresh_type_variable rng pre UniversalKind in
            let orv = OptionRowVarID.fresh pre.level in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            unify ty1 (get_range utast1, FuncType(OptionRowCons(beta0, optrow), beta1, beta2));
            (eret, (rng, FuncType(optrow, beta1, beta2)))
      end

  | UTFunction(optargs, pat, utast1) ->
      let utpatbr = UTPatternBranch(pat, utast1) in
      let (optrow, evids, tyenvnew) = add_optionals_to_type_environment tyenv pre optargs in
      let (patbr, typat, ty1) = typecheck_pattern_branch pre tyenvnew utpatbr in
      (Function(evids, patbr), (rng, FuncType(optrow, typat, ty1)))

  | UTPatternMatch(utastO, utpatbrs) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let beta = fresh_type_variable (Range.dummy "ut-pattern-match") pre UniversalKind in
      let patbrs = typecheck_pattern_branch_list pre tyenv utpatbrs tyO beta in
      Exhchecker.main rng patbrs tyO pre tyenv;
      (PatternMatch(rng, eO, patbrs), beta)

  | UTLetNonRecIn(mntyopt, utpat, utast1, utast2) ->
      let presub = { pre with level = Level.succ pre.level; } in
      let (pat, tyP, patvarmap) = typecheck_pattern presub tyenv utpat in
      let (e1, ty1) = typecheck presub tyenv utast1 in
      unify ty1 tyP;
      let tyenvnew =
        if is_nonexpansive_expression e1 then
        (* -- if 'e1' is polymorphically typeable -- *)
          add_pattern_var_poly pre tyenv patvarmap
        else
        (* -- 'e1' should be typed monomorphically -- *)
          add_pattern_var_mono pre tyenv patvarmap
      in
      let (e2, ty2) = typecheck_iter tyenvnew utast2 in
      (LetNonRecIn(pat, e1, e2), ty2)

  | UTLetRecIn(utrecbinds, utast2) ->
      let (tyenvnew, _, recbinds) = make_type_environment_by_letrec pre tyenv utrecbinds in
      let (e2, ty2) = typecheck_iter tyenvnew utast2 in
      (LetRecIn(recbinds, e2), ty2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      unify tyB (Range.dummy "if-bool", BaseType(BoolType));
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      unify ty2 ty1;
      (IfThenElse(eB, e1, e2), ty1)

(* ---- imperatives ---- *)

  | UTLetMutableIn(varrng, varnm, utastI, utastA) ->
      let (tyenvI, evid, eI, tyI) = make_type_environment_by_let_mutable pre tyenv varrng varnm utastI in
      let (eA, tyA) = typecheck_iter tyenvI utastA in
      (LetMutableIn(evid, eI, eA), tyA)

  | UTOverwrite(varrng, varnm, utastN) ->
      begin
        match typecheck_iter tyenv (varrng, UTContentOf([], varnm)) with
        | (ContentOf(_, evid), tyvar) ->
            let (eN, tyN) = typecheck_iter tyenv utastN in
            unify tyvar (get_range utastN, RefType(tyN));
              (* --
                 actually 'get_range utastnew' is not good
                 since the right side expression has type 't, not 't ref
              -- *)
            (Overwrite(evid, eN), (rng, BaseType(UnitType)))

        | _ ->
            assert false
      end

  | UTSequential(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 (get_range utast1, BaseType(UnitType));
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      (Sequential(e1, e2), ty2)

  | UTWhileDo(utastB, utastC) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      unify tyB (get_range utastB, BaseType(BoolType));
      let (eC, tyC) = typecheck_iter tyenv utastC in
      unify tyC (get_range utastC, BaseType(UnitType));
      (WhileDo(eB, eC), (rng, BaseType(UnitType)))

(* ---- lightweight itemize ---- *)

  | UTItemize(utitmz) ->
      let eitmz = typecheck_itemize pre tyenv utitmz in
      let ty = overwrite_range_of_type (Primitives.itemize_type ()) rng in
      (eitmz, ty)

(* ---- list ---- *)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      unify tyT (Range.dummy "list-cons", ListType(tyH));
      let tyres = (rng, ListType(tyH)) in
      (PrimitiveListCons(eH, eT), tyres)

  | UTEndOfList ->
      let beta = fresh_type_variable rng pre UniversalKind in
      (ASTEndOfList, (rng, ListType(beta)))

(* ---- tuple ---- *)

  | UTTuple(utastlst) ->
      let etylst = List.map (typecheck_iter tyenv) utastlst in
      let elst = List.map fst etylst in
      let tylst = List.map snd etylst in
      let tyres = (rng, ProductType(tylst)) in
      (PrimitiveTuple(elst), tyres)

(* ---- records ---- *)

  | UTRecord(flutlst) ->
      typecheck_record pre tyenv flutlst rng

  | UTAccessField(utast1, fldnm) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let betaF = fresh_type_variable rng pre UniversalKind in
      let beta1 = fresh_type_variable (get_range utast1) pre (RecordKind(Assoc.of_list [(fldnm, betaF)])) in
      unify beta1 ty1;
      (AccessField(e1, fldnm), betaF)

  | UTUpdateField(utast1, fldnm, utastF) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (eF, tyF) = typecheck_iter tyenv utastF in
      let beta1 = fresh_type_variable (get_range utast1) pre (RecordKind(Assoc.of_list [(fldnm, tyF)])) in
      unify beta1 ty1;
      (UpdateField(e1, fldnm, eF), ty1)

(* -- math -- *)

  | UTMath(utmath) ->
      let tymath = (rng, BaseType(MathType)) in
      let utast = typecheck_math pre tyenv utmath in
      (utast, tymath)

(* ---- other fundamentals ---- *)

  | UTDeclareVariantIn(mutvarntcons, utastA) ->
      let tyenvnew = Typeenv.add_mutual_cons tyenv pre.level mutvarntcons in
      typecheck_iter tyenvnew utastA

  | UTModule(mdlrng, mdlnm, sigopt, utastM, utastA) ->
      let tyenvinner = Typeenv.enter_new_module tyenv mdlnm in
      let (eM, _) = typecheck_iter tyenvinner utastM in
      let tyenvmid = Typeenv.sigcheck mdlrng pre (!final_tyenv) tyenv sigopt in
      let tyenvouter = Typeenv.leave_module tyenvmid in
      let (eA, tyA) = typecheck_iter tyenvouter utastA in
      (Module(eM, eA), tyA)

(* ---- for lightweight command definition ---- *)
  | UTLexHorz(utastctx, utasth) ->
      let (ectx, tyctx) = typecheck_iter tyenv utastctx in
      let (eh, tyh) = typecheck_iter tyenv utasth in
      let (eret, bstyctx, bstyret) =
        if OptionState.is_text_mode () then
          (TextHorzLex(ectx, eh), TextInfoType, StringType)
        else
          (HorzLex(ectx, eh), ContextType, BoxRowType)
      in
      unify tyctx (Range.dummy "ut-lex-horz-1", BaseType(bstyctx));
      unify tyh (Range.dummy "ut-lex-horz-2", BaseType(TextRowType));
      (eret, (rng, BaseType(bstyret)))

  | UTLexVert(utastctx, utastv) ->
      let (ectx, tyctx) = typecheck_iter tyenv utastctx in
      let (ev, tyv) = typecheck_iter tyenv utastv in
      let (eret, bstyctx, bstyret) =
        if OptionState.is_text_mode () then
          (TextVertLex(ectx, ev), TextInfoType, StringType)
        else
          (VertLex(ectx, ev), ContextType, BoxColType)
      in
      unify tyctx (Range.dummy "ut-lex-vert-1", BaseType(bstyctx));
      unify tyv (Range.dummy "ut-lex-vert-2", BaseType(TextColType));
      (eret, (rng, BaseType(bstyret)))

(* -- staged constructs -- *)

  | UTNext(utast1) ->
      begin
        match pre.stage with
        | Stage0 ->
            let (e1, ty1) = typecheck_iter ~s:Stage1 tyenv utast1 in
            (Next(e1), (rng, CodeType(ty1)))

        | Stage1 | Persistent0 ->
            raise (InvalidExpressionAsToStaging(rng, Stage0))
      end

  | UTPrev(utast1) ->
      begin
        match pre.stage with
        | Stage0 | Persistent0 ->
            raise (InvalidExpressionAsToStaging(rng, Stage1))

        | Stage1 ->
            let (e1, ty1) = typecheck_iter ~s:Stage0 tyenv utast1 in
            let beta = fresh_type_variable rng pre UniversalKind in
            unify ty1 (Range.dummy "prev", CodeType(beta));
            (Prev(e1), beta)
      end

(* -- macros -- *)

  | UTLetHorzMacroIn(rngcs, csnm, macparams, utast1, utast2) ->
      begin
        match pre.stage with
        | Stage0 | Persistent0 ->
            raise (InvalidExpressionAsToStaging(rng, Stage1))

        | Stage1 ->
            let (tyenv, argevids, macparamtys) = add_macro_parameters_to_type_environment tyenv pre macparams in
            let (e1, ty1) = typecheck_iter ~s:Stage1 tyenv utast1 in
            unify ty1 (Range.dummy "let-inline-macro", BaseType(TextRowType));
            let evid = EvalVarID.fresh (rngcs, csnm) in
            let (e2, ty2) = typecheck_iter (Typeenv.add_macro tyenv csnm (MacroType(macparamtys), evid)) utast2 in
            let e = Prev(LetNonRecIn(PVariable(evid), abstraction_list argevids (Next(e1)), Next(e2))) in
            (e, ty2)
      end


and typecheck_command_arguments (ecmd : abstract_tree) (tycmd : mono_type) (rngcmdapp : Range.t) (pre : pre) tyenv (utcmdarglst : untyped_command_argument list) (cmdargtylst : mono_command_argument_type list) : abstract_tree =
  let rec aux eacc utcmdarglst cmdargtylst =
    match (utcmdarglst, cmdargtylst) with
    | ([], _) ->
        cmdargtylst |> List.iter (function
        | MandatoryArgumentType(ty) -> raise (NeedsMoreArgument(rngcmdapp, tyenv, tycmd, ty))
        | OptionalArgumentType(_)   -> ()
        );
        eacc

    | (_ :: _, []) ->
        raise (TooManyArgument(rngcmdapp, tyenv, tycmd))

    | (UTMandatoryArgument(_) :: _, OptionalArgumentType(_) :: cmdargtytail) ->
          aux eacc utcmdarglst cmdargtytail

    | (UTMandatoryArgument(utastA) :: utcmdargtail, MandatoryArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck pre tyenv utastA in
        unify_ tyenv tyA tyreq;
        aux (Apply(eacc, eA)) utcmdargtail cmdargtytail

    | (UTOptionalArgument(utastA) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck pre tyenv utastA in
        unify_ tyenv tyA tyreq;
        aux (ApplyOptional(eacc, eA)) utcmdargtail cmdargtytail

    | (UTOptionalArgument((rngA, _)) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tyenv, tycmd, rngA))

    | (UTOmission(_) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        aux (ApplyOmission(eacc)) utcmdargtail cmdargtytail

    | (UTOmission(rngA) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tyenv, tycmd, rngA))
  in
  aux ecmd utcmdarglst cmdargtylst


and typecheck_math (pre : pre) tyenv ((rng, utmathmain) : untyped_math) : abstract_tree =
  let iter = typecheck_math pre tyenv in
  let open HorzBox in
    match utmathmain with
    | UTMChar(s) ->
        ASTMath([MathPure(MathVariantChar(s))])

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
        let (ecmd, tycmd) = typecheck pre tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with
          | MathCommandType(cmdargtylstreq) ->
              let eapp = typecheck_command_arguments ecmd tycmd rng pre tyenv utcmdarglst cmdargtylstreq in
              eapp

          | HorzCommandType(_) ->
              let (rngcmd, _) = utastcmd in
              raise (HorzCommandInMath(rngcmd))

          | _ ->
              assert false
        end

    | UTMEmbed(utast0) ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify_ tyenv ty0 (Range.dummy "math-embedded-var", BaseType(MathType));
          e0


and typecheck_path pre tyenv (utpathcomplst : (untyped_abstract_tree untyped_path_component) list) (utcycleopt : (unit untyped_path_component) option) =

  let typecheck_anchor_point utastpt =
    let (ept, typt) = typecheck pre tyenv utastpt in
    unify_ tyenv typt (Range.dummy "typecheck-path", point_type_main);
    ept
  in

  let pathcomplst =
    utpathcomplst |> List.fold_left (fun acc utpathcomp ->
      match utpathcomp with
      | UTPathLineTo(utastpt) ->
          let (ept, typt) = typecheck pre tyenv utastpt in
          unify_ tyenv typt (Range.dummy "typecheck-path-L", point_type_main);
          Alist.extend acc (PathLineTo(ept))

      | UTPathCubicBezierTo(utastpt1, utastpt2, utastpt) ->
          let ept1 = typecheck_anchor_point utastpt1 in
          let ept2 = typecheck_anchor_point utastpt2 in
          let ept = typecheck_anchor_point utastpt in
          Alist.extend acc (PathCubicBezierTo(ept1, ept2, ept))
    ) Alist.empty |> Alist.to_list
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
  (pathcomplst, cycleopt)


and typecheck_input_vert (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (utivlst : untyped_input_vert_element list) : input_vert_element list =
  let rec aux acc utivlst =
    match utivlst with
    | [] ->
        Alist.to_list acc

    | (_, UTInputVertEmbedded((rngcmd, _) as utastcmd, utcmdarglst)) :: tail ->
        let (ecmd, tycmd) = typecheck pre tyenv utastcmd in
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
              let evid = EvalVarID.fresh (Range.dummy "ctx-vert", "%ctx-vert") in
              let ecmdctx = Apply(ecmd, ContentOf(Range.dummy "ctx-vert", evid)) in
              let eapp = typecheck_command_arguments ecmdctx tycmd rngcmdapp pre tyenv utcmdarglst cmdargtylstreq in
              let eabs = abstraction evid eapp in
              aux (Alist.extend acc (InputVertEmbedded(eabs))) tail

          | _ ->
              assert false
        end

    | (_, UTInputVertContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify_ tyenv ty0 (Range.dummy "UTInputVertContent", BaseType(TextColType));
        aux (Alist.extend acc (InputVertContent(e0))) tail
  in
  aux Alist.empty utivlst



and typecheck_input_horz (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (utihlst : untyped_input_horz_element list) : input_horz_element list =
  let rec aux acc utihlst =
    match utihlst with
    | [] ->
        Alist.to_list acc

    | (_, UTInputHorzEmbedded((rngcmd, _) as utastcmd, utcmdarglst)) :: tail ->
        let rngcmdapp =
          match List.rev utcmdarglst with
          | []                                 -> rngcmd
          | UTMandatoryArgument((rng, _)) :: _ -> Range.unite rngcmd rng
          | UTOptionalArgument((rng, _)) :: _  -> Range.unite rngcmd rng
          | UTOmission(rng) :: _               -> Range.unite rngcmd rng
        in
        let (ecmd, tycmd) = typecheck pre tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with

          | HorzCommandType(cmdargtylstreq) ->
              let evid = EvalVarID.fresh (Range.dummy "ctx-horz", "%ctx-horz") in
              let ecmdctx = Apply(ecmd, ContentOf(Range.dummy "ctx-horz", evid)) in
              let eapp = typecheck_command_arguments ecmdctx tycmd rngcmdapp pre tyenv utcmdarglst cmdargtylstreq in
              let eabs = abstraction evid eapp in
              aux (Alist.extend acc (InputHorzEmbedded(eabs))) tail

          | MathCommandType(_) ->
              let (rngcmd, _) = utastcmd in
              raise (MathCommandInHorz(rngcmd))

          | _ ->
              assert false
        end

    | (_, UTInputHorzEmbeddedMath(utastmath)) :: tail ->
        let (emath, tymath) = typecheck pre tyenv utastmath in
        unify_ tyenv tymath (Range.dummy "ut-input-horz-embedded-math", BaseType(MathType));
        aux (Alist.extend acc (InputHorzEmbeddedMath(emath))) tail

    | (_, UTInputHorzContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify_ tyenv ty0 (Range.dummy "ut-input-horz-content", BaseType(TextRowType));
        aux (Alist.extend acc (InputHorzContent(e0))) tail

    | (_, UTInputHorzText(s)) :: tail ->
        aux (Alist.extend acc (InputHorzText(s))) tail

    | (rngapp, UTInputHorzMacro(hmacro, utmacargs)) :: tail ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              raise (InvalidExpressionAsToStaging(rngapp, Stage1))

          | Stage1 ->
              let (rngcs, csnm) = hmacro in
              begin
                match Typeenv.find_macro tyenv csnm with
                | None ->
                    raise (UndefinedHorzMacro(rngcs, csnm))

                | Some((MacroType(macparamtys), evid)) ->
                    let eargs = typecheck_macro_arguments rngapp pre tyenv macparamtys utmacargs in
                    let eapp = apply_tree_of_list (ContentOf(rngcs, evid)) eargs in
                    let ih = InputHorzContent(Prev(eapp)) in
                    aux (Alist.extend acc ih) tail
              end
        end
  in
  aux Alist.empty utihlst


and typecheck_macro_arguments (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (macparamtys : macro_parameter_type list) (utmacargs : untyped_macro_argument list) : abstract_tree list =
  let lenexp = List.length macparamtys in
  let lenact = List.length utmacargs in
  if (lenexp <> lenact) then
    raise (InvalidNumberOfMacroArguments(rng, macparamtys))
  else
    let argacc =
      List.fold_left2 (fun argacc macparamty utmacarg ->
        match (macparamty, utmacarg) with
        | (LateMacroParameter(tyexp), UTLateMacroArg(utast)) ->
            let (earg, tyarg) = typecheck pre tyenv utast in
            unify_ tyenv tyarg tyexp;
            Alist.extend argacc (Next(earg))
              (* -- late arguments are converted to quoted arguments -- *)

        | (EarlyMacroParameter(tyexp), UTEarlyMacroArg(utast)) ->
            let (earg, tyarg) = typecheck pre tyenv utast in
            unify_ tyenv tyarg tyexp;
            Alist.extend argacc earg

        | (LateMacroParameter(tyexp), UTEarlyMacroArg((rngarg, _))) ->
            raise (LateMacroArgumentExpected(rngarg, tyexp))

        | (EarlyMacroParameter(tyexp), UTLateMacroArg((rngarg, _))) ->
            raise (EarlyMacroArgumentExpected(rngarg, tyexp))

      ) Alist.empty macparamtys utmacargs
    in
    Alist.to_list argacc


and typecheck_record
    (pre : pre) (tyenv : Typeenv.t)
    (flutlst : (field_name * untyped_abstract_tree) list) (rng : Range.t)
=
  let (easc, tyasc) =
    flutlst |> List.fold_left (fun (easc, tyasc) (fldnmX, utastX) ->
      if Assoc.mem fldnmX easc then
        raise (MultipleFieldInRecord(rng, fldnmX))
      else
        let (eX, tyX) = typecheck pre tyenv utastX in
        (Assoc.add easc fldnmX eX, Assoc.add tyasc fldnmX tyX)
    ) (Assoc.empty, Assoc.empty)
  in
  (Record(easc), (rng, RecordType(tyasc)))


and typecheck_itemize (pre : pre) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) =
  let (e1, ty1) = typecheck pre tyenv utast1 in
  unify_ tyenv ty1 (Range.dummy "typecheck_itemize_string", BaseType(TextRowType));
  let e2 = typecheck_itemize_list pre tyenv utitmzlst in
  (NonValueConstructor("Item", PrimitiveTuple([e1; e2])))


and typecheck_itemize_list
    (pre : pre) (tyenv : Typeenv.t) (utitmzlst : untyped_itemize list) =
  match utitmzlst with
  | [] ->
      ASTEndOfList

  | hditmz :: tlitmzlst ->
      let ehd = typecheck_itemize pre tyenv hditmz in
      let etl = typecheck_itemize_list pre tyenv tlitmzlst in
      PrimitiveListCons(ehd, etl)


and typecheck_pattern_branch (pre : pre) (tyenv : Typeenv.t) (utpatbr : untyped_pattern_branch) : pattern_branch * mono_type * mono_type =
  match utpatbr with
    | UTPatternBranch(utpat, utast1) ->
        let (epat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
        let tyenvpat = add_pattern_var_mono pre tyenv patvarmap in
        let (e1, ty1) = typecheck pre tyenvpat utast1 in
        (PatternBranch(epat, e1), typat, ty1)

    | UTPatternBranchWhen(utpat, utastB, utast1) ->
        let (epat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
        let tyenvpat = add_pattern_var_mono pre tyenv patvarmap in
        let (eB, tyB) = typecheck pre tyenvpat utastB in
        unify_ tyenvpat tyB (Range.dummy "pattern-match-cons-when", BaseType(BoolType));
        let (e1, ty1) = typecheck pre tyenvpat utast1 in
        (PatternBranchWhen(epat, eB, e1), typat, ty1)


and typecheck_pattern_branch_list
    (pre : pre) (tyenv : Typeenv.t) (utpatbrs : untyped_pattern_branch list) (tyobj : mono_type) (tyres : mono_type) : pattern_branch list =

  let unify = unify_ tyenv in

  let rec iter (patbracc : pattern_branch Alist.t) (utpatbrs : untyped_pattern_branch list) =
    match utpatbrs with
    | [] ->
        Alist.to_list patbracc

    | utpatbr :: tail ->
        let (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
        unify typat tyobj;
        unify ty1 tyres;
        iter (Alist.extend patbracc patbr) tail

  in
  iter Alist.empty utpatbrs


and typecheck_pattern
    (pre : pre) (tyenv : Typeenv.t)
    ((rng, utpatmain) : untyped_pattern_tree) : pattern_tree * mono_type * pattern_var_map =
  let iter = typecheck_pattern pre tyenv in
  let unify = unify_ tyenv in
    match utpatmain with
    | UTPIntegerConstant(nc) -> (PIntegerConstant(nc), (rng, BaseType(IntType)), PatternVarMap.empty)
    | UTPBooleanConstant(bc) -> (PBooleanConstant(bc), (rng, BaseType(BoolType)), PatternVarMap.empty)
    | UTPUnitConstant        -> (PUnitConstant, (rng, BaseType(UnitType)), PatternVarMap.empty)

    | UTPStringConstant(sc) ->
        (PStringConstant(sc), (rng, BaseType(StringType)), PatternVarMap.empty)

    | UTPListCons(utpat1, utpat2) ->
        let (epat1, typat1, patvarmap1) = iter utpat1 in
        let (epat2, typat2, patvarmap2) = iter utpat2 in
        unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1));
        let patvarmap = unite_pattern_var_map patvarmap1 patvarmap2 in
        (PListCons(epat1, epat2), typat2, patvarmap)

    | UTPEndOfList ->
        let beta = fresh_type_variable rng pre UniversalKind in
        (PEndOfList, (rng, ListType(beta)), PatternVarMap.empty)

    | UTPTuple(utpatlst) ->
        let trilst = List.map iter utpatlst in
        let epatlst = trilst |> List.map (fun (epat, _, _) -> epat) in
        let typatlst = trilst |> List.map (fun (_, typat, _) -> typat) in
        let patvarmaplst = trilst |> List.map (fun (_, _, patvarmap) -> patvarmap) in
        let tyres = (rng, ProductType(typatlst)) in
        let patvarmap = List.fold_left unite_pattern_var_map PatternVarMap.empty patvarmaplst in
        (PTuple(epatlst), tyres, patvarmap)

    | UTPWildCard ->
        let beta = fresh_type_variable rng pre UniversalKind in
        (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let beta = fresh_type_variable rng pre UniversalKind in
        let evid = EvalVarID.fresh (rng, varnm) in
(*
        let () = print_endline ("\n#PAdd " ^ varnm ^ " : " ^ (string_of_mono_type_basic beta)) in  (* for debug *)
*)
        (PVariable(evid), beta, PatternVarMap.empty |> PatternVarMap.add varnm (rng, evid, beta))

    | UTPAsVariable(varnm, utpat1) ->
        let beta = fresh_type_variable rng pre UniversalKind in
        let (epat1, typat1, patvarmap1) = iter utpat1 in
        begin
          match PatternVarMap.find_opt varnm patvarmap1 with
          | Some((rngsub, _, _)) ->
            (* -- if 'varnm' also occurs in 'utpat1' -- *)
              raise (MultiplePatternVariable(rngsub, rng, varnm))

          | None ->
              let evid = EvalVarID.fresh (rng, varnm) in
              (PAsVariable(evid, epat1), typat1, patvarmap1 |> PatternVarMap.add varnm (rng, evid, beta))
        end

    | UTPConstructor(constrnm, utpat1) ->
        begin
          match Typeenv.find_constructor pre tyenv constrnm with
          | None ->
              let cands = Typeenv.find_constructor_candidates pre tyenv constrnm in
              raise (UndefinedConstructor(rng, constrnm, cands))

          | Some((tyarglist, tyid, tyc)) ->
(*
              let () = print_endline ("P-find " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc)) in (* for debug *)
*)
              let (epat1, typat1, tyenv1) = iter utpat1 in
              unify tyc typat1;
              (PConstructor(constrnm, epat1), (rng, VariantType(tyarglist, tyid)), tyenv1)
        end


and make_type_environment_by_letrec (pre : pre) (tyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) =

  let rec add_mutual_variables (acctyenv : Typeenv.t) (tvtyacc : (untyped_letrec_binding * mono_type * EvalVarID.t) Alist.t) (utrecbinds : untyped_letrec_binding list) : (Typeenv.t * (untyped_letrec_binding * mono_type * EvalVarID.t) list) =
    let iter = add_mutual_variables in
      match utrecbinds with
      | [] ->
          (acctyenv, Alist.to_list tvtyacc)

      | (UTLetRecBinding(_, varrng, varnm, astdef) as utrecbind) :: tailcons ->
          let tvid = FreeID.fresh UniversalKind pre.quantifiability (Level.succ pre.level) () in
          let tvref = ref (MonoFree(tvid)) in
          let rng = get_range astdef in
          let beta = (rng, TypeVariable(tvref)) in
          let pbeta = (rng, TypeVariable(PolyFree(tvref))) in
(*
          let () = print_endline ("#AddMutualVar " ^ varnm ^ " : '" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ " :: U") in (* for debug *)
*)
          let evid = EvalVarID.fresh (varrng, varnm) in
          iter (Typeenv.add acctyenv varnm (Poly(pbeta), evid, pre.stage)) (Alist.extend tvtyacc (utrecbind, beta, evid)) tailcons
  in

  let rec typecheck_mutual_contents (pre : pre) (tyenvforrec : Typeenv.t) (utreclst : (untyped_letrec_binding * mono_type * EvalVarID.t) list) (recbindacc : letrec_binding Alist.t) (acctvtylstout : (var_name * mono_type * EvalVarID.t) Alist.t) =
    let iter = typecheck_mutual_contents pre tyenvforrec in
    let unify = unify_ tyenv in  (* doubtful *)
    match utreclst with
    | [] ->
        (tyenvforrec, Alist.to_list recbindacc, Alist.to_list acctvtylstout)

    | (UTLetRecBinding(mntyopt, _, varnm, utast1), beta, evid) :: utrectail ->
        let (e1, ty1) = typecheck { pre with level = Level.succ pre.level; } tyenvforrec utast1 in
        begin
          match e1 with
          | Function([], patbr1) ->
              begin
                match mntyopt with
                | None ->
                    unify ty1 beta;
                    iter utrectail (Alist.extend recbindacc (LetRecBinding(evid, patbr1))) (Alist.extend acctvtylstout (varnm, beta, evid))

                | Some(mnty) ->
                    let tyin = Typeenv.fix_manual_type_free pre tyenv mnty [] in
                    unify ty1 beta;
                    unify tyin beta;
                    iter utrectail (Alist.extend recbindacc (LetRecBinding(evid, patbr1))) (Alist.extend acctvtylstout (varnm, beta, evid))
              end

          | _ ->
              let (rng1, _) = utast1 in
              raise (BreaksValueRestriction(rng1))
        end
  in

  let rec make_forall_type_mutual (tyenv : Typeenv.t) (tyenv_before_let : Typeenv.t) tvtylst tvtylst_forall =
    match tvtylst with
    | [] ->
        (tyenv, tvtylst_forall)

    | (varnm, tvty, evid) :: tvtytail ->
(*
        let () = print_endline ("#Generalize1 " ^ varnm ^ " : " ^ (string_of_mono_type_basic tvty)) in  (* for debug *)
*)
        let pty = (generalize pre.level (erase_range_of_type tvty)) in
(*
        let () = print_endline ("#Generalize2 " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty)) in (* for debug *)
*)
        let tvtylst_forall_new = (varnm, pty, evid) :: tvtylst_forall in
        make_forall_type_mutual (Typeenv.add tyenv varnm (pty, evid, pre.stage)) tyenv_before_let tvtytail tvtylst_forall_new
  in

  let (tyenvforrec, utreclst) = add_mutual_variables tyenv Alist.empty utrecbinds in
  let (tyenv_new, mutletcons, tvtylstout) =
    typecheck_mutual_contents pre tyenvforrec utreclst Alist.empty Alist.empty
  in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual tyenv_new tyenv tvtylstout [] in
  (tyenv_forall, tvtylst_forall, mutletcons)


and make_type_environment_by_let_mutable (pre : pre) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
(*
  let () = print_endline ("#AddMutable " ^ varnm ^ " : " ^ (string_of_mono_type_basic (varrng, RefType(tyI)))) in (* for debug *)
*)
  let evid = EvalVarID.fresh (varrng, varnm) in
  let tyenvI = Typeenv.add tyenv varnm (lift_poly (varrng, RefType(tyI)), evid, pre.stage) in
  (tyenvI, evid, eI, tyI)


let main (stage : stage) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) =
  begin
    final_tyenv := tyenv;
    let (e, ty) = typecheck { stage = stage; quantifiability = Quantifiable; level = Level.bottom; } tyenv utast in
    (ty, !final_tyenv, e)
  end
