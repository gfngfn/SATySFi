
module Types = Types_
module Primitives = Primitives_
open MyUtil
open Types
open Display

exception UndefinedVariable     of Range.t * module_name list * var_name * var_name list
exception UndefinedConstructor  of Range.t * var_name * var_name list
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


let add_optionals_to_type_environment (tyenv : Typeenv.t) qtfbl lev (optargs : (Range.t * var_name) list) : mono_option_row * EvalVarID.t list * Typeenv.t =
  let (tyenvnew, tyacc, evidacc) =
    optargs |> List.fold_left (fun (tyenv, tyacc, evidacc) (rng, varnm) ->
      let evid = EvalVarID.fresh varnm in
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let tvref = ref (MonoFree(tvid)) in
      let beta = (rng, TypeVariable(PolyFree(tvref))) in
      let tyenvnew = Typeenv.add tyenv varnm (Poly(Primitives.option_type beta), evid) in
        (tyenvnew, Alist.extend tyacc (rng, TypeVariable(tvref)), Alist.extend evidacc evid)
    ) (tyenv, Alist.empty, Alist.empty)
  in
  let optrow =
    List.fold_right (fun ty acc ->
      OptionRowCons(ty, acc)
    ) (Alist.to_list tyacc) OptionRowEmpty
  in
    (optrow, Alist.to_list evidacc, tyenvnew)


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
    let pty = lift_poly (erase_range_of_type ty) in
      Typeenv.add tyenvacc varnm (pty, evid)
  ) patvarmap tyenv


let add_pattern_var_poly lev (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = (generalize lev (erase_range_of_type ty)) in
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
          | MonoLink(tyl)   -> iter tyl
          | MonoFree(tvidx) ->
              if FreeID.equal tvidx tvid then
                true
              else
                let levx = FreeID.get_level tvidx in
                let () =
                  (* -- update level -- *)
                  if Level.less_than lev levx then
                    tvref := MonoFree(FreeID.set_level tvidx lev)
                  else
                    ()
                in
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
              let () =
                (* -- update level -- *)
                if Level.less_than lev levx then
                  orviref := MonoORFree(OptionRowVarID.set_level orvx lev)
                else
                  ()
              in
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
              let () =
                (* -- update level -- *)
                if Level.less_than lev levx then
                  tvref := MonoFree(FreeID.set_level tvidx lev)
                else
                  ()
              in
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
                let () =
                  (* -- update level -- *)
                  if Level.less_than lev levx then
                    orviref := MonoORFree(OptionRowVarID.set_level orvx lev)
                  else
                    ()
                in
                false
        end

  in
  iter_or optrow


let set_kind_with_checking_loop (tvid : FreeID.t) (kd : mono_kind) : FreeID.t =
  let () =
    match kd with
    | UniversalKind ->
        ()

    | RecordKind(tyasc) ->
        let b = Assoc.fold_value (fun bacc ty -> let b = occurs tvid ty in bacc || b) false tyasc in
        if b then raise InternalInclusionError else ()
  in
  FreeID.set_kind tvid kd


let rec unify_sub ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) =
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
  let unify_list = List.iter (fun (t1, t2) -> unify_sub t1 t2) in

    match (tymain1, tymain2) with

    | (SynonymType(_, _, tyreal1), _) -> unify_sub tyreal1 ty2
    | (_, SynonymType(_, _, tyreal2)) -> unify_sub ty1 tyreal2

    | (BaseType(bsty1), BaseType(bsty2))  when bsty1 = bsty2 -> ()

    | (FuncType(optrow1, tydom1, tycod1), FuncType(optrow2, tydom2, tycod2))
      ->
        begin
          unify_option_row optrow1 optrow2;
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

    | (TypeVariable({contents= MonoLink(tyl1)}), _) -> unify_sub tyl1 (rng2, tymain2)

    | (_, TypeVariable({contents= MonoLink(tyl2)})) -> unify_sub (rng1, tymain1) tyl2

    | (TypeVariable({contents= MonoFree(tvid1)} as tvref1), TypeVariable({contents= MonoFree(tvid2)} as tvref2)) ->
        if FreeID.equal tvid1 tvid2 then
          ()
        else
          let b1 = occurs tvid1 ty2 in
          let b2 = occurs tvid2 ty1 in
          if b1 || b2 then
            raise InternalInclusionError
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
              if Level.less_than lev1 lev2 then
                (tvid1q, FreeID.set_level tvid2q lev1)
              else if Level.less_than lev2 lev1 then
                (FreeID.set_level tvid1q lev2, tvid2q)
              else
                (tvid1q, tvid2q)
          in
          tvref1 := MonoFree(tvid1l);
          tvref2 := MonoFree(tvid2l);
          let (oldtvref, newtvref, newtvid, newty) =
            if Range.is_dummy rng1 then (tvref1, tvref2, tvid2l, ty2) else (tvref2, tvref1, tvid1l, ty1)
          in
          oldtvref := MonoLink(newty);
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
          unify_list eqnlst;
          newtvref := MonoFree(set_kind_with_checking_loop newtvid kdunion);
          ()

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
              raise InternalContradictionError
            else
              let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
              let eqnlst =
                match kd1 with
                | UniversalKind      -> []
                | RecordKind(tyasc1) -> Assoc.intersection tyasc1 tyasc2
              in
              unify_list eqnlst;
              tvref1 := MonoLink(newty2);
              ()

      | (TypeVariable({contents= MonoFree(tvid1)} as tvref1), _) ->
          let chk = occurs tvid1 ty2 in
            if chk then
              raise InternalInclusionError
            else
              let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
              tvref1 := MonoLink(newty2)

      | (_, TypeVariable(_)) -> unify_sub ty2 ty1

      | _ -> raise InternalContradictionError


and unify_option_row optrow1 optrow2 =
  match (optrow1, optrow2) with
  | (OptionRowCons(ty1, tail1), OptionRowCons(ty2, tail2)) ->
      unify_sub ty1 ty2;
      unify_option_row tail1 tail2

  | (OptionRowEmpty, OptionRowEmpty) ->
      ()

  | (OptionRowVariable({contents = MonoORLink(optrow1)}), _) -> unify_option_row optrow1 optrow2
  | (_, OptionRowVariable({contents = MonoORLink(optrow2)})) -> unify_option_row optrow1 optrow2

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
  | (OptionRowCons(_, _), OptionRowEmpty)
    ->
      raise InternalContradictionError


let unify_ (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  try
    unify_sub ty1 ty2
  with
  | InternalInclusionError     -> raise (InclusionError(tyenv, ty1, ty2))
  | InternalContradictionError -> raise (ContradictionError(tyenv, ty1, ty2))


let final_tyenv    : Typeenv.t ref = ref (Typeenv.empty)


let rec typecheck
    (qtfbl : quantifiability) (lev : level)
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
            raise (UndefinedVariable(rng, mdlnmlst, varnm, Typeenv.find_candidates tyenv mdlnmlst varnm rng))

        | Some((pty, evid)) ->
            let tyfree = instantiate lev qtfbl pty in
            let tyres = overwrite_range_of_type tyfree rng in
(*
            let () = print_endline ("\n#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty) ^ " = " ^ (string_of_mono_type_basic tyres) ^ "\n  (" ^ (Range.to_string rng) ^ ")") in (* for debug *)
*)
                (ContentOf(rng, evid), tyres)
      end

  | UTConstructor(constrnm, utast1) ->
      begin
        match Typeenv.find_constructor qtfbl tyenv lev constrnm with
        | None -> raise (UndefinedConstructor(rng, constrnm, Typeenv.find_constructor_candidates qtfbl tyenv lev constrnm))
        | Some((tyarglist, tyid, tyc)) ->
(*
            let () = print_endline ("\n#Constructor " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc) ^ " in ... " ^ (string_of_mono_type_basic (rng, VariantType([], tyid))) ^ "(" ^ (Typeenv.find_type_name tyenv tyid) ^ ")") in (* for debug *)
*)
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
      let beta = (varrng, TypeVariable(PolyFree(ref (MonoFree(tvid))))) in
      let evid = EvalVarID.fresh varnmctx in
      let (e1, ty1) = typecheck_iter (Typeenv.add tyenv varnmctx (Poly(beta), evid)) utast1 in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      let () = unify tyret (Range.dummy "lambda-horz-return", BaseType(BoxRowType)) in
        (LambdaHorz(evid, e1), (rng, HorzCommandType(cmdargtylist)))

  | UTLambdaVert(varrng, varnmctx, utast1) ->
      let tvid = FreeID.fresh UniversalKind qtfbl lev () in
      let beta = (varrng, TypeVariable(PolyFree(ref (MonoFree(tvid))))) in
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

  | UTApply(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let eret = Apply(e1, e2) in
      begin
        match ty1 with
        | (_, FuncType(_, tydom, tycod)) ->
            let () = unify tydom ty2 in
            let tycodnew = overwrite_range_of_type tycod rng in
              (eret, tycodnew)

        | _ ->
            let tvid = FreeID.fresh UniversalKind qtfbl lev () in
            let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
            let orv = OptionRowVarID.fresh lev in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            let () = unify ty1 (get_range utast1, FuncType(optrow, ty2, beta)) in
              (eret, beta)
      end

  | UTApplyOptional(utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let eret = ApplyOptional(e1, e2) in
      begin
        match ty1 with
        | (_, FuncType(OptionRowCons(tyopt, optrow), tydom, tycod)) ->
            let () = unify tyopt ty2 in
            let tynew = (rng, FuncType(optrow, tydom, tycod)) in
              (eret, tynew)

        | _ ->
            let tvid1 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta1 = (Range.dummy "UTApplyOptional:dom", TypeVariable(ref (MonoFree(tvid1)))) in
            let tvid2 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta2 = (Range.dummy "UTApplyOptional:cod", TypeVariable(ref (MonoFree(tvid2)))) in
            let orv = OptionRowVarID.fresh lev in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            let () = unify ty1 (get_range utast1, FuncType(OptionRowCons(ty2, optrow), beta1, beta2)) in
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
            let tvid0 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta0 = (rng, TypeVariable(ref (MonoFree(tvid0)))) in
            let tvid1 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta1 = (rng, TypeVariable(ref (MonoFree(tvid1)))) in
            let tvid2 = FreeID.fresh UniversalKind qtfbl lev () in
            let beta2 = (rng, TypeVariable(ref (MonoFree(tvid2)))) in
            let orv = OptionRowVarID.fresh lev in
            let optrow = OptionRowVariable(ref (MonoORFree(orv))) in
            let () = unify ty1 (get_range utast1, FuncType(OptionRowCons(beta0, optrow), beta1, beta2)) in
              (eret, (rng, FuncType(optrow, beta1, beta2)))
      end

  | UTFunction(optargs, utpatbrs) ->
      let (optrow, evids, tyenvnew) = add_optionals_to_type_environment tyenv qtfbl lev optargs in
      let tvidO = FreeID.fresh UniversalKind qtfbl lev () in
      let betaO = (Range.dummy "UTFunction:dom", TypeVariable(ref (MonoFree(tvidO)))) in
      let tvidR = FreeID.fresh UniversalKind qtfbl lev () in
      let betaR = (Range.dummy "UTFunction:cod", TypeVariable(ref (MonoFree(tvidR)))) in
      let (patbrs, _) = typecheck_pattern_branch_list qtfbl lev tyenvnew utpatbrs betaO betaR in
      let e = Function(evids, patbrs) in
        (e, (rng, FuncType(optrow, betaO, betaR)))
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
      let beta = (Range.dummy "ut-pattern-match", TypeVariable(ref (MonoFree(tvid)))) in
      let (patbrs, tyP) = typecheck_pattern_branch_list qtfbl lev tyenv utpatbrs tyO beta in
      let () = Exhchecker.main rng patbrs tyO qtfbl lev tyenv in
        (PatternMatch(rng, eO, patbrs), tyP)

  | UTLetNonRecIn(mntyopt, utpat, utast1, utast2) ->
      let (pat, tyP, patvarmap) = typecheck_pattern qtfbl (Level.succ lev) tyenv utpat in
      let (e1, ty1) = typecheck qtfbl (Level.succ lev) tyenv utast1 in
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
      let ty = overwrite_range_of_type (Primitives.itemize_type ()) rng in
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
      let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
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
      let betaF = (rng, TypeVariable(ref (MonoFree(tvidF)))) in
      let tvid1 = FreeID.fresh (RecordKind(Assoc.of_list [(fldnm, betaF)])) qtfbl lev () in
      let beta1 = (get_range utast1, TypeVariable(ref (MonoFree(tvid1)))) in
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


and typecheck_command_arguments (tycmd : mono_type) (rngcmdapp : Range.t) qtfbl lev tyenv (utcmdarglst : untyped_command_argument list) (cmdargtylst : mono_command_argument_type list) : abstract_tree list =
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


and typecheck_input_vert (rng : Range.t) (qtfbl : quantifiability) (lev : level) (tyenv : Typeenv.t) (utivlst : untyped_input_vert_element list) =
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



and typecheck_input_horz (rng : Range.t) (qtfbl : quantifiability) (lev : level) (tyenv : Typeenv.t) (utihlst : untyped_input_horz_element list) =
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
    (qtfbl : quantifiability) (lev : level) (tyenv : Typeenv.t)
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


and typecheck_itemize (qtfbl : quantifiability) (lev : level) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) =
  let (e1, ty1) = typecheck qtfbl lev tyenv utast1 in
  let () = unify_ tyenv ty1 (Range.dummy "typecheck_itemize_string", BaseType(TextRowType)) in
  let elst = typecheck_itemize_list qtfbl lev tyenv utitmzlst in
    (NonValueConstructor("Item", PrimitiveTupleCons(e1, PrimitiveTupleCons(elst, Value(EndOfTuple)))))


and typecheck_itemize_list
    (qtfbl : quantifiability) (lev : level)
    (tyenv : Typeenv.t) (utitmzlst : untyped_itemize list) =
  match utitmzlst with
  | []                  -> Value(EndOfList)
  | hditmz :: tlitmzlst ->
      let ehd = typecheck_itemize qtfbl lev tyenv hditmz in
      let etl = typecheck_itemize_list qtfbl lev tyenv tlitmzlst in
        PrimitiveListCons(ehd, etl)


and typecheck_pattern_branch_list
    (qtfbl : quantifiability) (lev : level)
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
    (qtfbl : quantifiability) (lev : level)
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
        let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
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
        let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
          (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
        let evid = EvalVarID.fresh varnm in
(*
        let () = print_endline ("\n#PAdd " ^ varnm ^ " : " ^ (string_of_mono_type_basic beta)) in  (* for debug *)
*)
          (PVariable(evid), beta, PatternVarMap.empty |> PatternVarMap.add varnm (rng, evid, beta))

    | UTPAsVariable(varnm, utpat1) ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let beta = (rng, TypeVariable(ref (MonoFree(tvid)))) in
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
          | None -> raise (UndefinedConstructor(rng, constrnm, Typeenv.find_constructor_candidates qtfbl tyenv lev constrnm))
          | Some((tyarglist, tyid, tyc)) ->
(*
              let () = print_endline ("P-find " ^ constrnm ^ " of " ^ (string_of_mono_type_basic tyc)) in (* for debug *)
*)
              let (epat1, typat1, tyenv1) = iter utpat1 in
              let () = unify tyc typat1 in
                (PConstructor(constrnm, epat1), (rng, VariantType(tyarglist, tyid)), tyenv1)
        end


and make_type_environment_by_letrec
    (qtfbl : quantifiability) (lev : level)
    (tyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) =

  let rec add_mutual_variables (acctyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) : (Typeenv.t * (var_name * mono_type * EvalVarID.t) list) =
    let iter = add_mutual_variables in
      match utrecbinds with
      | [] ->
          (acctyenv, [])

      | UTLetRecBinding(_, varnm, astdef) :: tailcons ->
          let tvid = FreeID.fresh UniversalKind qtfbl (Level.succ lev) () in
          let tvref = ref (MonoFree(tvid)) in
          let rng = get_range astdef in
          let beta = (rng, TypeVariable(tvref)) in
          let pbeta = (rng, TypeVariable(PolyFree(tvref))) in
(*
          let () = print_endline ("#AddMutualVar " ^ varnm ^ " : '" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ " :: U") in (* for debug *)
*)
          let evid = EvalVarID.fresh varnm in
          let (tyenvfinal, tvtylst) = iter (Typeenv.add acctyenv varnm (Poly(pbeta), evid)) tailcons in
            (tyenvfinal, ((varnm, beta, evid) :: tvtylst))
  in

  let rec typecheck_mutual_contents
      (lev : level)
      (tyenvforrec : Typeenv.t) (utrecbinds : untyped_letrec_binding list) (tvtylst : (var_name * mono_type * EvalVarID.t) list)
      (acctvtylstout : (var_name * mono_type * EvalVarID.t) list)
  =
    let iter = typecheck_mutual_contents lev in
    let unify = unify_ tyenv in
    match (utrecbinds, tvtylst) with
    | ([], []) -> (tyenvforrec, [], List.rev acctvtylstout)

    | (UTLetRecBinding(mntyopt, varnm, utast1) :: tailcons, (_, beta, evid) :: tvtytail) ->
        let (e1, ty1) = typecheck qtfbl (Level.succ lev) tyenvforrec utast1 in
        begin
          match mntyopt with
          | None ->
              let () = unify ty1 beta in
              let (tyenvfinal, recbindtail, tvtylstoutfinal) =
                iter tyenvforrec tailcons tvtytail ((varnm, beta, evid) :: acctvtylstout)
              in
              begin
                match e1 with
                | Function([], patbrs1) -> (tyenvfinal, LetRecBinding(evid, patbrs1) :: recbindtail, tvtylstoutfinal)
                | _                     -> let (rng1, _) = utast1 in raise (BreaksValueRestriction(rng1))
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
                | Function([], patbrs1) -> (tyenvfinal, LetRecBinding(evid, patbrs1) :: recbindtail, tvtylstoutfinal)
                | _                     -> let (rng1, _) = utast1 in raise (BreaksValueRestriction(rng1))
              end
        end

    | _ -> assert false
  in

  let rec make_forall_type_mutual (tyenv : Typeenv.t) (tyenv_before_let : Typeenv.t) tvtylst tvtylst_forall =
    match tvtylst with
    | []                              -> (tyenv, tvtylst_forall)
    | (varnm, tvty, evid) :: tvtytail ->
        let prety = tvty in
(*
          let () = print_endline ("#Generalize1 " ^ varnm ^ " : " ^ (string_of_mono_type_basic prety)) in  (* for debug *)
*)
          let pty = (generalize lev (erase_range_of_type prety)) in
(*
          let () = print_endline ("#Generalize2 " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty)) in (* for debug *)
*)
          let tvtylst_forall_new = (varnm, pty, evid) :: tvtylst_forall in
            make_forall_type_mutual (Typeenv.add tyenv varnm (pty, evid)) tyenv_before_let tvtytail tvtylst_forall_new
  in

  let (tyenvforrec, tvtylstforrec) = add_mutual_variables tyenv utrecbinds in
  let (tyenv_new, mutletcons, tvtylstout) =
        typecheck_mutual_contents lev tyenvforrec utrecbinds tvtylstforrec [] in
  let (tyenv_forall, tvtylst_forall) = make_forall_type_mutual tyenv_new tyenv tvtylstout [] in
    (tyenv_forall, tvtylst_forall, mutletcons)


and make_type_environment_by_let_mutable (lev : level) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI) = typecheck Unquantifiable lev tyenv utastI in
(*
  let () = print_endline ("#AddMutable " ^ varnm ^ " : " ^ (string_of_mono_type_basic (varrng, RefType(tyI)))) in (* for debug *)
*)
  let evid = EvalVarID.fresh varnm in
  let tyenvI = Typeenv.add tyenv varnm (lift_poly (varrng, RefType(tyI)), evid) in
    (tyenvI, evid, eI, tyI)


let main (tyenv : Typeenv.t) (utast : untyped_abstract_tree) =
  begin
    final_tyenv := tyenv;
    (* Format.printf "%a" pp_untyped_abstract_tree utast; *)
    let (e, ty) = typecheck Quantifiable Level.bottom tyenv utast in
      (ty, !final_tyenv, e)
  end
