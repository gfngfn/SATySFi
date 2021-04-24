
open MyUtil
open Types
open TypeConv
open Display
open StaticEnv

exception UndefinedVariable              of Range.t * module_name list * var_name * var_name list
exception UndefinedConstructor           of Range.t * var_name * var_name list
exception InclusionError                 of mono_type * mono_type
exception ContradictionError             of mono_type * mono_type
exception UnknownUnitOfLength            of Range.t * length_unit_name
exception HorzCommandInMath              of Range.t
exception MathCommandInHorz              of Range.t
exception BreaksValueRestriction         of Range.t
exception MultiplePatternVariable        of Range.t * Range.t * var_name
exception InvalidOptionalCommandArgument of mono_type * Range.t
exception NeedsMoreArgument              of Range.t * mono_type * mono_type
exception TooManyArgument                of Range.t * mono_type
exception MultipleFieldInRecord          of Range.t * field_name
exception ApplicationOfNonFunction       of Range.t * mono_type
exception InvalidExpressionAsToStaging   of Range.t * stage
exception InvalidOccurrenceAsToStaging   of Range.t * var_name * stage
exception UndefinedHorzMacro             of Range.t * ctrlseq_name
exception UndefinedVertMacro             of Range.t * ctrlseq_name
exception InvalidNumberOfMacroArguments  of Range.t * macro_parameter_type list
exception LateMacroArgumentExpected      of Range.t * mono_type
exception EarlyMacroArgumentExpected     of Range.t * mono_type
exception IllegalNumberOfTypeArguments   of Range.t * type_name * int * int
exception TypeParameterBoundMoreThanOnce of Range.t * type_variable_name
exception ConflictInSignature            of Range.t * string
exception NotOfStructureType             of Range.t * signature

exception InternalInclusionError
exception InternalContradictionError of bool


let fresh_free_id (kd : mono_kind) (qtfbl : quantifiability) (lev : Level.t) =
  let fid = FreeID.fresh () in
  let fentry =
    KindStore.{
      level           = lev;
      quantifiability = qtfbl;
      mono_kind       = kd;
    }
  in
  KindStore.set_free_id fid fentry;
  fid


let make_type_parameters (pre : pre) (tyvars : (type_variable_name ranged) list) : pre * BoundID.t list =
  let (typarams, bidacc) =
    tyvars |> List.fold_left (fun (typarams, bidacc) (rng, tyvarnm) ->
      if typarams |> TypeParameterMap.mem tyvarnm then
        raise (TypeParameterBoundMoreThanOnce(rng, tyvarnm))
      else
        let mbbid = MustBeBoundID.fresh (Level.succ pre.level) in
        let bid = MustBeBoundID.to_bound_id mbbid in
        KindStore.set_bound_id bid { poly_kind = UniversalKind };
        (typarams |> TypeParameterMap.add tyvarnm mbbid, Alist.extend bidacc bid)
    ) (pre.type_parameters, Alist.empty)
  in
  ({ pre with type_parameters = typarams }, Alist.to_list bidacc)


let find_constructor_and_instantiate (pre : pre) (tyenv : Typeenv.t) (constrnm : constructor_name) (rng : Range.t) =
  match tyenv |> Typeenv.find_constructor constrnm with
  | None ->
      failwith "TODO (error): find_constructor_and_instantiate, not found"
        (*
          let cands = Typeenv.find_constructor_candidates pre tyenv constrnm in
          raise (UndefinedConstructor(rng, constrnm, cands))
        *)

  | Some(centry) ->
      let qtfbl = pre.quantifiability in
      let lev = pre.level in
      let freef rng tv =
        (rng, TypeVariable(tv))
      in
      let orfreef orv =
        OptionRowVariable(orv)
      in
      let tyid = centry.ctor_belongs_to in
      let (bidlist, pty) = centry.ctor_parameter in
      let pairlst =
        bidlist |> List.map (fun bid ->
          let bentry = KindStore.get_bound_id bid in
          let pkd = bentry.KindStore.poly_kind in
          let kd = instantiate_kind lev qtfbl pkd in
          let fid = fresh_free_id kd qtfbl lev in
          let tv = Updatable(ref (MonoFree(fid))) in
          let ty = (Range.dummy "tc-constructor", TypeVariable(tv)) in
          (ty, bid)
        )
      in
      let ty = instantiate_type_scheme freef orfreef pairlst pty in
      let tyarglst = pairlst |> List.map (fun (ty, _) -> ty) in
      (tyarglst, tyid, ty)


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
      let tvid = fresh_free_id UniversalKind qtfbl lev in
      let tv = Updatable(ref (MonoFree(tvid))) in
      let beta = (rng, TypeVariable(PolyFree(tv))) in
      let tyenvnew =
        let ventry =
          {
            val_name  = evid;
            val_type  = Poly(Primitives.option_type beta);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry in
        (tyenvnew, Alist.extend tyacc (rng, TypeVariable(tv)), Alist.extend evidacc evid)
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
      let param =
        match macparam with
        | UTLateMacroParam(param)  -> param
        | UTEarlyMacroParam(param) -> param
      in
      let (rng, varnm) = param in
      let evid = EvalVarID.fresh param in
      let (ptybody, beta) =
        let tvid = fresh_free_id UniversalKind pre.quantifiability pre.level in
        let tv = Updatable(ref (MonoFree(tvid))) in
        ((rng, TypeVariable(PolyFree(tv))), (rng, TypeVariable(tv)))
      in
      let (pty, macpty) =
      match macparam with
      | UTLateMacroParam(_) ->
          (Poly(Range.dummy "late-macro-param", CodeType(ptybody)), LateMacroParameter(beta))

      | UTEarlyMacroParam(_) ->
          (Poly(ptybody), EarlyMacroParameter(beta))
      in
      let ventry =
        {
          val_name  = evid;
          val_type  = pty;
          val_stage = Stage0;
        }
      in
      (tyenv |> Typeenv.add_value varnm ventry, Alist.extend evidacc evid, Alist.extend macptyacc macpty)
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
  | ContentOf(_)
  | Persistent(_) ->
      true

  | NonValueConstructor(constrnm, e1) -> iter e1
  | PrimitiveListCons(e1, e2)         -> iter e1 && iter e2
  | PrimitiveTuple(es)                -> es |> TupleList.to_list |> List.for_all iter
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
    let ventry =
      {
        val_name  = evid;
        val_type  = pty;
        val_stage = pre.stage;
      }
    in
    tyenvacc |> Typeenv.add_value varnm ventry
  ) patvarmap tyenv


let add_pattern_var_poly (pre : pre) (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = (generalize pre.level (erase_range_of_type ty)) in
    let ventry =
      {
        val_name  = evid;
        val_type  = pty;
        val_stage = pre.stage;
      }
    in
    tyenvacc |> Typeenv.add_value varnm ventry
  ) patvarmap tyenv


let point_type_main =
  (ProductType(TupleList.make
    (Range.dummy "point-type-1", BaseType(LengthType))
    (Range.dummy "point-type-2", BaseType(LengthType))
    []))


(* -- 'apply_tree_of_list': converts e0 and [e1; ...; eN] into (e0 e1 ... eN) -- *)
let apply_tree_of_list astfunc astlst =
  List.fold_left (fun astf astx -> Apply(astf, astx)) astfunc astlst


(* -- 'flatten_type': converts type (t1 -> ... -> tN -> t) into ([t1; ...; tN], t) -- *)
let flatten_type (ty : mono_type) : mono_command_argument_type list * mono_type =

  let rec aux_or = function
    | OptionRowEmpty                                                 -> []
    | OptionRowVariable(UpdatableRow{contents = MonoORFree(_)})      -> []
    | OptionRowVariable(UpdatableRow{contents = MonoORLink(optrow)}) -> aux_or optrow
    | OptionRowCons(ty, tail)                                        -> OptionalArgumentType(ty) :: aux_or tail
  in

  let rec aux acc ty =
    let (rng, tymain) = ty in
      match tymain with
      | TypeVariable(Updatable{contents= MonoLink(tylink)}) ->
          aux acc tylink

      | FuncType(optrow, tydom, tycod) ->
          let accnew =
            Alist.append acc (List.append (aux_or optrow) [MandatoryArgumentType(tydom)])
          in
          aux accnew tycod

      | _ -> (Alist.to_list acc, ty)
  in
  aux Alist.empty ty


let opaque_occurs (oidset : OpaqueIDSet.t) (modsig : signature) : bool =
  failwith "TODO: opaque_occurs"


let occurs (fid : FreeID.t) (ty : mono_type) =

  let lev =
    let fentry = KindStore.get_free_id fid in
    fentry.KindStore.level
  in

  let rec iter (_, tymain) =

    match tymain with
    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable(tvuref) ->
              begin
                match !tvuref with
                | MonoLink(tyl) ->
                    iter tyl

                | MonoFree(fidx) ->
                    if FreeID.equal fidx fid then
                      true
                    else
                      let fentryx = KindStore.get_free_id fidx in
                      let levx = fentryx.KindStore.level in
                      if Level.less_than lev levx then begin
                        KindStore.set_free_id fidx { fentryx with level = lev }
                          (* -- update level -- *)
                      end;
                      begin
                        match fentryx.KindStore.mono_kind with
                        | UniversalKind     -> false
                        | RecordKind(tyasc) -> Assoc.fold_value (fun b ty -> b || iter ty) false tyasc
                      end
              end

          | MustBeBound(_) ->
              false
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_or optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tys)               -> iter_list (tys |> TupleList.to_list)
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub

    | DataType(tyargs, tyid) ->
        begin
          match tyid with
          | TypeID.Variant(_) | TypeID.Opaque(_) ->
              iter_list tyargs

          | TypeID.Synonym(_sid) ->
              let tyact = failwith "TODO: get real type and instantiate" in
              iter tyact
        end

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

    | OptionRowVariable(UpdatableRow(orvuref)) ->
        begin
          match !orvuref with
          | MonoORLink(optrow) ->
              iter_or optrow

          | MonoORFree(orvx) ->
              let levx = OptionRowVarID.get_level orvx in
              if Level.less_than lev levx then begin
                orvuref := MonoORFree(OptionRowVarID.set_level orvx lev)
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
    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable(tvuref) ->
              begin
                match !tvuref with
                | MonoLink(tyl) ->
                    iter tyl

                | MonoFree(fidx) ->
                    let fentryx = KindStore.get_free_id fidx in
                    let levx = fentryx.KindStore.level in
                    if Level.less_than lev levx then begin
                      KindStore.set_free_id fidx { fentryx with level = lev }
                        (* -- update level -- *)
                    end;
                    begin
                      match fentryx.KindStore.mono_kind with
                      | UniversalKind     -> false
                      | RecordKind(tyasc) -> Assoc.fold_value (fun bacc ty -> let b = iter ty in bacc || b) false tyasc
                    end
              end

          | MustBeBound(_) ->
              false
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_or optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tys)              -> iter_list (tys |> TupleList.to_list)
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub

    | DataType(tyargs, tyid) ->
        begin
          match tyid with
          | TypeID.Variant(_) | TypeID.Opaque(_) ->
              iter_list tyargs

          | TypeID.Synonym(_sid) ->
              let tyact = failwith "TODO: get real type and instantiate it" in
              iter tyact
        end

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

    | OptionRowVariable(UpdatableRow(orvuref)) ->
        begin
          match !orvuref with
          | MonoORLink(optrow) ->
              iter_or optrow

          | MonoORFree(orvx) ->
              if OptionRowVarID.equal orv orvx then
                true
              else
                let levx = OptionRowVarID.get_level orvx in
                if Level.less_than lev levx then begin
                  orvuref := MonoORFree(OptionRowVarID.set_level orvx lev)
                    (* -- update level -- *)
                end;
                false
        end

  in
  iter_or optrow


let set_kind_with_occurs_check (fid : FreeID.t) (kd : mono_kind) : unit =
  begin
    match kd with
    | UniversalKind ->
        ()

    | RecordKind(tyasc) ->
        let b = Assoc.fold_value (fun bacc ty -> let b = occurs fid ty in bacc || b) false tyasc in
        if b then raise InternalInclusionError else ()
  end;
  let fentry = KindStore.get_free_id fid in
  KindStore.set_free_id fid { fentry with mono_kind = kd }


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
  let unify_list tys1 tys2 =
    try
      let tyzipped = List.combine tys1 tys2 in
      tyzipped |> List.iter (fun (t1, t2) -> unify t1 t2)
    with
    | Invalid_argument(_) ->
        raise (InternalContradictionError(reversed))
  in

    match (tymain1, tymain2) with

    | (DataType(_, TypeID.Synonym(sid1)), _) ->
        let tyreal1 = failwith "TODO: get real type" in
        unify tyreal1 ty2

    | (_, DataType(_, TypeID.Synonym(sid2))) ->
        let tyreal2 = failwith "TODO: get real type" in
        unify ty1 tyreal2

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

    | (ProductType(tys1), ProductType(tys2)) ->
        unify_list (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

    | (RecordType(tyasc1), RecordType(tyasc2)) ->
        if not (Assoc.domain_same tyasc1 tyasc2) then
          raise (InternalContradictionError(reversed))
        else
          Assoc.combine_value tyasc1 tyasc2 |> List.iter (fun (ty1, ty2) -> unify ty1 ty2)

    | (DataType(tyargs1, TypeID.Variant(vid1)), DataType(tyargs2, TypeID.Variant(vid2))) ->
        if TypeID.Variant.equal vid1 vid2 then
          unify_list tyargs1 tyargs2
        else
          raise (InternalContradictionError(reversed))

    | (DataType(tyargs1, TypeID.Opaque(oid1)), DataType(tyargs2, TypeID.Opaque(oid2))) ->
        if TypeID.Opaque.equal oid1 oid2 then
          unify_list tyargs1 tyargs2
        else
          raise (InternalContradictionError(reversed))

    | (ListType(tysub1), ListType(tysub2)) -> unify tysub1 tysub2
    | (RefType(tysub1), RefType(tysub2))   -> unify tysub1 tysub2
    | (CodeType(tysub1), CodeType(tysub2)) -> unify tysub1 tysub2

    | (TypeVariable(Updatable{contents = MonoLink(tylinked1)}), _) -> unify tylinked1 ty2
    | (_, TypeVariable(Updatable{contents = MonoLink(tylinked2)})) -> unify ty1 tylinked2

    | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)),
          TypeVariable(Updatable({contents = MonoFree(fid2)} as tvref2))) ->
        if FreeID.equal fid1 fid2 then
          ()
        else
          let b1 = occurs fid1 ty2 in
          let b2 = occurs fid2 ty1 in
          if b1 || b2 then
            raise InternalInclusionError
          else
            let fentry1 = KindStore.get_free_id fid1 in
            let fentry2 = KindStore.get_free_id fid2 in

            (* Equate quantifiabilities *)
            let (fentry1, fentry2) =
              match (fentry1.quantifiability, fentry2.quantifiability) with
              | (Quantifiable, Quantifiable) ->
                  (fentry1, fentry2)

              | _ ->
                  ({ fentry1 with quantifiability = Unquantifiable },
                   { fentry2 with quantifiability = Unquantifiable})
            in

            (* Equate levels *)
            let lev1 = fentry1.level in
            let lev2 = fentry2.level in
            let (fentry1, fentry2) =
              if Level.less_than lev1 lev2 then
                (fentry1, { fentry2 with level = lev1 })
              else if Level.less_than lev2 lev1 then
                ({ fentry1 with level = lev2 }, fentry2)
              else
                (fentry1, fentry2)
            in

            KindStore.set_free_id fid1 fentry1;
            KindStore.set_free_id fid2 fentry2;

            (* Generate constraints by unifying kinds and solve them *)
            let (oldtvref, newtvref, newfid, newty) =
              if Range.is_dummy rng1 then (tvref1, tvref2, fid2, ty2) else (tvref2, tvref1, fid1, ty1)
            in
            oldtvref := MonoLink(newty);
            let (eqns, kdunion) =
              let kd1 = fentry1.mono_kind in
              let kd2 = fentry2.mono_kind in
              match (kd1, kd2) with
              | (UniversalKind, UniversalKind)       -> ([], UniversalKind)
              | (RecordKind(asc1), UniversalKind)    -> ([], RecordKind(asc1))
              | (UniversalKind, RecordKind(asc2))    -> ([], RecordKind(asc2))

              | (RecordKind(asc1), RecordKind(asc2)) ->
                  let kdunion = RecordKind(Assoc.union asc1 asc2) in
                  (Assoc.intersection asc1 asc2, kdunion)
            in
            eqns |> List.iter (fun (ty1, ty2) -> unify ty1 ty2);
            set_kind_with_occurs_check newfid kdunion

      | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)), RecordType(tyasc2)) ->
          let fentry1 = KindStore.get_free_id fid1 in
          let kd1 = fentry1.mono_kind in
          let binc =
            match kd1 with
            | UniversalKind      -> true
            | RecordKind(tyasc1) -> Assoc.domain_included tyasc1 tyasc2
          in
          let chk = occurs fid1 ty2 in
          if chk then
            raise InternalInclusionError
          else if not binc then
            raise (InternalContradictionError(reversed))
          else
            let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
            let eqns =
              match kd1 with
              | UniversalKind      -> []
              | RecordKind(tyasc1) -> Assoc.intersection tyasc1 tyasc2
            in
            eqns |> List.iter (fun (ty1, ty2) -> unify ty1 ty2);
            tvref1 := MonoLink(newty2)

      | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)), _) ->
          let fentry1 = KindStore.get_free_id fid1 in
          let kd1 = fentry1.mono_kind in
          let () =
            match kd1 with
            | UniversalKind -> ()
            | RecordKind(_) -> raise (InternalContradictionError(reversed))
                (* -- `ty2` is not a record type, a type variable, nor a link,
                      and thereby cannot have a record kind -- *)
          in
          let chk = occurs fid1 ty2 in
          if chk then
            raise InternalInclusionError
          else
            let newty2 = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
            tvref1 := MonoLink(newty2)

      | (TypeVariable(MustBeBound(mbbid1)), TypeVariable(MustBeBound(mbbid2))) ->
          if MustBeBoundID.equal mbbid1 mbbid2 then
            ()
          else
            raise (InternalContradictionError(reversed))

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

  | (OptionRowVariable(UpdatableRow{contents = MonoORLink(optrow1)}), _) ->
      unify_option_row ~reversed optrow1 optrow2

  | (_, OptionRowVariable(UpdatableRow{contents = MonoORLink(optrow2)})) ->
      unify_option_row ~reversed optrow1 optrow2

  | (OptionRowVariable(UpdatableRow({contents = MonoORFree(orv1)} as orviref1)),
        OptionRowVariable(UpdatableRow{contents = MonoORFree(orv2)})) ->
      if OptionRowVarID.equal orv1 orv2 then () else
        orviref1 := MonoORLink(optrow2)

  | (OptionRowVariable(UpdatableRow({contents = MonoORFree(orv1)} as orviref1)), _) ->
      if occurs_optional_row orv1 optrow2 then
        raise InternalInclusionError
      else
        orviref1 := MonoORLink(optrow2)

  | (_, OptionRowVariable(UpdatableRow({contents = MonoORFree(orv2)} as orviref2))) ->
      if occurs_optional_row orv2 optrow1 then
        raise InternalInclusionError
      else
        orviref2 := MonoORLink(optrow1)

  | (OptionRowEmpty, OptionRowCons(_, _))
  | (OptionRowCons(_, _), OptionRowEmpty) ->
      raise (InternalContradictionError(reversed))


let unify (ty1 : mono_type) (ty2 : mono_type) =
  try
    unify_sub ~reversed:false ty1 ty2
  with
  | InternalInclusionError ->
      raise (InclusionError(ty1, ty2))

  | InternalContradictionError(reversed) ->
      if reversed then
        raise (ContradictionError(ty2, ty1))
      else
        raise (ContradictionError(ty1, ty2))


let fresh_type_variable rng pre kd =
  let tvid = fresh_free_id kd pre.quantifiability pre.level in
  let tvuref = ref (MonoFree(tvid)) in
  (rng, TypeVariable(Updatable(tvuref)))


let base bc =
  ASTBaseConstant(bc)


let rec typecheck
    (pre : pre) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?s:(s = pre.stage) ?l:(l = pre.level) ?p:(p = pre.type_parameters) ?q:(q = pre.quantifiability) t u =
    let presub =
      {
        stage           = s;
        type_parameters = p;
        quantifiability = q;
        level           = l;
      }
    in
    typecheck presub t u
  in
  match utastmain with
  | UTStringEmpty         -> (base (BCString(""))   , (rng, BaseType(StringType)))
  | UTIntegerConstant(nc) -> (base (BCInt(nc))      , (rng, BaseType(IntType))   )
  | UTFloatConstant(nc)   -> (base (BCFloat(nc))    , (rng, BaseType(FloatType)) )
  | UTStringConstant(sc)  -> (base (BCString(sc))   , (rng, BaseType(StringType)))
  | UTBooleanConstant(bc) -> (base (BCBool(bc))     , (rng, BaseType(BoolType))  )
  | UTUnitConstant        -> (base BCUnit           , (rng, BaseType(UnitType))  )
  | UTHorz(hblst)         -> (base (BCHorz(hblst))  , (rng, BaseType(BoxRowType)))
  | UTVert(imvblst)       -> (base (BCVert(imvblst)), (rng, BaseType(BoxColType)))

  | UTPositionedString(ipos, s) ->
      begin
        match pre.stage with
        | Stage1 | Persistent0 ->
            raise (InvalidExpressionAsToStaging(rng, Stage0))

        | Stage0 ->
            let e =
              let e1 = base (BCInputPos(ipos)) in
              let e2 = base (BCString(s)) in
              PrimitiveTuple(TupleList.make e1 e2 []) in
            let ty =
              let ty1 = (Range.dummy "positioned1", BaseType(InputPosType)) in
              let ty2 = (Range.dummy "positioned2", BaseType(StringType)) in
              (rng, ProductType(TupleList.make ty1 ty2 []))
            in
            (e, ty)
      end

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

  | UTOpenIn(rngtok, mdlnm, utast1) ->
      failwith "TODO: UTOpenIn"
(*
      let tyenvnew = tyenv |> Typeenv.open_module rngtok mdlnm in
      typecheck_iter tyenvnew utast1
*)

  | UTContentOf(_, varnm) ->
      begin
        match tyenv |> Typeenv.find_value varnm with
        | None ->
            failwith "TODO (error): UTContentOf, not found"
(*
            let cands = Typeenv.find_candidates tyenv mdlnmlst varnm rng in
            raise (UndefinedVariable(rng, mdlnmlst, varnm, cands))
*)

        | Some(ventry) ->
            let evid = ventry.val_name in
            let pty = ventry.val_type in
            let stage = ventry.val_stage in
            let tyfree = instantiate pre.level pre.quantifiability pty in
            let tyres = overwrite_range_of_type tyfree rng in
            begin
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0)
              | (Stage1, Persistent0) ->
                  (Persistent(rng, evid), tyres)

              | (Stage0, Stage0)
              | (Stage1, Stage1) ->
(*
                  let () = print_endline ("\n#Content " ^ varnm ^ " : " ^ (string_of_poly_type_basic pty) ^ " = " ^ (string_of_mono_type_basic tyres) ^ "\n  (" ^ (Range.to_string rng) ^ ")") in (* for debug *)
*)
                  (ContentOf(rng, evid), tyres)

              | _ ->
                  raise (InvalidOccurrenceAsToStaging(rng, varnm, stage))
            end
      end

  | UTConstructor(constrnm, utast1) ->
      let (tyargs, vid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 tyc;
      let tyres = (rng, DataType(tyargs, TypeID.Variant(vid))) in
      (NonValueConstructor(constrnm, e1), tyres)

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
      let tyenvsub =
        let ventry =
          {
            val_name  = evid;
            val_type  = Poly(varrng, BaseType(bstyvar));
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnmctx ventry in
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
      let tyenvsub =
        let ventry =
          {
            val_name  = evid;
            val_type  = Poly(varrng, BaseType(bstyvar));
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnmctx ventry in
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
            let orvuref = ref (MonoORFree(orv)) in
            let optrow = OptionRowVariable(UpdatableRow(orvuref)) in
            unify ty1 (get_range utast1, FuncType(optrow, ty2, beta));
            (eret, beta)

        | ty1 ->
            let (rng1, _) = utast1 in
            raise (ApplicationOfNonFunction(rng1, ty1))
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
            let orvuref = ref (MonoORFree(orv)) in
            let optrow = OptionRowVariable(UpdatableRow(orvuref)) in
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
            let orvuref = ref (MonoORFree(orv)) in
            let optrow = OptionRowVariable(UpdatableRow(orvuref)) in
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

  | UTLetIn(UTNonRec(tyannot, utpat, utast1), utast2) ->
      let presub = { pre with level = Level.succ pre.level; } in
      let (pat, tyP, patvarmap) = typecheck_pattern presub tyenv utpat in
      let (e1, ty1) = typecheck presub tyenv utast1 in
      unify ty1 tyP;
      tyannot |> Option.map (fun mnty ->
        let tyA = decode_manual_type pre tyenv mnty in
        unify ty1 tyA
      ) |> ignore;
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

  | UTLetIn(UTRec(utrecbinds), utast2) ->
      let quints = typecheck_letrec pre tyenv utrecbinds in
      let (tyenv, recbindacc) =
        quints |> List.fold_left (fun (tyenv, recbindacc) quint ->
          let (x, pty, evid, stage, recbind) = quint in
          let tyenv =
            let ventry =
              {
                val_name  = evid;
                val_type  = pty;
                val_stage = stage;
              }
            in
            tyenv |> Typeenv.add_value x ventry
          in
          let recbindacc = Alist.extend recbindacc recbind in
          (tyenv, recbindacc)
        ) (tyenv, Alist.empty)
      in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      (LetRecIn(recbindacc |> Alist.to_list, e2), ty2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let (eB, tyB) = typecheck_iter tyenv utastB in
      unify tyB (Range.dummy "if-bool", BaseType(BoolType));
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      unify ty2 ty1;
      (IfThenElse(eB, e1, e2), ty1)

(* ---- imperatives ---- *)

  | UTLetIn(UTMutable((varrng, varnm), utastI), utastA) ->
      let (tyenvI, evid, eI, tyI) = make_type_environment_by_let_mutable pre tyenv varrng varnm utastI in
      let (eA, tyA) = typecheck_iter tyenvI utastA in
      (LetMutableIn(evid, eI, eA), tyA)

  | UTOverwrite(varrng, varnm, utastN) ->
      begin
        match typecheck_iter tyenv (varrng, UTContentOf([], varnm)) with
        | ((ContentOf(_, evid) | Persistent(_, evid)), tyvar) ->
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

  | UTTuple(utasts) ->
      let etys = TupleList.map (typecheck_iter tyenv) utasts in
      let es = TupleList.map fst etys in
      let tys = TupleList.map snd etys in
      let tyres = (rng, ProductType(tys)) in
      (PrimitiveTuple(es), tyres)

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

(*
(* ---- other fundamentals ---- *)
  | UTDeclareVariantIn(mutvarntcons, utastA) ->
      let tyenvnew = Typeenv.add_mutual_cons tyenv pre.level mutvarntcons in
      typecheck_iter tyenvnew utastA
  | UTModule(mdlrng, mdlnm, sigopt, utastM, utastA) ->
      let tyenvinner = Typeenv.enter_new_module tyenv mdlnm in
      let (eM, _) = typecheck_iter tyenvinner utastM in
        (* -- the final type environment in the module has been written to `final_tyenv` -- *)
      let tyenvmid = Typeenv.sigcheck mdlrng pre (!final_tyenv) tyenv sigopt in
      let tyenvouter = Typeenv.leave_module tyenvmid in
      let (eA, tyA) = typecheck_iter tyenvouter utastA in
      (Module(eM, eA), tyA)
*)

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

(*
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
            let (e2, ty2) = typecheck_iter (Typeenv.add_macro tyenv csnm (HorzMacroType(macparamtys), evid)) utast2 in
            let e = Prev(LetNonRecIn(PVariable(evid), abstraction_list argevids (Next(e1)), Next(e2))) in
            (e, ty2)
      end

  | UTLetVertMacroIn(rngcs, csnm, macparams, utast1, utast2) ->
      begin
        match pre.stage with
        | Stage0 | Persistent0 ->
            raise (InvalidExpressionAsToStaging(rng, Stage1))

        | Stage1 ->
            let (tyenv, argevids, macparamtys) = add_macro_parameters_to_type_environment tyenv pre macparams in
            let (e1, ty1) = typecheck_iter ~s:Stage1 tyenv utast1 in
            unify ty1 (Range.dummy "let-block-macro", BaseType(TextColType));
            let evid = EvalVarID.fresh (rngcs, csnm) in
            let (e2, ty2) = typecheck_iter (Typeenv.add_macro tyenv csnm (VertMacroType(macparamtys), evid)) utast2 in
            let e = Prev(LetNonRecIn(PVariable(evid), abstraction_list argevids (Next(e1)), Next(e2))) in
            (e, ty2)
      end
*)

and typecheck_command_arguments (ecmd : abstract_tree) (tycmd : mono_type) (rngcmdapp : Range.t) (pre : pre) tyenv (utcmdarglst : untyped_command_argument list) (cmdargtylst : mono_command_argument_type list) : abstract_tree =
  let rec aux eacc utcmdarglst cmdargtylst =
    match (utcmdarglst, cmdargtylst) with
    | ([], _) ->
        cmdargtylst |> List.iter (function
        | MandatoryArgumentType(ty) -> raise (NeedsMoreArgument(rngcmdapp, tycmd, ty))
        | OptionalArgumentType(_)   -> ()
        );
        eacc

    | (_ :: _, []) ->
        raise (TooManyArgument(rngcmdapp, tycmd))

    | (UTMandatoryArgument(_) :: _, OptionalArgumentType(_) :: cmdargtytail) ->
          aux eacc utcmdarglst cmdargtytail

    | (UTMandatoryArgument(utastA) :: utcmdargtail, MandatoryArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck pre tyenv utastA in
        unify tyA tyreq;
        aux (Apply(eacc, eA)) utcmdargtail cmdargtytail

    | (UTOptionalArgument(utastA) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        let (eA, tyA) = typecheck pre tyenv utastA in
        unify tyA tyreq;
        aux (ApplyOptional(eacc, eA)) utcmdargtail cmdargtytail

    | (UTOptionalArgument((rngA, _)) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tycmd, rngA))

    | (UTOmission(_) :: utcmdargtail, OptionalArgumentType(tyreq) :: cmdargtytail) ->
        aux (ApplyOmission(eacc)) utcmdargtail cmdargtytail

    | (UTOmission(rngA) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tycmd, rngA))
  in
  aux ecmd utcmdarglst cmdargtylst


and typecheck_math (pre : pre) tyenv ((rng, utmathmain) : untyped_math) : abstract_tree =
  let iter = typecheck_math pre tyenv in
  let open HorzBox in
    match utmathmain with
    | UTMChar(s) ->
        let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
        let ms = uchs |> List.map (fun uch -> MathPure(MathVariantChar(uch))) in
        ASTMath(ms)

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
        unify ty0 (Range.dummy "math-embedded-var", BaseType(MathType));
        e0


and typecheck_path pre tyenv (utpathcomplst : (untyped_abstract_tree untyped_path_component) list) (utcycleopt : (unit untyped_path_component) option) =

  let typecheck_anchor_point utastpt =
    let (ept, typt) = typecheck pre tyenv utastpt in
    unify typt (Range.dummy "typecheck-path", point_type_main);
    ept
  in

  let pathcomplst =
    utpathcomplst |> List.fold_left (fun acc utpathcomp ->
      match utpathcomp with
      | UTPathLineTo(utastpt) ->
          let (ept, typt) = typecheck pre tyenv utastpt in
          unify typt (Range.dummy "typecheck-path-L", point_type_main);
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
      | UTPathLineTo(()) ->
          PathLineTo(())

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
        unify ty0 (Range.dummy "UTInputVertContent", BaseType(TextColType));
        aux (Alist.extend acc (InputVertContent(e0))) tail

    | (rngapp, UTInputVertMacro(vmacro, utmacargs)) :: tail ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              raise (InvalidExpressionAsToStaging(rngapp, Stage1))

          | Stage1 ->
              let (rngcs, csnm) = vmacro in
              begin
                match tyenv |> Typeenv.find_macro csnm with
                | None ->
                    raise (UndefinedVertMacro(rngcs, csnm))

                | Some(macentry) ->
                    let macparamtys =
                      match macentry.macro_type with
                      | VertMacroType(macparamtys) -> macparamtys
                      | _                          -> assert false
                    in
                    let evid = macentry.macro_name in
                    let eargs = typecheck_macro_arguments rngapp pre tyenv macparamtys utmacargs in
                    let eapp = apply_tree_of_list (ContentOf(rngcs, evid)) eargs in
                    let iv = InputVertContent(Prev(eapp)) in
                    aux (Alist.extend acc iv) tail
              end
        end
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
        unify tymath (Range.dummy "ut-input-horz-embedded-math", BaseType(MathType));
        aux (Alist.extend acc (InputHorzEmbeddedMath(emath))) tail

    | (_, UTInputHorzEmbeddedCodeText(s)) :: tail ->
        aux (Alist.extend acc (InputHorzEmbeddedCodeText(s))) tail

    | (_, UTInputHorzContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify ty0 (Range.dummy "ut-input-horz-content", BaseType(TextRowType));
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
                match tyenv |> Typeenv.find_macro csnm with
                | None ->
                    raise (UndefinedHorzMacro(rngcs, csnm))

                | Some(macentry) ->
                    let macparamtys =
                      match macentry.macro_type with
                      | HorzMacroType(macparamtys) -> macparamtys
                      | _                          -> assert false
                    in
                    let evid = macentry.macro_name in
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
            unify tyarg tyexp;
            Alist.extend argacc (Next(earg))
              (* -- late arguments are converted to quoted arguments -- *)

        | (EarlyMacroParameter(tyexp), UTEarlyMacroArg(utast)) ->
            let (earg, tyarg) = typecheck { pre with stage = Stage0 } tyenv utast in
            unify tyarg tyexp;
            Alist.extend argacc earg

        | (LateMacroParameter(tyexp), UTEarlyMacroArg((rngarg, _))) ->
            raise (LateMacroArgumentExpected(rngarg, tyexp))

        | (EarlyMacroParameter(tyexp), UTLateMacroArg((rngarg, _))) ->
            raise (EarlyMacroArgumentExpected(rngarg, tyexp))

      ) Alist.empty macparamtys utmacargs
    in
    Alist.to_list argacc


and typecheck_record (pre : pre) (tyenv : Typeenv.t) (flutlst : (field_name * untyped_abstract_tree) list) (rng : Range.t) =
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
  unify ty1 (Range.dummy "typecheck_itemize_string", BaseType(TextRowType));
  let e2 = typecheck_itemize_list pre tyenv utitmzlst in
  (NonValueConstructor("Item", PrimitiveTuple(TupleList.make e1 e2 [])))


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
        unify tyB (Range.dummy "pattern-match-cons-when", BaseType(BoolType));
        let (e1, ty1) = typecheck pre tyenvpat utast1 in
        (PatternBranchWhen(epat, eB, e1), typat, ty1)


and typecheck_pattern_branch_list (pre : pre) (tyenv : Typeenv.t) (utpatbrs : untyped_pattern_branch list) (tyobj : mono_type) (tyres : mono_type) : pattern_branch list =
  utpatbrs |> List.map (fun utpatbr ->
    let (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
    unify typat tyobj;
    unify ty1 tyres;
    patbr
  )


and typecheck_pattern (pre : pre) (tyenv : Typeenv.t) ((rng, utpatmain) : untyped_pattern_tree) : pattern_tree * mono_type * pattern_var_map =
  let iter = typecheck_pattern pre tyenv in
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

    | UTPTuple(utpats) ->
        let tris = TupleList.map iter utpats in
        let epats = tris |> TupleList.map (fun (epat, _, _) -> epat) in
        let typats = tris |> TupleList.map (fun (_, typat, _) -> typat) in
        let tyres = (rng, ProductType(typats)) in
        let patvarmap =
          let patvarmaps = tris |> TupleList.to_list |> List.map (fun (_, _, patvarmap) -> patvarmap) in
          List.fold_left unite_pattern_var_map PatternVarMap.empty patvarmaps
        in
        (PTuple(epats), tyres, patvarmap)

    | UTPWildCard ->
        let beta = fresh_type_variable rng pre UniversalKind in
        (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let beta = fresh_type_variable rng pre UniversalKind in
        let evid = EvalVarID.fresh (rng, varnm) in
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
        let (tyargs, vid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
        let (epat1, typat1, tyenv1) = iter utpat1 in
        unify tyc typat1;
        (PConstructor(constrnm, epat1), (rng, DataType(tyargs, TypeID.Variant(vid))), tyenv1)


and typecheck_letrec (pre : pre) (tyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) : (var_name * poly_type * EvalVarID.t * stage * letrec_binding) list =

  (* First, adds a type variable for each bound identifier. *)
  let (tyenv, utrecacc) =
    utrecbinds |> List.fold_left (fun (tyenv, utrecacc) utrecbind ->
      let UTLetRecBinding(_, varrng, varnm, astdef) = utrecbind in
      let tvuref =
        let tvid = fresh_free_id UniversalKind pre.quantifiability (Level.succ pre.level) in
        ref (MonoFree(tvid))
      in
      let tv = Updatable(tvuref) in
      let rng = get_range astdef in
      let beta = (rng, TypeVariable(tv)) in
      let pbeta = (rng, TypeVariable(PolyFree(tv))) in
      let evid = EvalVarID.fresh (varrng, varnm) in
      let tyenv =
        let ventry =
          {
            val_type  = Poly(pbeta);
            val_name  = evid;
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry
      in
      let utrecacc = Alist.extend utrecacc (utrecbind, beta, evid) in
      (tyenv, utrecacc)
    ) (tyenv, Alist.empty)
  in

  (* Typechecks each body of the definitions. *)
  let tupleacc =
    utrecacc |> Alist.to_list |> List.fold_left (fun tupleacc utrec ->
      let (UTLetRecBinding(mntyopt, _, varnm, utast1), beta, evid) = utrec in
      let (e1, ty1) = typecheck { pre with level = Level.succ pre.level; } tyenv utast1 in
      begin
        match e1 with
        | Function([], patbr1) ->
            unify ty1 beta;
            mntyopt |> Option.map (fun mnty ->
              let tyin = decode_manual_type pre tyenv mnty in
              unify tyin beta
            ) |> Option.value ~default:();
            let recbind = LetRecBinding(evid, patbr1) in
            let tupleacc = Alist.extend tupleacc (varnm, beta, evid, recbind) in
            tupleacc

        | _ ->
            let (rng1, _) = utast1 in
            raise (BreaksValueRestriction(rng1))
      end
    ) Alist.empty
  in

  tupleacc |> Alist.to_list |> List.map (fun (varnm, ty, evid, recbind) ->
    let pty = generalize pre.level (erase_range_of_type ty) in
    (varnm, pty, evid, pre.stage, recbind)
  )


and make_type_environment_by_let_mutable (pre : pre) (tyenv : Typeenv.t) varrng varnm utastI =
  let (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
  let evid = EvalVarID.fresh (varrng, varnm) in
  let tyenvI =
    let ventry =
      {
        val_type  = lift_poly (varrng, RefType(tyI));
        val_name  = evid;
        val_stage = pre.stage;
      }
    in
    tyenv |> Typeenv.add_value varnm ventry
  in
  (tyenvI, evid, eI, tyI)


and decode_manual_type_scheme (k : TypeID.t -> unit) (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : mono_type =
  let invalid rng tynm ~expect:len_expected ~actual:len_actual =
    raise (IllegalNumberOfTypeArguments(rng, tynm, len_expected, len_actual))
  in
  let rec aux (rng, mtymain) =
    let tymain =
      match mtymain with
      | MTypeName(tynm, mtyargs) ->
          let tyargs = mtyargs |> List.map aux in
          let len_actual = List.length tyargs in
          begin
            match tyenv |> Typeenv.find_type tynm with
            | None ->
                begin
                  match base_type_map |> TypeNameMap.find_opt tynm with
                  | None ->
                      failwith "TODO (error): report undefined type name"

                  | Some(bt) ->
                      BaseType(bt)
                end

            | Some(tentry) ->
                let tyid = tentry.type_id in
                let len_expected = tentry.type_arity in
                if len_actual = len_expected then
                  begin
                    k tyid;
                    DataType(tyargs, tyid)
                  end
                else
                  invalid rng tynm ~expect:len_expected ~actual:len_actual
          end

      | MTypeParam(typaram) ->
          begin
            match pre.type_parameters |> TypeParameterMap.find_opt typaram with
            | None ->
                failwith "TODO (error): unbound type parameter"

            | Some(mbbid) ->
                TypeVariable(MustBeBound(mbbid))
          end

      | MFuncType(mtyadds, mtydom, mtycod) ->
          FuncType(aux_row mtyadds, aux mtydom, aux mtycod)

      | MProductType(mntys) ->
          ProductType(TupleList.map aux mntys)

      | MRecordType(mnasc) ->
          RecordType(Assoc.map_value aux mnasc)

      | MHorzCommandType(mncmdargtys) -> HorzCommandType(List.map aux_cmd mncmdargtys)
      | MVertCommandType(mncmdargtys) -> VertCommandType(List.map aux_cmd mncmdargtys)
      | MMathCommandType(mncmdargtys) -> MathCommandType(List.map aux_cmd mncmdargtys)

    in
    (rng, tymain)

  and aux_cmd = function
    | MMandatoryArgumentType(mnty) -> MandatoryArgumentType(aux mnty)
    | MOptionalArgumentType(mnty)  -> OptionalArgumentType(aux mnty)

  and aux_row (mtyadds : manual_type list) =
    List.fold_right (fun mty row -> OptionRowCons(aux mty, row)) mtyadds OptionRowEmpty
  in
  aux mty


and decode_manual_type (pre : pre) : Typeenv.t -> manual_type -> mono_type =
  decode_manual_type_scheme (fun _ -> ()) pre


and decode_manual_type_and_get_dependency (vertices : SynonymIDSet.t) (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : mono_type * SynonymIDSet.t =
  let hashset = SynonymIDHashTable.create 32 in
  let k = function
    | TypeID.Synonym(sid) ->
        if vertices |> SynonymIDSet.mem sid then
          SynonymIDHashTable.replace hashset sid ()
        else
          ()

    | _ ->
        ()
  in
  let ty = decode_manual_type_scheme k pre tyenv mty in
  let dependencies =
    SynonymIDHashTable.fold (fun sid () set ->
      set |> SynonymIDSet.add sid
    ) hashset SynonymIDSet.empty
  in
  (ty, dependencies)


and make_constructor_branch_map (pre : pre) (tyenv : Typeenv.t) (utctorbrs : constructor_branch list) =
  utctorbrs |> List.fold_left (fun ctormap utctorbr ->
    match utctorbr with
    | UTConstructorBranch((rng, ctornm), mtyarg) ->
        let tyarg = decode_manual_type pre tyenv mtyarg in
        let ptyarg = generalize pre.level tyarg in
        ctormap |> ConstructorMap.add ctornm ptyarg
  ) ConstructorMap.empty


and typecheck_module (stage : stage) (tyenv : Typeenv.t) (utmod : untyped_module) : signature abstracted * binding list =
  let (rng, utmodmain) = utmod in
  match utmodmain with
  | UTModVar(modnm) ->
      begin
        match tyenv |> Typeenv.find_module modnm with
        | None ->
            failwith "TODO (error): typecheck_module, UTModVar, not found"

        | Some(mentry) ->
            let modsig = mentry.mod_signature in
            ((OpaqueIDSet.empty, modsig), [])  (* TODO: output *)
      end

  | UTModBinds(utbinds) ->
      let (binds, _, (oidset, ssig)) = typecheck_binding_list stage tyenv utbinds in
      ((oidset, ConcStructure(ssig)), binds)

  | UTModProjMod(utmod1, (_, modnm2)) ->
      let (absmodsig1, binds) = typecheck_module stage tyenv utmod1 in
      let (oidset, modsig1) = absmodsig1 in
      begin
        match modsig1 with
        | ConcFunctor(_) ->
            let (rng1, _) = utmod1 in
            raise (NotOfStructureType(rng1, modsig1))

        | ConcStructure(ssig1) ->
            begin
              match ssig1 |> StructSig.find_module modnm2 with
              | None ->
                  failwith "TODO (error): typecheck_module, UTModProjMod, not found"

              | Some(mentry2) ->
                  let absmodsig2 = (oidset, mentry2.mod_signature) in
                  (absmodsig2, binds)
            end
      end

  | UTModFunctor((_, _modnm1), _utsig1, _utmod2) ->
      failwith "TODO: typecheck_module, UTModFunctor"

  | UTModApply((_, _modnm1), (_, _modnm2)) ->
      failwith "TODO: typecheck_module, UTModApply"

  | UTModCoerce((_, _modnm1), _utsig2) ->
      failwith "TODO: typecheck_module, UTModCoerce"


and typecheck_signature (stage : stage) (tyenv : Typeenv.t) (utsig : untyped_signature) : signature abstracted =
  let (rng, utsigmain) = utsig in
  match utsigmain with
  | UTSigVar(signm) ->
      begin
        match tyenv |> Typeenv.find_signature signm with
        | None ->
            failwith "TODO (error): typecheck_signature, UTSigVar, not found"

        | Some(absmodsig) ->
            absmodsig
      end

  | UTSigPath(utmod1, (_, signm2)) ->
      let (absmodsig1, _binds1) = typecheck_module stage tyenv utmod1 in
      let (oidset1, modsig1) = absmodsig1 in
      begin
        match modsig1 with
        | ConcFunctor(_) ->
            let (rng1, _) = utmod1 in
            raise (NotOfStructureType(rng1, modsig1))

        | ConcStructure(ssig1) ->
            begin
              match ssig1 |> StructSig.find_signature signm2 with
              | None ->
                  failwith "TODO (error): typecheck_signature, UTSigPath, not found"

              | Some(absmodsig2) ->
                  let (_, modsig2) = absmodsig2 in
                  if opaque_occurs oidset1 modsig2 then
                    failwith "TODO (error): typecheck_signature, UTSigPath, extrusion"
                  else
                    absmodsig2
            end
      end

  | UTSigDecls(_utdecls) ->
      failwith "TODO: typecheck_signature, UTSigDecls"

  | UTSigFunctor((_, _modnm1), _utsig1, utsig2) ->
      failwith "TODO: typecheck_signature, UTSigFunctor"

  | UTSigWith(_utsig1, _modidents, _tyident, _typarams, _mnty) ->
      failwith "TODO: typecheck_signature, UTSigWith"


and coerce_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : signature abstracted =
  failwith "TODO: coerce_signature"


and add_to_type_environment_by_signature (ssig : StructSig.t) (tyenv : Typeenv.t) =
  ssig |> StructSig.fold
      ~v:(fun x vtup -> Typeenv.add_value x vtup)
      ~t:(fun tydefs tyenv ->
        tydefs |> List.fold_left (fun tyenv (tynm, tentry) ->
          tyenv |> Typeenv.add_type tynm tentry
        ) tyenv
      )
      ~m:(fun modnm mentry -> Typeenv.add_module modnm mentry)
      ~s:(fun signm absmodsig -> Typeenv.add_signature signm absmodsig)
      tyenv


and typecheck_binding_list (stage : stage) (tyenv : Typeenv.t) (utbinds : untyped_binding list) : binding list * Typeenv.t * StructSig.t abstracted =
  let (bindacc, tyenv, oidsetacc, ssigacc) =
    utbinds |> List.fold_left (fun (bindacc, tyenv, oidsetacc, ssigacc) utbind ->
      let (binds, (oidset, ssig)) = typecheck_binding stage tyenv utbind in
      let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
      let bindacc = Alist.append bindacc binds in
      let oidsetacc = OpaqueIDSet.union oidsetacc oidset in
      match StructSig.union ssigacc ssig with
      | Ok(ssigacc) -> (bindacc, tyenv, oidsetacc, ssigacc)
      | Error(s)    -> let (rng, _) = utbind in raise (ConflictInSignature(rng, s))
    ) (Alist.empty, tyenv, OpaqueIDSet.empty, StructSig.empty)
  in
  (Alist.to_list bindacc, tyenv, (oidsetacc, ssigacc))


and typecheck_binding (stage : stage) (tyenv : Typeenv.t) (utbind : untyped_binding) : binding list * StructSig.t abstracted =
  let (_, utbindmain) = utbind in
  match utbindmain with
  | UTBindValue(valbind) ->
      let pre =
        {
          stage           = stage;
          type_parameters = TypeParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in
      let (rec_or_nonrec, ssig) =
        match valbind with
        | UTNonRec(tyannot, utpat, utast1) ->
            let presub = { pre with level = Level.succ pre.level; } in
            let (pat, tyP, patvarmap) = typecheck_pattern presub tyenv utpat in
            let (e1, ty1) = typecheck presub tyenv utast1 in
            unify ty1 tyP;
            tyannot |> Option.map (fun mnty ->
              let tyA = decode_manual_type pre tyenv mnty in
              unify ty1 tyA
            ) |> ignore;
            let should_be_polymorphic = is_nonexpansive_expression e1 in
            let ssig =
              PatternVarMap.fold (fun varnm (_, evid, ty) ssig ->
                let pty =
                  if should_be_polymorphic then
                    generalize pre.level (erase_range_of_type ty)
                  else
                    lift_poly (erase_range_of_type ty)
                in
                let ventry =
                  {
                    val_type  = pty;
                    val_name  = evid;
                    val_stage = pre.stage;
                  }
                in
                ssig |> StructSig.add_value varnm ventry
              ) patvarmap StructSig.empty
            in
            (NonRec(pat, e1), ssig)

        | UTRec(utrecbinds) ->
            let quints = typecheck_letrec pre tyenv utrecbinds in
            let (recbindacc, ssig) =
              quints |> List.fold_left (fun (recbindacc, ssig) quint ->
                let (x, pty, evid, stage, recbind) = quint in
                let ssig =
                  let ventry =
                    {
                      val_type  = pty;
                      val_name  = evid;
                      val_stage = stage;
                    }
                  in
                  ssig |> StructSig.add_value x ventry
                in
                let recbindacc = Alist.extend recbindacc recbind in
                (recbindacc, ssig)
              ) (Alist.empty, StructSig.empty)
            in
            (Rec(recbindacc |> Alist.to_list), ssig)

        | UTMutable((rng, varnm) as var, utastI) ->
            let (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
            let evid = EvalVarID.fresh var in
            let pty = lift_poly (rng, RefType(tyI)) in
            let ssig =
              let ventry =
                {
                  val_type  = pty;
                  val_name  = evid;
                  val_stage = pre.stage;
                }
              in
              StructSig.empty |> StructSig.add_value varnm ventry
            in
            (Mutable(evid, eI), ssig)
      in
      ([ BindValue(rec_or_nonrec) ], (OpaqueIDSet.empty, ssig))

  | UTBindType([]) ->
      assert false

  | UTBindType(tybinds) ->
      (* Registers types to the type environment and the graph for detecting cyclic dependency. *)
      let (synacc, vntacc, vertices, graph, tyenv) =
        tybinds |> List.fold_left (fun (synacc, vntacc, vertices, graph, tyenv) tybind ->
          let (tyident, typarams, constraints, syn_or_vnt) = tybind in
          let (_, tynm) = tyident in
          let arity = List.length typarams in
          match syn_or_vnt with
          | UTBindSynonym(synbind) ->
              let sid = TypeID.Synonym.fresh tynm in
              let graph = graph |> SynonymDependencyGraph.add_vertex sid tyident in
              let tyenv =
                let tentry =
                  {
                    type_id    = TypeID.Synonym(sid);
                    type_arity = arity;
                  }
                in
                tyenv |> Typeenv.add_type tynm tentry
              in
              let synacc = Alist.extend synacc (tyident, typarams, synbind, sid) in
              (synacc, vntacc, vertices |> SynonymIDSet.add sid, graph, tyenv)

          | UTBindVariant(vntbind) ->
              let vid = TypeID.Variant.fresh tynm in
              let tyenv =
                let tentry =
                  {
                    type_id    = TypeID.Variant(vid);
                    type_arity = arity;
                  }
                in
                tyenv |> Typeenv.add_type tynm tentry in
              let vntacc = Alist.extend vntacc (tyident, typarams, vntbind, vid) in
              (synacc, vntacc, vertices, graph, tyenv)
        ) (Alist.empty, Alist.empty, SynonymIDSet.empty, SynonymDependencyGraph.empty, tyenv)
      in

      let pre =
        {
          stage           = stage;
          type_parameters = TypeParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in

      (* Traverse each definition of the synonym types and extract dependencies between them. *)
      let (graph, tydefacc) =
        synacc |> Alist.to_list |> List.fold_left (fun (graph, tydefacc) syn ->
          let (tyident, tyvars, synbind, sid) = syn in
          let (pre, bids) = make_type_parameters pre tyvars in
          let (ty_real, dependencies) = decode_manual_type_and_get_dependency vertices pre tyenv synbind in
          let pty_real = generalize Level.bottom ty_real in
          let graph =
            graph |> SynonymIDSet.fold (fun sid_dep graph ->
              graph |> SynonymDependencyGraph.add_edge sid sid_dep
            ) dependencies
          in
          TypeDefinitionStore.add_synonym_type sid bids pty_real;
          let tydefacc = Alist.extend tydefacc (tyident, TypeID.Synonym(sid), List.length bids) in
          (graph, tydefacc)
        ) (graph, Alist.empty)
      in

      (* Traverse each definition of the variant types. *)
      let tydefacc =
        vntacc |> Alist.to_list |> List.fold_left (fun tydefacc vnt ->
          let (tyident, tyvars, vntbind, vid) = vnt in
          let (pre, bids) = make_type_parameters pre tyvars in
          let ctorbrmap = make_constructor_branch_map pre tyenv vntbind in
          TypeDefinitionStore.add_variant_type vid bids ctorbrmap;
          Alist.extend tydefacc (tyident, TypeID.Variant(vid), List.length bids)
        ) tydefacc
      in

      (* Check that no cyclic dependency exists among synonym types. *)
      begin
        match SynonymDependencyGraph.find_cycle graph with
        | Some(_cycle) ->
            failwith "TODO (error): Typechecker.typecheck_binding, UTBindType, cycle"

        | None ->
            let tydefs =
              tydefacc |> Alist.to_list |> List.map (fun ((_, tynm), tyid, arity) ->
                let tentry =
                  {
                    type_id    = tyid;
                    type_arity = arity;
                  }
                in
                (tynm, tentry)
              )
            in
            let ssig = StructSig.empty |> StructSig.add_types tydefs in
            ([], (OpaqueIDSet.empty, ssig))
      end

  | UTBindModule((rngm, modnm), utsigopt2, utmod1) ->
      let (absmodsig1, binds1) = typecheck_module stage tyenv utmod1 in
      let (oidset, modsig) =
        match utsigopt2 with
        | None ->
            absmodsig1

        | Some(utsig2) ->
            let (_, modsig1) = absmodsig1 in
            let absmodsig2 = typecheck_signature stage tyenv utsig2 in
            coerce_signature rngm modsig1 absmodsig2
      in
      let mid = ModuleID.fresh modnm in
      let ssig =
        let mentry =
          {
            mod_signature = modsig;
            mod_name      = mid;
          }
        in
        StructSig.empty |> StructSig.add_module modnm mentry
      in
      ([ BindModule(mid, binds1) ], (oidset, ssig))

  | UTBindSignature((_, signm), utsig) ->
      let absmodsig = typecheck_signature stage tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      ([], (OpaqueIDSet.empty, ssig))

  | UTBindInclude(utmod) ->
      let (absmodsig, binds) = typecheck_module stage tyenv utmod in
      let (oidset, modsig) = absmodsig in
      begin
        match modsig with
        | ConcStructure(ssig) ->
            (binds, (oidset, ssig))

        | ConcFunctor(_) ->
            failwith "TODO (error): Typechecker.typecheck_binding, UTBindInclude, not a structure"
      end

  | UTBindOpen((_, _modnm)) ->
      failwith "TODO: Typechecker.typecheck_binding, UTBindOpen"

  | UTBindHorzMacro((_, _csnm), _macparams, _utast1) ->
      failwith "TODO: Typechecker.typecheck_binding, UTBindHorzMacro"

  | UTBindVertMacro((_, _csnm), _macparams, _utast2) ->
      failwith "TODO: Typechecker.typecheck_binding, UTBindVertMacro"


let main (stage : stage) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) : mono_type * abstract_tree =
  let pre =
    {
      stage           = stage;
      type_parameters = TypeParameterMap.empty;
      quantifiability = Quantifiable;
      level           = Level.bottom;
    }
  in
  let (e, ty) = typecheck pre tyenv utast in
  (ty, e)


let main_bindings (stage : stage) (tyenv : Typeenv.t) (utbinds : untyped_binding list) : binding list * Typeenv.t =
  let (binds, tyenv, _) = typecheck_binding_list stage tyenv utbinds in
  (binds, tyenv)


let are_unifiable (ty1 : mono_type) (ty2 : mono_type) : bool =
  try
    unify_sub ~reversed:false ty1 ty2;
    true
  with
  | InternalContradictionError(_) -> false
  | InternalInclusionError        -> false
