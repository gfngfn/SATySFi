
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
exception NotAStructureSignature         of Range.t
exception MissingRequiredValueName       of Range.t * var_name * poly_type
exception MissingRequiredTypeName        of Range.t * type_name * int
exception MissingRequiredModuleName      of Range.t * module_name * signature
exception MissingRequiredSignatureName   of Range.t * signature_name * signature abstracted
exception NotASubtypeAboutType           of Range.t * type_name
exception NotASubtype                    of Range.t * signature * signature
exception PolymorphicContradiction       of Range.t * var_name * poly_type * poly_type
exception MultiCharacterMathScriptWithoutBrace of Range.t

exception InternalInclusionError
exception InternalContradictionError of bool


module SubstMap = Map.Make(TypeID)

type substitution = type_scheme SubstMap.t

module SynonymNameSet = Set.Make(String)


let fresh_free_id (qtfbl : quantifiability) (lev : Level.t) : FreeID.t =
  let fid = FreeID.fresh () in
  let fentry =
    KindStore.{
      level           = lev;
      quantifiability = qtfbl;
      mono_kind       = UniversalKind;
    }
  in
  KindStore.set_free_id fid fentry;
  fid


let fresh_free_row_id (lev : Level.t) (labset : LabelSet.t) : FreeRowID.t =
  let frid = FreeRowID.fresh lev in
  KindStore.register_free_row_id frid labset;
  frid


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
      let orfreef frid =
        RowVar(frid)
      in
      let tyid = centry.ctor_belongs_to in
      let (bids, pty) = centry.ctor_parameter in
      let pairs =
        bids |> List.map (fun bid ->
          let fid = fresh_free_id qtfbl lev in
          let tv = Updatable(ref (MonoFree(fid))) in
          let ty = (Range.dummy "tc-constructor", TypeVariable(tv)) in
          (ty, bid)
        )
      in
      let ty = instantiate_type_scheme freef orfreef pairs pty in
      let tyargs = pairs |> List.map (fun (ty, _) -> ty) in
      (tyargs, tyid, ty)


let abstraction (evid : EvalVarID.t) (ast : abstract_tree) : abstract_tree =
  Function(LabelMap.empty, PatternBranch(PVariable(evid), ast))


let abstraction_list (evids : EvalVarID.t list) (ast : abstract_tree) : abstract_tree =
  List.fold_right abstraction evids ast


let add_optionals_to_type_environment (tyenv : Typeenv.t) (pre : pre) (optargs : (label ranged * var_name) list) : mono_row * EvalVarID.t LabelMap.t * Typeenv.t =
  let qtfbl = pre.quantifiability in
  let lev = pre.level in
  let (tyenv, row, evid_labmap) =
    optargs |> List.fold_left (fun (tyenv, row, evid_labmap) (rlabel, varnm) ->
      let (rng, label) = rlabel in
      let evid = EvalVarID.fresh (rng, varnm) in
      let fid = fresh_free_id qtfbl lev in
      let tv = Updatable(ref (MonoFree(fid))) in
      let beta = (rng, TypeVariable(PolyFree(tv))) in
      let tyenv =
        let ventry =
          {
            val_name  = evid;
            val_type  = Poly(Primitives.option_type beta);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry
      in
      (tyenv, RowCons(rlabel, (rng, TypeVariable(tv)), row), evid_labmap |> LabelMap.add label evid)
    ) (tyenv, RowEmpty, LabelMap.empty)
  in
  (row, evid_labmap, tyenv)


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
        let tvid = fresh_free_id pre.quantifiability pre.level in
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
  | Record(asc)                       -> asc |> LabelMap.for_all (fun _label e -> iter e)
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


(* `apply_tree_of_list` converts `e0` and `[e1; ...; eN]` into `(e0 e1 ... eN)`. *)
let apply_tree_of_list (astfunc : abstract_tree) (asts : abstract_tree list) =
  List.fold_left (fun astf astx -> Apply(LabelMap.empty, astf, astx)) astfunc asts


(* `flatten_type` converts type `(t1 -> ... -> tN -> t)` into `([t1; ...; tN], t)`. *)
let flatten_type (ty : mono_type) : mono_command_argument_type list * mono_type =
  let rec aux_row (tylabmap : mono_type LabelMap.t) = function
    | RowEmpty                                         -> tylabmap
    | RowVar(UpdatableRow{contents = MonoORFree(_)})   -> tylabmap
    | RowVar(UpdatableRow{contents = MonoORLink(row)}) -> aux_row tylabmap row
    | RowCons((_, label), ty, row)                     -> aux_row (tylabmap |> LabelMap.add label ty) row
  in
  let rec aux acc ty =
    let (rng, tymain) = ty in
      match tymain with
      | TypeVariable(Updatable{contents = MonoLink(tysub)}) ->
          aux acc tysub

      | FuncType(optrow, tydom, tycod) ->
          let tylabmap = aux_row LabelMap.empty optrow in
          let acc = Alist.extend acc (CommandArgType(tylabmap, tydom)) in
          aux acc tycod

      | _ ->
          (Alist.to_list acc, ty)
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
                      false
              end

          | MustBeBound(_) ->
              false
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_row optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tys)               -> iter_list (tys |> TupleList.to_list)
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub
    | DataType(tyargs, _tyid)        -> iter_list tyargs
    | RecordType(row)                -> iter_row row
    | BaseType(_)                    -> false
    | HorzCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | VertCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | MathCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | CodeType(tysub)                -> iter tysub

  and iter_list tylst =
    List.exists iter tylst

  and iter_cmd_list (cmdargtys : mono_command_argument_type list) =
    List.exists (function
    | CommandArgType(tylabmap, ty) -> tylabmap |> LabelMap.for_all (fun _ -> iter) && iter ty
    ) cmdargtys

  and iter_row = function
    | RowEmpty ->
        false

    | RowCons(_, ty, tail) ->
        let b1 = iter ty in
        let b2 = iter_row tail in
        b1 || b2

    | RowVar(UpdatableRow(orvuref)) ->
        begin
          match !orvuref with
          | MonoORLink(row) ->
              iter_row row

          | MonoORFree(frid0) ->
              let lev0 = FreeRowID.get_level frid0 in
              if Level.less_than lev lev0 then FreeRowID.set_level frid0 lev;
              false
        end
  in
  iter ty


let occurs_row (frid : FreeRowID.t) (row : mono_row) =

  let lev = FreeRowID.get_level frid in

  let rec iter ((_, tymain) : mono_type) =
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
                    false
              end

          | MustBeBound(_) ->
              false
        end

    | FuncType(optrow, tydom, tycod) ->
        let b0 = iter_row optrow in
        let b1 = iter tydom in
        let b2 = iter tycod in
        b0 || b1 || b2

    | ProductType(tys)               -> iter_list (tys |> TupleList.to_list)
    | ListType(tysub)                -> iter tysub
    | RefType(tysub)                 -> iter tysub
    | DataType(tyargs, _tyid)        -> iter_list tyargs
    | RecordType(row)                -> iter_row row
    | BaseType(_)                    -> false
    | HorzCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | VertCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | MathCommandType(cmdargtys)     -> iter_cmd_list cmdargtys
    | CodeType(tysub)                -> iter tysub

  and iter_list (tys : mono_type list) =
    List.exists iter tys

  and iter_cmd_list (cmdargtys : mono_command_argument_type list) =
    List.exists (function
      | CommandArgType(tylabmap, ty) -> tylabmap |> LabelMap.for_all (fun _label -> iter) && iter ty
    ) cmdargtys

  and iter_row = function
    | RowEmpty ->
        false

    | RowCons(_, ty, tail) ->
        let b1 = iter ty in
        let b2 = iter_row tail in
        b1 || b2

    | RowVar(UpdatableRow(orvuref)) ->
        begin
          match !orvuref with
          | MonoORLink(row) ->
              iter_row row

          | MonoORFree(frid0) ->
              if FreeRowID.equal frid frid0 then
                true
              else
                let lev0 = FreeRowID.get_level frid0 in
                if Level.less_than lev lev0 then FreeRowID.set_level frid0 lev;
                false
        end

  in
  iter_row row


let set_kind (fid : FreeID.t) (kd : kind) : unit =
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

    | (BaseType(bsty1), BaseType(bsty2))  when bsty1 = bsty2 -> ()

    | (FuncType(optrow1, tydom1, tycod1), FuncType(optrow2, tydom2, tycod2)) ->
        begin
          unify_row ~reversed optrow1 optrow2;
          unify tydom1 tydom2;
          unify tycod1 tycod2;
        end

    | (HorzCommandType(cmdargtys1), HorzCommandType(cmdargtys2))
    | (VertCommandType(cmdargtys1), VertCommandType(cmdargtys2))
    | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2)) ->
        begin
          try
            List.iter2 (fun cmdargty1 cmdargty2 ->
              match (cmdargty1, cmdargty2) with
              | (CommandArgType(tylabmap1, ty1), CommandArgType(tylabmap2, ty2)) ->
                  LabelMap.merge (fun label tyopt1 tyopt2 ->
                    match (tyopt1, tyopt2) with
                    | (Some(ty1), Some(ty2)) -> Some(unify ty1 ty2)
                    | (_, None) | (None, _)  -> raise (InternalContradictionError(reversed))
                  ) tylabmap1 tylabmap2 |> ignore;
                  unify ty1 ty2
            ) cmdargtys1 cmdargtys2
          with
          | Invalid_argument(_) ->
              raise (InternalContradictionError(reversed))
        end

    | (ProductType(tys1), ProductType(tys2)) ->
        unify_list (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

    | (RecordType(row1), RecordType(row2)) ->
        unify_row ~reversed row1 row2

    | (DataType(tyargs1, tyid1), DataType(tyargs2, tyid2)) ->
        if TypeID.equal tyid1 tyid2 then
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
            set_kind newfid UniversalKind

      | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)), _) ->
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


and solve_row_disjointness (row : mono_row) (labset : LabelSet.t) =
  match row with
  | RowCons((rng, label), ty, rowsub) ->
      if labset |> LabelSet.mem label then
        failwith "TODO (error): should report error about label"
      else
        solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoORLink(rowsub)}) ->
      solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoORFree(frid0)}) ->
      let labset0 = KindStore.get_free_row_id frid0 in
      KindStore.register_free_row_id frid0 (LabelSet.union labset0 labset);
      ()

  | RowEmpty ->
      ()


and solve_row_membership ~reversed (rng : Range.t) (label : label) (ty : mono_type) (row : mono_row) : mono_row =
  match row with
  | RowCons((rng0, label0), ty0, row0) ->
      if String.equal label0 label then begin
        unify_sub ~reversed ty0 ty;
        row0
      end else
        let row0rest = solve_row_membership ~reversed rng label ty row0 in
        RowCons((rng0, label0), ty0, row0rest)

  | RowVar(UpdatableRow{contents = MonoORLink(row0)}) ->
      solve_row_membership ~reversed rng label ty row0

  | RowVar(UpdatableRow({contents = MonoORFree(frid0)} as orvuref0)) ->
      let labset0 = KindStore.get_free_row_id frid0 in
      if labset0 |> LabelSet.mem label then
        failwith "TODO (error): reject for the disjointness"
      else begin
        let lev0 = FreeRowID.get_level frid0 in
        let orv1 = FreeRowID.fresh lev0 in
        KindStore.register_free_row_id orv1 LabelSet.empty;
        let orvuref1 = ref (MonoORFree(orv1)) in
        let row_rest = RowVar(UpdatableRow(orvuref1)) in
        let row_new = RowCons((rng, label), ty, row_rest) in
        orvuref0 := MonoORLink(row_new);
        row_rest
      end

  | RowEmpty ->
      failwith "TODO (error): solve_membership_aux, RowEmpty"


and unify_row ~reversed (row1 : mono_row) (row2 : mono_row) =
  match (row1, row2) with
  | (RowVar(UpdatableRow{contents = MonoORLink(row1sub)}), _) ->
      unify_row ~reversed row1sub row2

  | (_, RowVar(UpdatableRow{contents = MonoORLink(row2sub)})) ->
      unify_row ~reversed row1 row2sub

  | (RowVar(UpdatableRow({contents = MonoORFree(orv1)} as orviref1)), RowVar(UpdatableRow{contents = MonoORFree(orv2)})) ->
      if FreeRowID.equal orv1 orv2 then
        ()
      else
        orviref1 := MonoORLink(row2)

  | (RowVar(UpdatableRow({contents = MonoORFree(orv1)} as orviref1)), _) ->
      if occurs_row orv1 row2 then
        raise InternalInclusionError
      else begin
        let labset1 = KindStore.get_free_row_id orv1 in
        solve_row_disjointness row2 labset1;
        orviref1 := MonoORLink(row2)
      end

  | (_, RowVar(UpdatableRow({contents = MonoORFree(orv2)} as orviref2))) ->
      if occurs_row orv2 row1 then
        raise InternalInclusionError
      else begin
        let labset2 = KindStore.get_free_row_id orv2 in
        solve_row_disjointness row1 labset2;
        orviref2 := MonoORLink(row1)
      end

  | (RowCons((rng, label), ty1, row1sub), _) ->
      let row2rest = solve_row_membership ~reversed rng label ty1 row2 in
      unify_row ~reversed row1sub row2rest

  | (RowEmpty, RowEmpty) ->
      ()

  | (RowEmpty, RowCons(_, _, _)) ->
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


let fresh_type_variable (rng : Range.t) (pre : pre) : mono_type =
  let fid = fresh_free_id pre.quantifiability pre.level in
  let tvuref = ref (MonoFree(fid)) in
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
      let (tyargs, tyid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 tyc;
      let tyres = (rng, DataType(tyargs, tyid)) in
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

  | UTApply(opts, utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let (e_labmap, row0) =
        let frid = FreeRowID.fresh pre.level in
        let orvuref = ref (MonoORFree(frid)) in
        opts |> List.fold_left (fun (e_labmap, row) (rlabel, utast0) ->
          let (_, label) = rlabel in
          let (e0, ty0) = typecheck_iter tyenv utast0 in
          (e_labmap |> LabelMap.add label e0, RowCons(rlabel, ty0, row))
        ) (LabelMap.empty, RowVar(UpdatableRow(orvuref)))
      in
      let eret = Apply(e_labmap, e1, e2) in
      begin
        match unlink ty1 with
        | (_, FuncType(row, tydom, tycod)) ->
            unify_row ~reversed:false row0 row;
            unify ty2 tydom;
            let tycodnew = overwrite_range_of_type tycod rng in
            (eret, tycodnew)

        | (_, TypeVariable(_)) as ty1 ->
            let beta = fresh_type_variable rng pre in
            unify ty1 (get_range utast1, FuncType(row0, ty2, beta));
            (eret, beta)

        | ty1 ->
            let (rng1, _) = utast1 in
            raise (ApplicationOfNonFunction(rng1, ty1))
      end

  | UTFunction(optargs, pat, utast1) ->
      let utpatbr = UTPatternBranch(pat, utast1) in
      let (optrow, evid_labmap, tyenv) = add_optionals_to_type_environment tyenv pre optargs in
      let (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
      (Function(evid_labmap, patbr), (rng, FuncType(optrow, typat, ty1)))

  | UTPatternMatch(utastO, utpatbrs) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let beta = fresh_type_variable (Range.dummy "ut-pattern-match") pre in
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
      let beta = fresh_type_variable rng pre in
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

  | UTAccessField(utast1, label) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let beta = fresh_type_variable rng pre in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoORFree(frid)) in
        RowCons((Range.dummy "UTAccessField", label), beta, RowVar(UpdatableRow(rvuref)))
      in
      unify ty1 (Range.dummy "UTAccessField", RecordType(row));
      (AccessField(e1, label), beta)

  | UTUpdateField(utast1, label, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoORFree(frid)) in
        RowCons((Range.dummy "UTUpdateField", label), ty2, RowVar(UpdatableRow(rvuref)))
      in
      unify ty1 (Range.dummy "UTUpdateField", RecordType(row));
      (UpdateField(e1, label, e2), ty1)

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
            let beta = fresh_type_variable rng pre in
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
  failwith "TODO: typecheck_command_arguments"
(*
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
        aux (Apply(LabelMap.empty, eacc, eA)) utcmdargtail cmdargtytail

    | (UTOptionalArgument(utastA) :: utcmdargtail, OptionalArgumentType(rlabel, tyreq) :: cmdargtytail) ->
        failwith "TODO: fix this for using rlabel"
(*
        let (eA, tyA) = typecheck pre tyenv utastA in
        unify tyA tyreq;
        aux (ApplyOptional(eacc, eA)) utcmdargtail cmdargtytail
*)
    | (UTOptionalArgument((rngA, _)) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tycmd, rngA))

    | (UTOmission(_) :: utcmdargtail, OptionalArgumentType(rlabel, tyreq) :: cmdargtytail) ->
        failwith "TODO: fix this for using rlabel"
(*
        aux (ApplyOmission(eacc)) utcmdargtail cmdargtytail
*)

    | (UTOmission(rngA) :: _, MandatoryArgumentType(_) :: _) ->
        raise (InvalidOptionalCommandArgument(tycmd, rngA))
  in
  aux ecmd utcmdarglst cmdargtylst
*)

and typecheck_math (pre : pre) tyenv ((rng, utmathmain) : untyped_math) : abstract_tree =
  let iter = typecheck_math pre tyenv in
  let check_brace (has_braceS : bool) (utmathS : untyped_math) : unit =
    match (has_braceS, utmathS) with
    | (true, _) ->
        ()

    | (false, (rng, UTMChars(uchs))) ->
        if List.length uchs >= 2 then
          raise (MultiCharacterMathScriptWithoutBrace(rng))
        else
          Logging.warn_math_script_without_brace rng

    | (false, (rng, _)) ->
        Logging.warn_math_script_without_brace rng
  in
  let open HorzBox in
    match utmathmain with
    | UTMChars(uchs) ->
        let ms = uchs |> List.map (fun uch -> MathPure(MathVariantChar(uch))) in
        ASTMath(ms)

    | UTMList(utmathlst) ->
        let astlst = utmathlst |> List.map iter in
        BackendMathList(astlst)

    | UTMSubScript(utmathB, has_braceS, utmathS) ->
        check_brace has_braceS utmathS;
        let astB = iter utmathB in
        let astS = iter utmathS in
        BackendMathSubscript(astB, astS)

    | UTMSuperScript(utmathB, has_braceS, utmathS) ->
        check_brace has_braceS utmathS;
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
                | []                             -> rngcmd
                | UTCommandArg(_, (rng, _)) :: _ -> Range.unite rngcmd rng
              in
              let evid = EvalVarID.fresh (Range.dummy "ctx-vert", "%ctx-vert") in
              let ecmdctx = Apply(LabelMap.empty, ecmd, ContentOf(Range.dummy "ctx-vert", evid)) in
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
          | []                             -> rngcmd
          | UTCommandArg(_, (rng, _)) :: _ -> Range.unite rngcmd rng
        in
        let (ecmd, tycmd) = typecheck pre tyenv utastcmd in
        let (_, tycmdmain) = tycmd in
        begin
          match tycmdmain with
          | HorzCommandType(cmdargtylstreq) ->
              let evid = EvalVarID.fresh (Range.dummy "ctx-horz", "%ctx-horz") in
              let ecmdctx = Apply(LabelMap.empty, ecmd, ContentOf(Range.dummy "ctx-horz", evid)) in
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


and typecheck_record (pre : pre) (tyenv : Typeenv.t) (fluts : (field_name * untyped_abstract_tree) list) (rng : Range.t) =
  let (easc, row) =
    fluts |> List.fold_left (fun (easc, row) (fldnmX, utastX) ->
      if easc |> LabelMap.mem fldnmX then
        raise (MultipleFieldInRecord(rng, fldnmX))
      else
        let rngX = failwith "TODO: typecheck_record, range of field" in
        let (eX, tyX) = typecheck pre tyenv utastX in
        (easc |> LabelMap.add fldnmX eX, RowCons((rngX, fldnmX), tyX, row))
    ) (LabelMap.empty, RowEmpty)
  in
  (Record(easc), (rng, RecordType(row)))


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
        let beta = fresh_type_variable rng pre in
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
        let beta = fresh_type_variable rng pre in
        (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let beta = fresh_type_variable rng pre in
        let evid = EvalVarID.fresh (rng, varnm) in
        (PVariable(evid), beta, PatternVarMap.empty |> PatternVarMap.add varnm (rng, evid, beta))

    | UTPAsVariable(varnm, utpat1) ->
        let beta = fresh_type_variable rng pre in
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
        let (tyargs, tyid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
        let (epat1, typat1, tyenv1) = iter utpat1 in
        unify tyc typat1;
        (PConstructor(constrnm, epat1), (rng, DataType(tyargs, tyid)), tyenv1)


and typecheck_letrec (pre : pre) (tyenv : Typeenv.t) (utrecbinds : untyped_letrec_binding list) : (var_name * poly_type * EvalVarID.t * stage * letrec_binding) list =

  (* First, adds a type variable for each bound identifier. *)
  let (tyenv, utrecacc) =
    utrecbinds |> List.fold_left (fun (tyenv, utrecacc) utrecbind ->
      let UTLetRecBinding(_, varrng, varnm, astdef) = utrecbind in
      let tvuref =
        let tvid = fresh_free_id pre.quantifiability (Level.succ pre.level) in
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
        | Function(evid_labmap, patbr1) ->
            if LabelMap.cardinal evid_labmap = 0 then begin
              unify ty1 beta;
              mntyopt |> Option.map (fun mnty ->
                let tyin = decode_manual_type pre tyenv mnty in
                unify tyin beta
              ) |> Option.value ~default:();
              let recbind = LetRecBinding(evid, patbr1) in
              let tupleacc = Alist.extend tupleacc (varnm, beta, evid, recbind) in
              tupleacc
            end else
            let (rng1, _) = utast1 in
            raise (BreaksValueRestriction(rng1))

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


and decode_manual_type (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : mono_type =
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
                begin
                  match TypeConv.apply_type_scheme_mono tentry.type_scheme tyargs with
                  | Some((_, tymain)) ->
                      tymain

                  | None ->
                      let (bids, _) = tentry.type_scheme in
                      let len_expected = List.length bids in
                      invalid rng tynm ~expect:len_expected ~actual:len_actual
                end
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

      | MRecordType(mnkvs) ->
          failwith "TODO: decode_manual_type, MRecordType"
(*
          RecordType(Assoc.map_value aux mnasc)
*)

      | MHorzCommandType(mncmdargtys) -> HorzCommandType(aux_cmd_list mncmdargtys)
      | MVertCommandType(mncmdargtys) -> VertCommandType(aux_cmd_list mncmdargtys)
      | MMathCommandType(mncmdargtys) -> MathCommandType(aux_cmd_list mncmdargtys)

    in
    (rng, tymain)

  and aux_cmd_list (mncmdargtys : manual_command_argument_type list) =
    failwith "TODO: decode_manual_type, aux_cmd_list"

  and aux_row (mtyadds : manual_type list) =
    failwith "TODO: decode_manual_type, aux_row"
(*
    List.fold_right (fun mty row -> OptionRowCons(aux mty, row)) mtyadds OptionRowEmpty
*)
  in
  aux mty


and decode_manual_kind (pre : pre) (tyenv : Typeenv.t) (mnkd : manual_kind) : kind =
  failwith "TODO: decode_manual_kind"


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

  | UTModFunctor((_, modnm1), utsig1, utmod2) ->
      let absmodsig1 = typecheck_signature stage tyenv utsig1 in
      let (oidset, modsig1) = absmodsig1 in
      begin
        match modsig1 with
        | ConcFunctor(_) ->
            failwith "TODO (error): typecheck_module, UTModFunctor"

        | ConcStructure(ssig1) ->
            let (absmodsig2, _binds) =
              let mentry1 =
                {
                  mod_name      = ModuleID.fresh modnm1;
                  mod_signature = modsig1;
                }
              in
              let tyenv = tyenv |> Typeenv.add_module modnm1 mentry1 in
              typecheck_module stage tyenv utmod2
            in
            let fsig =
              {
                opaques  = oidset;
                domain   = ssig1;
                codomain = absmodsig2;
              }
            in
            let absmodsig = (OpaqueIDSet.empty, ConcFunctor(fsig)) in
            (absmodsig, [])
      end

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

  | UTSigDecls(utdecls) ->
      let (oidset, ssig) = typecheck_declaration_list stage tyenv utdecls in
      (oidset, ConcStructure(ssig))

  | UTSigFunctor((_, _modnm1), _utsig1, utsig2) ->
      failwith "TODO: typecheck_signature, UTSigFunctor"

  | UTSigWith(_utsig1, _modidents, _tyident, _typarams, _mnty) ->
      failwith "TODO: typecheck_signature, UTSigWith"


and lookup_type_entry (tentry1 : type_entry) (tentry2 : type_entry) : substitution option =
  failwith "TODO: lookup_type_entry"


and lookup_struct (rng : Range.t) (modsig1 : signature) (modsig2 : signature) : substitution =
  let take_left = (fun _tyid to1 _to2 -> Some(to1)) in
  match (modsig1, modsig2) with
  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun _x2 _ventry2 subst ->
            subst
          )
          ~t:(fun tynm2 tentry2 subst ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let (bids, _) = tentry2.type_scheme in
                raise (MissingRequiredTypeName(rng, tynm2, List.length bids))

            | Some(tentry1) ->
                begin
                  match lookup_type_entry tentry1 tentry2 with
                  | None ->
                      raise (NotASubtypeAboutType(rng, tynm2))

                  | Some(subst0) ->
                      SubstMap.union take_left subst0 subst
                end
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } subst ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                raise (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some({ mod_signature = modsig1; _ }) ->
                let subst0 = lookup_struct rng modsig1 modsig2 in
                SubstMap.union take_left subst0 subst
          )
          ~s:(fun _ _ subst ->
            subst
          )
          SubstMap.empty

  | _ ->
      SubstMap.empty


and substitute_abstract (subst : substitution) (absmodsig : signature abstracted) : signature abstracted =
  let (oidset, modsig) = absmodsig in
  let modsig = substitute_concrete subst modsig in
  (oidset, modsig)
    (* Strictly speaking, we should assert that `oidset` and the domain of `wtmap` be disjoint. *)


and substitute_concrete (subst : substitution) (modsig : signature) : signature =
  match modsig with
  | ConcFunctor(fsig) ->
      let
        {
          opaques = oidset;
          domain  = ssig;
          codomain = absmodsigcod
        } = fsig
      in
      let ssig = ssig |> substitute_struct subst in
      let absmodsigcod = absmodsigcod |> substitute_abstract subst in
      let fsig =
        {
          opaques  = oidset;
          domain   = ssig;
          codomain = absmodsigcod;
        }
      in
      ConcFunctor(fsig)
        (* Strictly speaking, we should assert that `oidset` and the domain of `wtmap` be disjoint. *)

  | ConcStructure(ssig) ->
      let ssig = ssig |> substitute_struct subst in
      ConcStructure(ssig)


and substitute_poly_type (subst : substitution) (Poly(pty) : poly_type) : poly_type =
  let rec aux (rng, ptymain) =
    let ptymain =
      match ptymain with
      | BaseType(bt)  -> BaseType(bt)
      | ListType(pty) -> ListType(aux pty)
      | RefType(pty)  -> RefType(aux pty)
      | CodeType(pty) -> CodeType(aux pty)

      | HorzCommandType(pargs) -> HorzCommandType(pargs |> List.map aux_command_arg)
      | VertCommandType(pargs) -> HorzCommandType(pargs |> List.map aux_command_arg)
      | MathCommandType(pargs) -> HorzCommandType(pargs |> List.map aux_command_arg)

      | FuncType(poptrow, ptydom, ptycod) ->
          FuncType(aux_option_row poptrow, aux ptydom, aux ptycod)

      | ProductType(ptys) ->
          ProductType(ptys |> TupleList.map aux)

      | TypeVariable(ptv) ->
          TypeVariable(ptv)

      | RecordType(row) ->
          RecordType(aux_option_row row)

      | DataType(ptyargs, tyid_from) ->
          begin
            match subst |> SubstMap.find_opt tyid_from with
            | None ->
                DataType(ptyargs |> List.map aux, tyid_from)

            | Some(tyscheme) ->
                begin
                  match TypeConv.apply_type_scheme_poly tyscheme (ptyargs |> List.map aux) with
                  | None ->
                      (* arity mismatch; this cannot happen *)
                      assert false

                  | Some((_, ptymain)) ->
                      ptymain
                end
          end
    in
    (rng, ptymain)

  and aux_option_row = function
    | RowCons(rlabel, pty, poptrow) -> RowCons(rlabel, aux pty, aux_option_row poptrow)
    | RowEmpty                      -> RowEmpty
    | RowVar(prv)                   -> RowVar(prv)

  and aux_command_arg = function
    | CommandArgType(ptylabmap, pty) -> CommandArgType(ptylabmap |> LabelMap.map aux, aux pty)
  in
  Poly(aux pty)


and substitute_struct (subst : substitution) (ssig : StructSig.t) : StructSig.t =
  ssig |> StructSig.map
      ~v:(fun _x ventry ->
        { ventry with val_type = ventry.val_type |> substitute_poly_type subst }
      )
      ~t:(fun _tynm tentry ->
        let (bids, pty) = tentry.type_scheme in
        {
          type_scheme = (bids, pty |> substitute_poly_type subst);
          type_kind   = tentry.type_kind;
        }
      )
      ~m:(fun _modnm mentry ->
        let modsig = mentry.mod_signature |> substitute_concrete subst in
        { mentry with mod_signature = modsig }
      )
      ~s:(fun _signm absmodsig ->
        absmodsig |> substitute_abstract subst
      )


and subtype_abstract_with_abstract (rng : Range.t) (absmodsig1 : signature abstracted) (absmodsig2 : signature abstracted) : unit =
  let (_, modsig1) = absmodsig1 in
  let _ = subtype_concrete_with_abstract rng modsig1 absmodsig2 in
  ()


and subtype_concrete_with_concrete (rng : Range.t) (modsig1 : signature) (modsig2 : signature) =
  match (modsig1, modsig2) with
  | (ConcFunctor(fsig1), ConcFunctor(fsig2)) ->
      let
        {
          opaques  = oidset1;
          domain   = ssig1;
          codomain = absmodsigcod1;
        } = fsig1
      in
      let
        {
          opaques  = oidset2;
          domain   = ssig2;
          codomain = absmodsigcod2;
        } = fsig2
      in
      let subst =
        let modsigdom1 = ConcStructure(ssig1) in
        let modsigdom2 = ConcStructure(ssig2) in
        subtype_concrete_with_abstract rng modsigdom2 (oidset1, modsigdom1)
      in
      let absmodsigcod1 = absmodsigcod1 |> substitute_abstract subst in
      subtype_abstract_with_abstract rng absmodsigcod1 absmodsigcod2

  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun x2 { val_type = pty2; _ } () ->
            match ssig1 |> StructSig.find_value x2 with
            | None ->
                raise (MissingRequiredValueName(rng, x2, pty2))

            | Some({ val_type = pty1; _ }) ->
               if subtype_poly_type pty1 pty2 then
                 ()
               else
                 raise (PolymorphicContradiction(rng, x2, pty1, pty2))
          )
          ~t:(fun tynm2 tentry2 () ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let arity =
                  let (bids, _) = tentry2.type_scheme in
                  List.length bids
                in
                raise (MissingRequiredTypeName(rng, tynm2, arity))

            | Some(tentry1) ->
                let tyscheme1 = tentry1.type_scheme in
                let tyscheme2 = tentry2.type_scheme in
                subtype_type_scheme tyscheme1 tyscheme2;
                subtype_type_scheme tyscheme2 tyscheme1;
                ()
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } () ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                raise (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some({ mod_signature = modsig1; _ }) ->
                subtype_concrete_with_concrete rng modsig1 modsig2
          )
          ~s:(fun signm2 absmodsig2 () ->
            match ssig1 |> StructSig.find_signature signm2 with
            | None ->
                raise (MissingRequiredSignatureName(rng, signm2, absmodsig2))

            | Some(absmodsig1) ->
                subtype_abstract_with_abstract rng absmodsig1 absmodsig2;
                subtype_abstract_with_abstract rng absmodsig2 absmodsig1;
                ()
          )
          ()

  | _ ->
      raise (NotASubtype(rng, modsig1, modsig2))


and subtype_concrete_with_abstract (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : substitution =
  let (oidset2, modsig2) = absmodsig2 in
  let subst = lookup_struct rng modsig1 modsig2 in
  let modsig2 = modsig2 |> substitute_concrete subst in
  subtype_concrete_with_concrete rng modsig1 modsig2;
  subst


and subtype_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) =
  subtype_concrete_with_abstract rng modsig1 absmodsig2


and subtype_type_scheme (tyscheme1 : type_scheme) (tyscheme2 : type_scheme) =
  failwith "TODO: subtype_type_scheme"


and copy_contents (modsig1 : signature) (modsig2 : signature) =
  failwith "TODO: copy_contents"


and coerce_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : signature abstracted =
  let _ = subtype_signature rng modsig1 absmodsig2 in
  let (oidset2, modsig2) = absmodsig2 in
  (oidset2, copy_contents modsig1 modsig2)


and add_to_type_environment_by_signature (ssig : StructSig.t) (tyenv : Typeenv.t) =
  ssig |> StructSig.fold
      ~v:(fun x ventry -> Typeenv.add_value x ventry)
      ~t:(fun tynm tentry -> Typeenv.add_type tynm tentry)
      ~m:(fun modnm mentry -> Typeenv.add_module modnm mentry)
      ~s:(fun signm absmodsig -> Typeenv.add_signature signm absmodsig)
      tyenv


and typecheck_declaration_list (stage : stage) (tyenv : Typeenv.t) (utdecls : untyped_declaration list) : OpaqueIDSet.t * StructSig.t =
  let (oidsetacc, ssigacc, _) =
    utdecls |> List.fold_left (fun (oidsetacc, ssigacc, tyenv) utdecl ->
      let (oidset, ssig) = typecheck_declaration stage tyenv utdecl in
      let oidsetacc = OpaqueIDSet.union oidsetacc oidset in
      let ssigacc =
        match StructSig.union ssigacc ssig with
        | Ok(ssigacc) -> ssigacc
        | Error(s)    -> raise (ConflictInSignature(Range.dummy "TODO (error): add range to declarations", s))
      in
      let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
      (oidsetacc, ssigacc, tyenv)
    ) (OpaqueIDSet.empty, StructSig.empty, tyenv)
  in
  (oidsetacc, ssigacc)


and typecheck_declaration (stage : stage) (tyenv : Typeenv.t) (utdecl : untyped_declaration) : OpaqueIDSet.t * StructSig.t =
  match utdecl with
  | UTDeclValue((_, x) as var, _constrnts, mty) ->
      let tyvars = [] in (* TODO: get universally quantified type variables *)
      let pre =
        let pre_init =
          {
            stage           = stage;
            level           = Level.bottom;
            type_parameters = TypeParameterMap.empty;
            quantifiability = Quantifiable;
          }
        in
        let (pre, _) = make_type_parameters pre_init tyvars in
        { pre with level = Level.succ Level.bottom }
      in
      let ty = decode_manual_type pre tyenv mty in
      let pty = generalize Level.bottom ty in
      let evid = EvalVarID.fresh var in
      let ventry =
        {
          val_stage = stage;
          val_name  = evid; (* TODO: make this `None` *)
          val_type  = pty;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_value x ventry in
      (OpaqueIDSet.empty, ssig)

  | UTDeclDirect(_) ->
      failwith "TODO: typecheck_declaration, UTDeclDirect"

  | UTDeclTypeTrans(_) ->
      failwith "TODO: typecheck_declaration, UTDeclTypeTrans"

  | UTDeclTypeOpaque((_, tynm), typarams) ->
      let tyid = TypeID.fresh tynm in
      let arity = List.length typarams in
      let tentry =
        {
          type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
          type_kind   = failwith "TODO: UTDeclTypeOpaque, type_kind";
        }
      in
      let ssig = StructSig.empty |> StructSig.add_type tynm tentry in
      (OpaqueIDSet.singleton tyid, ssig)

  | UTDeclModule((_, modnm), utsig) ->
      let absmodsig = typecheck_signature stage tyenv utsig in
      let (oidset, modsig) = absmodsig in
      let mentry =
        {
          mod_name      = ModuleID.fresh modnm; (* TODO: make this `None` *)
          mod_signature = modsig;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_module modnm mentry in
      (oidset, ssig)

  | UTDeclSignature((_, signm), utsig) ->
      let absmodsig = typecheck_signature stage tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      (OpaqueIDSet.empty, ssig)

  | UTDeclInclude(utsig) ->
      let absmodsig = typecheck_signature stage tyenv utsig in
      let (oidset, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utsig in
            raise (NotAStructureSignature(rng))

        | ConcStructure(ssig) ->
            (oidset, ssig)
      end


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


and get_dependency_on_synonym_types (vertices : SynonymNameSet.t) (pre : pre) (tyenv : Typeenv.t) (synbind : manual_type) : SynonymNameSet.t =
  failwith "TODO: get_dependency_on_synonym_types"


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
      let tyenv0 = tyenv in
      let pre =
        {
          stage           = stage;
          type_parameters = TypeParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in

      (* Registers types to the type environment and the graph for detecting cyclic dependency. *)
      let (synacc, vntacc, vertices, graph, tyenv) =
        tybinds |> List.fold_left (fun (synacc, vntacc, vertices, graph, tyenv) tybind ->
          let (tyident, typarams, constraints, syn_or_vnt) = tybind in
          let typaramkds =
            typarams |> List.map (fun typaram ->
              let (_rng, tyvar) = typaram in
              let kd =
                constraints |> List.find_map (fun (tyvar0, mnkd) ->
                  if String.equal tyvar tyvar0 then
                    let kd = decode_manual_kind pre tyenv0 mnkd in
                    Some(kd)
                  else
                    None
                ) |> Option.value ~default:UniversalKind
              in
              (typaram, kd)
            )
          in
          let (rng, tynm) = tyident in
          match syn_or_vnt with
          | UTBindSynonym(synbind) ->
              let data =
                SynonymDependencyGraph.{
                  position        = rng;
                  type_variables  = typaramkds;
                  definition_body = synbind;
                }
              in
              let graph = graph |> SynonymDependencyGraph.add_vertex tynm data in
              let synacc = Alist.extend synacc (tyident, typarams, synbind) in
              (synacc, vntacc, vertices |> SynonymNameSet.add tynm, graph, tyenv)

          | UTBindVariant(vntbind) ->
              let tyid = TypeID.fresh tynm in
              let arity = List.length typarams in
              let tentry =
                {
                  type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
                  type_kind   = failwith "TODO: make kind from typaramkds";
                }
              in
              let tyenv = tyenv |> Typeenv.add_type tynm tentry in
              let vntacc = Alist.extend vntacc (tyident, typarams, vntbind, tyid, tentry) in
              (synacc, vntacc, vertices, graph, tyenv)
        ) (Alist.empty, Alist.empty, SynonymNameSet.empty, SynonymDependencyGraph.empty, tyenv)
      in

      (* Traverse each definition of the synonym types and extract dependencies between them. *)
      let graph =
        synacc |> Alist.to_list |> List.fold_left (fun graph syn ->
          let ((_, tynm), tyvars, synbind) = syn in
          let dependencies = get_dependency_on_synonym_types vertices pre tyenv synbind in
          let graph =
            graph |> SynonymNameSet.fold (fun tynm_dep graph ->
              graph |> SynonymDependencyGraph.add_edge tynm tynm_dep
            ) dependencies
          in
          graph
        ) graph
      in

      (* Check that no cyclic dependency exists among synonym types. *)
      let syns =
        match SynonymDependencyGraph.topological_sort graph with
        | Error(cycle) -> failwith "TODO (error): Typechecker.typecheck_binding, UTBindType, cycle"
        | Ok(syns)     -> syns
      in

      (* Add the definition of the synonym types to the type environment. *)
      let (tyenv, tydefacc) =
        syns |> List.fold_left (fun (tyenv, tydefacc) syn ->
          let (tynm, syndata) = syn in
          let
            SynonymDependencyGraph.{
              type_variables  = typarams;
              definition_body = mty_body;
              _
            } = syndata
          in
          let bids = failwith "TODO: make bound IDs from typarams" in
          let ty_body = decode_manual_type pre tyenv mty_body in
          let pty_body = TypeConv.generalize Level.bottom ty_body in
          let tentry =
            {
              type_scheme = (bids, pty_body);
              type_kind   = failwith "TODO: make kind from typarams";
            }
          in
          let tyenv = tyenv |> Typeenv.add_type tynm tentry in
          let tydefacc = Alist.extend tydefacc (tynm, tentry) in
          (tyenv, tydefacc)
        ) (tyenv, Alist.empty)
      in

      (* Traverse each definition of the variant types. *)
      let tydefacc =
        vntacc |> Alist.to_list |> List.fold_left (fun tydefacc vnt ->
          let (tyident, tyvars, vntbind, tyid, tentry) = vnt in
          let (_, tynm) = tyident in
          let (pre, bids) = make_type_parameters pre tyvars in
          let _ctorbrmap = make_constructor_branch_map pre tyenv vntbind in
          Alist.extend tydefacc (tynm, tentry)
        ) tydefacc
      in

      let ssig =
        tydefacc |> Alist.to_list |> List.fold_left (fun ssig (tynm, tentry) ->
          ssig |> StructSig.add_type tynm tentry
        ) StructSig.empty
      in
      ([], (OpaqueIDSet.empty, ssig))

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

  | UTBindHorzMacro((_, _csnm), _macparams, _utast1) ->
      failwith "TODO: Typechecker.typecheck_binding, UTBindHorzMacro"

  | UTBindVertMacro((_, _csnm), _macparams, _utast2) ->
      failwith "TODO: Typechecker.typecheck_binding, UTBindVertMacro"


and subtype_poly_type_scheme (internbid : BoundID.t -> poly_type -> bool) (pty1 : poly_type) (pty2 : poly_type) : bool =
  failwith "TODO: subtype_poly_type_scheme"


and subtype_poly_type (pty1 : poly_type) (pty2 : poly_type) : bool =
  let bidht = BoundIDHashTable.create 32 in
  let internbid (bid1 : BoundID.t) (pty2 : poly_type) : bool =
    match BoundIDHashTable.find_opt bidht bid1 with
    | None ->
        BoundIDHashTable.add bidht bid1 pty2;
        true

    | Some(pty) ->
        poly_type_equal pty pty2
  in
  subtype_poly_type_scheme internbid pty1 pty2


and poly_type_equal (pty1 : poly_type) (pty2 : poly_type) : bool =
  failwith "TODO: poly_type_equal"


and subtype_type_abstraction (ptyfun1 : BoundID.t list * poly_type) (ptyfun2 : BoundID.t list * poly_type) : bool =
  let (typarams1, pty1) = ptyfun1 in
  let (typarams2, pty2) = ptyfun2 in
(*
  Format.printf "subtype_type_abstraction: %a <?= %a\n" pp_poly_type pty1 pp_poly_type pty2;  (* for debug *)
*)
  match List.combine typarams1 typarams2 with
  | exception Invalid_argument(_) ->
      false

  | typarampairs ->
      let map =
        typarampairs |> List.fold_left (fun map (bid1, bid2) ->
          map |> BoundIDMap.add bid2 bid1
        ) BoundIDMap.empty
      in
      let internbid (bid1 : BoundID.t) (Poly(pty2) : poly_type) : bool =
        match pty2 with
        | (_, TypeVariable(PolyBound(bid2))) ->
            begin
              match map |> BoundIDMap.find_opt bid1 with
              | None      -> false
              | Some(bid) -> BoundID.equal bid bid2
            end

        | _ ->
            false
      in
      subtype_poly_type_scheme internbid pty1 pty2


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
