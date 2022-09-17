
open MyUtil
open SyntaxBase
open Types
open StaticEnv

exception TypeError of TypeError.type_error


let raise_error (tyerr : TypeError.type_error) =
  raise (TypeError(tyerr))


exception InternalInclusionError
exception InternalContradictionError


module SubstMap = Map.Make(TypeID)

type substitution = type_scheme SubstMap.t

module SynonymNameMap = Map.Make(String)

module SynonymVertexHashSet = Hashtbl.Make(SynonymDependencyGraph.Vertex)

module SynonymVertexSet = Set.Make(SynonymDependencyGraph.Vertex)

type variant_definition = type_name * TypeID.t * BoundID.t list * constructor_branch_map

module PatternVarMap = Map.Make(String)

type pattern_var_map = (Range.t * EvalVarID.t * mono_type) PatternVarMap.t

type type_intern = (BoundID.t -> poly_type -> bool)

type row_intern = (BoundRowID.t -> normalized_poly_row -> bool)


let make_range_from_module_chain ((modident, projs) : module_name_chain) : Range.t =
  let (rngL, _) = modident in
  match List.rev projs with
  | []             -> rngL
  | (rngR, _) :: _ -> Range.unite rngL rngR


let fresh_free_id (qtfbl : quantifiability) (lev : Level.t) : FreeID.t =
  FreeID.fresh lev (qtfbl = Quantifiable)


let fresh_free_row_id (lev : Level.t) (labset : LabelSet.t) : FreeRowID.t =
  FreeRowID.fresh lev labset


let unify_quantifier (quant1 : quantifier) (quant2 : quantifier) : quantifier =
  OpaqueIDMap.union (fun _ _kd1 kd2 -> Some(kd2)) quant1 quant2


let decode_manual_row_base_kind (mnrbkd : manual_row_base_kind) : row_base_kind =
  mnrbkd |> List.fold_left (fun labset (rng, label) ->
    if labset |> LabelSet.mem label then
      raise_error (LabelUsedMoreThanOnce(rng, label))
    else
      labset |> LabelSet.add label
  ) LabelSet.empty


let add_dummy_fold (tynm : type_name) (tyid : TypeID.t) (bids : BoundID.t list) (ctorbrmap : constructor_branch_map) (ssig : StructSig.t) : StructSig.t =
  let bid = BoundID.fresh () in
  let dr = Range.dummy "add_dummy_fold" in
  let prow =
    ConstructorMap.fold (fun ctornm (Poly(ptyarg)) prow ->
      let pty = (dr, FuncType(RowEmpty, ptyarg, (dr, TypeVariable(PolyBound(bid))))) in
      RowCons((dr, ctornm), pty, prow)
    ) ctorbrmap RowEmpty
  in
  let ptydom1 = (dr, RecordType(prow)) in
  let ptydom2 = (dr, DataType(bids |> List.map (fun bid -> (dr, TypeVariable(PolyBound(bid)))), tyid)) in
  let ptycod = (dr, TypeVariable(PolyBound(bid))) in
  let pty = (dr, FuncType(RowEmpty, ptydom1, (dr, FuncType(RowEmpty, ptydom2, ptycod)))) in
  ssig |> StructSig.add_dummy_fold tynm (Poly(pty))


let add_constructor_definitions (ctordefs : variant_definition list) (ssig : StructSig.t) : StructSig.t =
  ctordefs |> List.fold_left (fun ssig ctordef ->
    let (tynm, tyid, bids, ctorbrmap) = ctordef in
    let ssig =
      ConstructorMap.fold (fun ctornm ptyarg ssig ->
        let centry =
          {
            ctor_belongs_to = tyid;
            ctor_parameter  = (bids, ptyarg);
          }
        in
        ssig |> StructSig.add_constructor ctornm centry
      ) ctorbrmap ssig
    in
    ssig |> add_dummy_fold tynm tyid bids ctorbrmap
  ) ssig


let add_type_parameters (lev : Level.t) (tyvars : (type_variable_name ranged) list) (typarammap : type_parameter_map) : type_parameter_map * BoundID.t list =
  let (typarammap, bidacc) =
    tyvars |> List.fold_left (fun (typarammap, bidacc) (rng, tyvarnm) ->
      if typarammap |> TypeParameterMap.mem tyvarnm then
        raise_error (TypeParameterBoundMoreThanOnce(rng, tyvarnm))
      else
        let mbbid = MustBeBoundID.fresh lev in
        let bid = MustBeBoundID.to_bound_id mbbid in
        (typarammap |> TypeParameterMap.add tyvarnm mbbid, Alist.extend bidacc bid)
    ) (typarammap, Alist.empty)
  in
  (typarammap, Alist.to_list bidacc)


let add_row_parameters (lev : Level.t) (rowvars : (row_variable_name ranged * manual_row_base_kind) list) (rowparammap : row_parameter_map) : row_parameter_map * BoundRowID.t list =
  let (rowparammap, bridacc) =
    rowvars |> List.fold_left (fun (rowparammap, bridacc) ((rng, rowvarnm), mnbrkd) ->
      if rowparammap |> RowParameterMap.mem rowvarnm then
        raise_error (LabelUsedMoreThanOnce(rng, rowvarnm))
      else
        let labset = decode_manual_row_base_kind mnbrkd in
        let mbbrid = MustBeBoundRowID.fresh lev labset in
        let brid = MustBeBoundRowID.to_bound_id mbbrid in
        (rowparammap |> RowParameterMap.add rowvarnm mbbrid, Alist.extend bridacc brid)
    ) (rowparammap, Alist.empty)
  in
  (rowparammap, Alist.to_list bridacc)


let find_constructor_and_instantiate (pre : pre) (tyenv : Typeenv.t) (ctornm : constructor_name) (rng : Range.t) =
  match tyenv |> Typeenv.find_constructor ctornm with
  | None ->
      let cands =
        []  (* TODO (enhance): find candidate constructors *)
        (* tyenv |> Typeenv.find_constructor_candidates ctornm *)
      in
      raise_error (UndefinedConstructor(rng, ctornm, cands))

  | Some(centry) ->
      let qtfbl = pre.quantifiability in
      let lev = pre.level in
      let tyid = centry.ctor_belongs_to in
      let (bids, pty) = centry.ctor_parameter in
      let (bidmap, tyacc) =
        bids |> List.fold_left (fun (bidmap, tyacc) bid ->
          let fid = fresh_free_id qtfbl lev in
          let tv = Updatable(ref (MonoFree(fid))) in
          let ty = (Range.dummy "tc-constructor", TypeVariable(tv)) in
          (bidmap |> BoundIDMap.add bid ty, Alist.extend tyacc ty)
        ) (BoundIDMap.empty, Alist.empty)
      in
      let ty = TypeConv.instantiate_by_map_mono bidmap pty in
      let tys_arg = Alist.to_list tyacc in
      (tys_arg, tyid, ty)


let find_module (tyenv : Typeenv.t) ((rng, modnm) : module_name ranged) : module_entry =
  match tyenv |> Typeenv.find_module modnm with
  | None ->
      raise_error (UndefinedModuleName(rng, modnm))

  | Some(mentry) ->
      mentry


let find_module_chain (tyenv : Typeenv.t) ((modident0, modidents) : module_name_chain) : module_entry =
  let mentry0 = find_module tyenv modident0 in
  modidents |> List.fold_left (fun mentry (rng, modnm) ->
    match mentry.mod_signature with
    | ConcFunctor(fsig) ->
        raise_error (NotAStructureSignature(rng, fsig))

    | ConcStructure(ssig) ->
        begin
          match ssig |> StructSig.find_module modnm with
          | None ->
              raise_error (UndefinedModuleName(rng, modnm))

          | Some(mentry) ->
              mentry
        end
  ) mentry0


let find_macro (tyenv : Typeenv.t) (modidents : (module_name ranged) list) ((rng_cs, csnm) : macro_name ranged) : macro_entry =
  match modidents with
  | [] ->
      begin
        match tyenv |> Typeenv.find_macro csnm with
        | None           -> raise_error (UndefinedHorzMacro(rng_cs, csnm))
        | Some(macentry) -> macentry
      end

  | modident0 :: proj ->
      let modchain = (modident0, proj) in
      let mentry = find_module_chain tyenv modchain in
      begin
        match mentry.mod_signature with
        | ConcFunctor(fsig) ->
            let (rng_first, _) = modident0 in
            let rng_last =
              match List.rev proj with
              | []                 -> rng_first
              | (rng_last, _) :: _ -> rng_last
            in
            let rng = Range.unite rng_first rng_last in
            raise_error (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig) ->
            begin
              match ssig |> StructSig.find_macro csnm with
              | None           -> raise_error (UndefinedHorzMacro(rng_cs, csnm))
              | Some(macentry) -> macentry
            end
      end


let abstraction (evid : EvalVarID.t) (ast : abstract_tree) : abstract_tree =
  Function(LabelMap.empty, PatternBranch(PVariable(evid), ast))


let abstraction_list (evids : EvalVarID.t list) (ast : abstract_tree) : abstract_tree =
  List.fold_right abstraction evids ast


let add_optionals_to_type_environment ~(cons : label ranged -> mono_type -> 'a -> 'a) ~(nil : 'a) (tyenv : Typeenv.t) (pre : pre) (opt_params : (label ranged * var_name ranged) list) : 'a * EvalVarID.t LabelMap.t * Typeenv.t =
  let qtfbl = pre.quantifiability in
  let lev = pre.level in
  let (tyenv, row, evid_labmap) =
    opt_params |> List.fold_left (fun (tyenv, row, evid_labmap) (rlabel, ident) ->
      let (_, label) = rlabel in
      let (rng, varnm) = ident in
      let evid = EvalVarID.fresh ident in
      let fid = fresh_free_id qtfbl lev in
      let tvuref = ref (MonoFree(fid)) in
      let pbeta = (rng, TypeVariable(PolyFree(tvuref))) in
      let tyenv =
        let ventry =
          {
            val_type  = Poly(Primitives.option_type pbeta);
            val_name  = Some(evid);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry
      in
      (tyenv, cons rlabel (rng, TypeVariable(Updatable(tvuref))) row, evid_labmap |> LabelMap.add label evid)
    ) (tyenv, nil, LabelMap.empty)
  in
  (row, evid_labmap, tyenv)


let add_macro_parameters_to_type_environment (tyenv : Typeenv.t) (pre : pre) (macparams : untyped_macro_parameter list) : Typeenv.t * EvalVarID.t list * mono_macro_parameter_type list =
  let (tyenv, evidacc, macparamtyacc) =
    macparams |> List.fold_left (fun (tyenv, evidacc, macptyacc) macparam ->
      let param =
        match macparam with
        | UTLateMacroParam(param)  -> param
        | UTEarlyMacroParam(param) -> param
      in
      let (rng, varnm) = param in
      let evid = EvalVarID.fresh param in
      let (ptybody, beta) =
        let tvid = fresh_free_id pre.quantifiability (Level.succ pre.level) in
        let tvuref = ref (MonoFree(tvid)) in
        ((rng, TypeVariable(PolyFree(tvuref))), (rng, TypeVariable(Updatable(tvuref))))
      in
      let (pty, macparamty) =
      match macparam with
      | UTLateMacroParam(_) ->
          (Poly(Range.dummy "late-macro-param", CodeType(ptybody)), LateMacroParameter(beta))

      | UTEarlyMacroParam(_) ->
          (Poly(ptybody), EarlyMacroParameter(beta))
      in
      let ventry =
        {
          val_type  = pty;
          val_name = Some(evid);
          val_stage = Stage0;
        }
      in
      (tyenv |> Typeenv.add_value varnm ventry, Alist.extend evidacc evid, Alist.extend macptyacc macparamty)
    ) (tyenv, Alist.empty, Alist.empty)
  in
  (tyenv, Alist.to_list evidacc, Alist.to_list macparamtyacc)


let rec is_nonexpansive_expression e =
  let iter = is_nonexpansive_expression in
  match e with
  | ASTBaseConstant(_)
  | ASTEndOfList
  | Function(_)
  | ContentOf(_) ->
      true

  | NonValueConstructor(_, e1) -> iter e1
  | PrimitiveListCons(e1, e2)  -> iter e1 && iter e2
  | PrimitiveTuple(es)         -> es |> TupleList.to_list |> List.for_all iter
  | Record(e_labmap)           -> e_labmap |> LabelMap.for_all (fun _label e -> iter e)
  | LetRecIn(_, e2)            -> iter e2
  | LetNonRecIn(_, e1, e2)     -> iter e1 && iter e2
  | _                          -> false


let unite_pattern_var_map (patvarmap1 : pattern_var_map) (patvarmap2 : pattern_var_map) : pattern_var_map =
  PatternVarMap.union (fun varnm (rng1, _, _) (rng2, _, _) ->
    raise_error (MultiplePatternVariable(rng1, rng2, varnm))
  ) patvarmap1 patvarmap2


let add_pattern_var_mono (pre : pre) (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = TypeConv.lift_poly (TypeConv.erase_range_of_type ty) in
    let ventry =
      {
        val_type  = pty;
        val_name  = Some(evid);
        val_stage = pre.stage;
      }
    in
    tyenvacc |> Typeenv.add_value varnm ventry
  ) patvarmap tyenv


let add_pattern_var_poly (pre : pre) (tyenv : Typeenv.t) (patvarmap : pattern_var_map) : Typeenv.t =
  PatternVarMap.fold (fun varnm (_, evid, ty) tyenvacc ->
    let pty = TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty) in
    let ventry =
      {
        val_type  = pty;
        val_name  = Some(evid);
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
    | RowVar(MustBeBoundRow(_))                        -> assert false
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


let occurs (fid : FreeID.t) (ty : mono_type) =

  let lev = FreeID.get_level fid in

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

                | MonoFree(fid0) ->
                    if FreeID.equal fid0 fid then
                      true
                    else
                      let lev0 = FreeID.get_level fid0 in
                      if Level.less_than lev lev0 then FreeID.set_level fid0 lev; (* Updates the level *)
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

  and iter_list tys =
    List.exists iter tys

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
              if Level.less_than lev lev0 then FreeRowID.set_level frid0 lev; (* Updates the level *)
              false
        end

    | RowVar(MustBeBoundRow(_)) ->
        false
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

                | MonoFree(fid0) ->
                    let lev0 = FreeID.get_level fid0 in
                    if Level.less_than lev lev0 then FreeID.set_level fid0 lev; (* Updates the level *)
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

    | RowVar(MustBeBoundRow(_)) ->
        false

  in
  iter_row row


let rec unify_sub ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) : unit =
  let unify = unify_sub in
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
    match (tymain1, tymain2) with
    | (BaseType(bty1), BaseType(bty2)) ->
        if bty1 = bty2 then
          ()
        else
          raise InternalContradictionError

    | (FuncType(optrow1, tydom1, tycod1), FuncType(optrow2, tydom2, tycod2)) ->
        begin
          unify_row optrow1 optrow2;
          unify tydom1 tydom2;
          unify tycod1 tycod2;
        end

    | (HorzCommandType(cmdargtys1), HorzCommandType(cmdargtys2))
    | (VertCommandType(cmdargtys1), VertCommandType(cmdargtys2))
    | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2)) ->
        begin
          match List.combine cmdargtys1 cmdargtys2 with
          | exception Invalid_argument(_) ->
              raise InternalContradictionError

          | zipped ->
              zipped |> List.iter (fun (cmdargty1, cmdargty2) ->
                let CommandArgType(ty_labmap1, ty1) = cmdargty1 in
                let CommandArgType(ty_labmap2, ty2) = cmdargty2 in
                LabelMap.merge (fun label tyopt1 tyopt2 ->
                  match (tyopt1, tyopt2) with
                  | (Some(ty1), Some(ty2)) -> Some(unify ty1 ty2)
                  | (_, None) | (None, _)  -> raise InternalContradictionError
                ) ty_labmap1 ty_labmap2 |> ignore;
                unify ty1 ty2
              )
        end

    | (ProductType(tys1), ProductType(tys2)) ->
        unify_list (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

    | (RecordType(row1), RecordType(row2)) ->
        unify_row row1 row2

    | (DataType(tyargs1, tyid1), DataType(tyargs2, tyid2)) ->
        if TypeID.equal tyid1 tyid2 then
          unify_list tyargs1 tyargs2
        else
          raise InternalContradictionError

    | (ListType(tysub1), ListType(tysub2)) -> unify tysub1 tysub2
    | (RefType(tysub1), RefType(tysub2))   -> unify tysub1 tysub2
    | (CodeType(tysub1), CodeType(tysub2)) -> unify tysub1 tysub2

    | (TypeVariable(Updatable{contents = MonoLink(tylinked1)}), _) -> unify tylinked1 ty2
    | (_, TypeVariable(Updatable{contents = MonoLink(tylinked2)})) -> unify ty1 tylinked2

    | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)),
          TypeVariable(Updatable({contents = MonoFree(fid2)} as tvref2))) ->
        if FreeID.equal fid1 fid2 then
          ()
        else begin
          (* Equate quantifiabilities *)
          begin
            match (FreeID.get_quantifiability fid1, FreeID.get_quantifiability fid2) with
            | (true, true) ->
                ()

            | _ ->
                FreeID.set_quantifiability fid1 false;
                FreeID.set_quantifiability fid2 false
          end;

          (* Equate levels *)
          let lev1 = FreeID.get_level fid1 in
          let lev2 = FreeID.get_level fid2 in
          let lev_min = if Level.less_than lev1 lev2 then lev1 else lev2 in
          FreeID.set_level fid1 lev_min;
          FreeID.set_level fid2 lev_min;

          let (tvref_old, ty_new) = if Range.is_dummy rng1 then (tvref1, ty2) else (tvref2, ty1) in
          tvref_old := MonoLink(ty_new)
        end

      | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)), _) ->
          let chk = occurs fid1 ty2 in
          if chk then
            raise InternalInclusionError
          else
            let ty2new = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
            tvref1 := MonoLink(ty2new)

      | (_, TypeVariable(Updatable({contents = MonoFree(fid2)} as tvref2))) ->
          let chk = occurs fid2 ty1 in
          if chk then
            raise InternalInclusionError
          else
            let ty1new = if Range.is_dummy rng2 then (rng1, tymain1) else (rng2, tymain1) in
            tvref2 := MonoLink(ty1new)

      | (TypeVariable(MustBeBound(mbbid1)), TypeVariable(MustBeBound(mbbid2))) ->
          if MustBeBoundID.equal mbbid1 mbbid2 then
            ()
          else
            raise InternalContradictionError

      | _ ->
          raise InternalContradictionError


and unify_list (tys1 : mono_type list) (tys2 : mono_type list) : unit =
  match List.combine tys1 tys2 with
  | exception Invalid_argument(_) -> raise InternalContradictionError
  | zipped                        -> zipped |> List.iter (fun (t1, t2) -> unify_sub t1 t2)


and solve_row_disjointness (row : mono_row) (labset : LabelSet.t) : unit =
  match row with
  | RowCons((rng, label), ty, rowsub) ->
      if labset |> LabelSet.mem label then
        raise InternalContradictionError
          (* TODO (error): should report error about label *)
      else
        solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoORLink(rowsub)}) ->
      solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoORFree(frid0)}) ->
      let labset0 = FreeRowID.get_label_set frid0 in
      FreeRowID.set_label_set frid0 (LabelSet.union labset0 labset)

  | RowVar(MustBeBoundRow(mbbrid0)) ->
      let labset0 = BoundRowID.get_label_set (MustBeBoundRowID.to_bound_id mbbrid0) in
      if LabelSet.subset labset labset0 then
        ()
      else
        raise InternalContradictionError
          (* TODO (error): insufficient constraint *)

  | RowEmpty ->
      ()


and solve_row_membership (rng : Range.t) (label : label) (ty : mono_type) (row : mono_row) : mono_row =
  match row with
  | RowCons((rng0, label0), ty0, row0) ->
      if String.equal label0 label then begin
        unify_sub ty0 ty;
        row0
      end else
        let row0rest = solve_row_membership rng label ty row0 in
        RowCons((rng0, label0), ty0, row0rest)

  | RowVar(UpdatableRow{contents = MonoORLink(row0)}) ->
      solve_row_membership rng label ty row0

  | RowVar(UpdatableRow({contents = MonoORFree(frid0)} as orvuref0)) ->
      let labset0 = FreeRowID.get_label_set frid0 in
      if labset0 |> LabelSet.mem label then
        raise InternalContradictionError
          (* TODO (error): reject for the disjointness *)
      else begin
        let lev0 = FreeRowID.get_level frid0 in
        let frid1 = FreeRowID.fresh lev0 LabelSet.empty in
        let rvref1 = ref (MonoORFree(frid1)) in
        let row_rest = RowVar(UpdatableRow(rvref1)) in
        let row_new = RowCons((rng, label), ty, row_rest) in
        orvuref0 := MonoORLink(row_new);
        row_rest
      end

  | RowVar(MustBeBoundRow(_)) ->
      raise InternalContradictionError
        (* TODO (error): solve_row_membership, MustBeBoundRow *)

  | RowEmpty ->
      raise InternalContradictionError
        (* TODO (error): solve_row_membership, RowEmpty *)


and unify_row (row1 : mono_row) (row2 : mono_row) : unit =
  match (row1, row2) with
  | (RowVar(UpdatableRow{contents = MonoORLink(row1sub)}), _) ->
      unify_row row1sub row2

  | (_, RowVar(UpdatableRow{contents = MonoORLink(row2sub)})) ->
      unify_row row1 row2sub

  | (RowVar(UpdatableRow({contents = MonoORFree(frid1)} as rvref1)), RowVar(UpdatableRow{contents = MonoORFree(frid2)})) ->
      if FreeRowID.equal frid1 frid2 then
        ()
      else
        rvref1 := MonoORLink(row2)

  | (RowVar(UpdatableRow({contents = MonoORFree(frid1)} as rvref1)), _) ->
      if occurs_row frid1 row2 then
        raise InternalInclusionError
      else begin
        let labset1 = FreeRowID.get_label_set frid1 in
        solve_row_disjointness row2 labset1;
        rvref1 := MonoORLink(row2)
      end

  | (_, RowVar(UpdatableRow({contents = MonoORFree(frid2)} as rvref2))) ->
      if occurs_row frid2 row1 then
        raise InternalInclusionError
      else begin
        let labset2 = FreeRowID.get_label_set frid2 in
        solve_row_disjointness row1 labset2;
        rvref2 := MonoORLink(row1)
      end

  | (RowVar(MustBeBoundRow(mbbrid1)), RowVar(MustBeBoundRow(mbbrid2))) ->
      if MustBeBoundRowID.equal mbbrid1 mbbrid2 then
        ()
      else
        raise InternalContradictionError

  | (RowVar(MustBeBoundRow(_)), _) | (_, RowVar(MustBeBoundRow(_))) ->
      raise InternalContradictionError

  | (RowCons((rng, label), ty1, row1sub), _) ->
      let row2rest = solve_row_membership rng label ty1 row2 in
      unify_row row1sub row2rest

  | (RowEmpty, RowEmpty) ->
      ()

  | (RowEmpty, RowCons(_, _, _)) ->
      raise InternalContradictionError


let unify (ty1 : mono_type) (ty2 : mono_type) =
  try
    unify_sub ty1 ty2
  with
  | InternalInclusionError     -> raise_error (InclusionError(ty1, ty2))
  | InternalContradictionError -> raise_error (ContradictionError(ty1, ty2))


let fresh_type_variable (rng : Range.t) (pre : pre) : mono_type =
  let fid = fresh_free_id pre.quantifiability pre.level in
  let tvuref = ref (MonoFree(fid)) in
  (rng, TypeVariable(Updatable(tvuref)))


let base bc =
  ASTBaseConstant(bc)


let rec typecheck
    (pre : pre) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) =
  let typecheck_iter ?s:(s = pre.stage) ?l:(l = pre.level) ?p:(p = pre.type_parameters) ?r:(r = pre.row_parameters) ?q:(q = pre.quantifiability) t u =
    let presub =
      {
        stage           = s;
        type_parameters = p;
        row_parameters  = r;
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

  | UTPositionedString(ipos, s) ->
      begin
        match pre.stage with
        | Stage1 | Persistent0 ->
            raise_error (InvalidExpressionAsToStaging(rng, Stage0))

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
          | _      -> raise_error (UnknownUnitOfLength(rng, unitnm))
        in
        (base (BCLength(len)), (rng, BaseType(LengthType)))

  | UTInputHorz(utihlst) ->
      let ihlst = typecheck_input_horz rng pre tyenv utihlst in
      (InputHorz(ihlst), (rng, BaseType(TextRowType)))

  | UTInputVert(utivlst) ->
      let ivlst = typecheck_input_vert rng pre tyenv utivlst in
      (InputVert(ivlst), (rng, BaseType(TextColType)))

  | UTInputMath(utmes) ->
      let tymath = (rng, BaseType(TextMathType)) in
      let ms = typecheck_math pre tyenv utmes in
      (InputMath(ms), tymath)

  | UTOpenIn((rng_mod, modnm), utast1) ->
      begin
        match tyenv |> Typeenv.find_module modnm with
        | None ->
            raise_error (UndefinedModuleName(rng_mod, modnm))

        | Some(mentry) ->
            begin
              match mentry.mod_signature with
              | ConcFunctor(fsig) ->
                  raise_error (NotAStructureSignature(rng_mod, fsig))

              | ConcStructure(ssig) ->
                  let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
                  typecheck_iter tyenv utast1
            end
      end

  | UTContentOf(modidents, (rng_var, varnm)) ->
      let (pty, e) =
        match modidents with
        | [] ->
            let ventry =
              match tyenv |> Typeenv.find_value varnm with
              | None ->
                  let cands =
                    [] (* TODO (enhance): find candidates *)
                    (* tyenv |> Typeenv.find_candidates varnm *)
                  in
                  raise_error (UndefinedVariable(rng, varnm, cands))

              | Some(ventry) ->
                  ventry
            in
            let evid =
              match ventry.val_name with
              | None       -> assert false
              | Some(evid) -> evid
            in
            let stage = ventry.val_stage in
            let e =
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0) ->
                  ContentOf(rng, evid)

              | (Stage1, Persistent0) ->
                  Persistent(rng, evid)

              | (Stage0, Stage0)
              | (Stage1, Stage1) ->
                  ContentOf(rng, evid)

              | _ ->
                  raise_error (InvalidOccurrenceAsToStaging(rng_var, varnm, stage))
            in
            (ventry.val_type, e)

        | modident0 :: proj ->
            let modchain = (modident0, proj) in
            let mentry = find_module_chain tyenv modchain in
            let ssig =
              match mentry.mod_signature with
              | ConcStructure(ssig) -> ssig
              | ConcFunctor(fsig)   -> raise_error (NotAStructureSignature(rng, fsig))
                  (*TODO (enhance): give a better code range to this error. *)
            in
            let ventry =
              match ssig |> StructSig.find_value varnm with
              | None ->
                  let cands =
                    [] (* TODO (enhance): find candidates *)
                    (* ssig |> StructSig.find_candidates varnm *)
                  in
                  raise_error (UndefinedVariable(rng, varnm, cands))

              | Some(ventry) ->
                  ventry
            in
            let stage = ventry.val_stage in
            let evid =
              match ventry.val_name with
              | None -> (* from a functor parameter *)
                  EvalVarID.fresh (rng, "(dummy)")
(*
                  let (_, modnm0) = modident0 in failwith (Format.asprintf "%s, %a" modnm0 Range.pp rng)
*)
              | Some(evid) -> evid
            in
            let e =
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0) ->
                  ContentOf(rng, evid)

              | (Stage1, Persistent0) ->
                  Persistent(rng, evid)

              | (Stage0, Stage0)
              | (Stage1, Stage1) ->
                  ContentOf(rng, evid)

              | _ ->
                  raise_error (InvalidOccurrenceAsToStaging(rng_var, varnm, stage))
            in
            (ventry.val_type, e)
      in
      let tyfree = TypeConv.instantiate pre.level pre.quantifiability pty in
      let tyres = TypeConv.overwrite_range_of_type tyfree rng in
      (e, tyres)

  | UTConstructor(constrnm, utast1) ->
      let (tyargs, tyid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      unify ty1 tyc;
      let tyres = (rng, DataType(tyargs, tyid)) in
      (NonValueConstructor(constrnm, e1), tyres)

  | UTLambdaHorz(ident_ctx, utast1) ->
      let (rng_var, varnm_ctx) = ident_ctx in
      let (bsty_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BoxRowType)
      in
      let evid = EvalVarID.fresh ident_ctx in
      let (e1, ty1) =
        let tyenv_sub =
          let ventry =
            {
              val_type  = Poly(rng_var, BaseType(bsty_var));
              val_name  = Some(evid);
              val_stage = pre.stage;
            }
          in
          tyenv |> Typeenv.add_value varnm_ctx ventry
        in
        typecheck_iter tyenv_sub utast1
      in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      unify tyret (Range.dummy "lambda-horz-return", BaseType(bsty_ret));
      (abstraction evid e1, (rng, HorzCommandType(cmdargtylist)))

  | UTLambdaVert(ident_ctx, utast1) ->
      let (rng_var, varnm_ctx) = ident_ctx in
      let (bsty_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BoxColType)
      in
      let evid = EvalVarID.fresh ident_ctx in
      let (e1, ty1) =
        let tyenv_sub =
          let ventry =
            {
              val_type  = Poly(rng_var, BaseType(bsty_var));
              val_name  = Some(evid);
              val_stage = pre.stage;
            }
          in
          tyenv |> Typeenv.add_value varnm_ctx ventry
        in
        typecheck_iter tyenv_sub utast1
      in
      let (cmdargtylist, tyret) = flatten_type ty1 in
      unify tyret (Range.dummy "lambda-vert-return", BaseType(bsty_ret));
      (abstraction evid e1, (rng, VertCommandType(cmdargtylist)))

  | UTLambdaMathCommand{
      parameters       = param_units;
      context_variable = ident_ctx;
      script_variables = ident_pair_opt;
      body             = utast_body;
    } ->
      let (tyenv, params) = typecheck_abstraction pre tyenv param_units in
      let (rng_ctx_var, varnm_ctx) = ident_ctx in
      let (bsty_ctx_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BoxMathType)
      in
      let evid_ctx = EvalVarID.fresh ident_ctx in
      let script_params_opt =
        ident_pair_opt |> Option.map (fun (ident_sub, ident_sup) ->
          let evid_sub = EvalVarID.fresh ident_sub in
          let evid_sup = EvalVarID.fresh ident_sup in
          (ident_sub, evid_sub, ident_sup, evid_sup)
        )
      in
      let tyenv =
        let ventry =
          {
            val_type  = Poly(rng_ctx_var, BaseType(bsty_ctx_var));
            val_name  = Some(evid_ctx);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm_ctx ventry
      in
      let (tyenv, evid_pair_opt) =
        match script_params_opt with
        | None ->
            (tyenv, None)

        | Some((ident_sub, evid_sub, ident_sup, evid_sup)) ->
            let (rng_sub_var, varnm_sub) = ident_sub in
            let (rng_sup_var, varnm_sup) = ident_sup in
            let pty_script rng =
              Poly(rng, snd (Primitives.option_type (Range.dummy "sub-or-sup", BaseType(TextMathType))))
            in
            let ventry_sub =
              {
                val_type  = pty_script rng_sub_var;
                val_name  = Some(evid_sub);
                val_stage = pre.stage;
              }
            in
            let ventry_sup =
              {
                val_type  = pty_script rng_sup_var;
                val_name  = Some(evid_sup);
                val_stage = pre.stage;
              }
            in
            let tyenv =
              tyenv
                |> Typeenv.add_value varnm_sub ventry_sub
                |> Typeenv.add_value varnm_sup ventry_sup
            in
            (tyenv, Some((evid_sub, evid_sup)))
      in
      let (e_body, ty_body) = typecheck_iter tyenv utast_body in
      unify ty_body (Range.dummy "lambda-math-return", BaseType(bsty_ret));
      let e =
        List.fold_right (fun (evid_labmap, pat, ty_labmap, ty_pat) e ->
          Function(evid_labmap, PatternBranch(pat, e))
        ) params (LambdaMath(evid_ctx, evid_pair_opt, e_body))
      in
      let cmdargtys =
        params |> List.map (fun (_, _, ty_labmap, ty_pat) ->
          CommandArgType(ty_labmap, ty_pat)
        )
      in
      (e, (rng, MathCommandType(cmdargtys)))

  | UTApply(opts, utast1, utast2) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let frid = FreeRowID.fresh pre.level LabelSet.empty in
      let (e_labmap0, labset0, row0) =
        let rvref = ref (MonoORFree(frid)) in
        opts |> List.fold_left (fun (e_labmap, labset, row) (rlabel, utast0) ->
          let (_, label) = rlabel in
          let (e0, ty0) = typecheck_iter tyenv utast0 in
          (e_labmap |> LabelMap.add label e0, labset |> LabelSet.add label, RowCons(rlabel, ty0, row))
        ) (LabelMap.empty, LabelSet.empty, RowVar(UpdatableRow(rvref)))
      in
      let labset = FreeRowID.get_label_set frid in
      FreeRowID.set_label_set frid (LabelSet.union labset labset0);
      let eret = Apply(e_labmap0, e1, e2) in
      begin
        match TypeConv.unlink ty1 with
        | (_, FuncType(row, tydom, tycod)) ->
            unify_row row0 row;
            unify ty2 tydom;
            let tycodnew = TypeConv.overwrite_range_of_type tycod rng in
            (eret, tycodnew)

        | (_, TypeVariable(_)) as ty1 ->
            let beta = fresh_type_variable rng pre in
            unify ty1 (get_range utast1, FuncType(row0, ty2, beta));
            (eret, beta)

        | ty1 ->
            let (rng1, _) = utast1 in
            raise_error (ApplicationOfNonFunction(rng1, ty1))
      end

  | UTFunction(UTParameterUnit(opt_params, pat, mnty_opt), utast1) ->
      let (optrow, evid_labmap, tyenv) =
        let cons rlabel ty row =
          (RowCons(rlabel, ty, row))
        in
        let nil = RowEmpty in
        add_optionals_to_type_environment ~cons ~nil tyenv pre opt_params
      in
      let utpatbr = UTPatternBranch(pat, utast1) in
      let (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
      mnty_opt |> Option.map (fun mnty ->
        let typat_annot = decode_manual_type pre tyenv mnty in
        unify typat typat_annot;
      ) |> ignore;
      (Function(evid_labmap, patbr), (rng, FuncType(optrow, typat, ty1)))

  | UTPatternMatch(utastO, utpatbrs) ->
      let (eO, tyO) = typecheck_iter tyenv utastO in
      let beta = fresh_type_variable (Range.dummy "ut-pattern-match") pre in
      let patbrs = typecheck_pattern_branch_list pre tyenv utpatbrs tyO beta in
      Exhchecker.main rng patbrs tyO pre tyenv;
      (PatternMatch(rng, eO, patbrs), beta)

  | UTLetIn(UTNonRec((ident, utast1)), utast2) ->
      let presub = { pre with level = Level.succ pre.level; } in
      let (_, varnm) = ident in
      let evid = EvalVarID.fresh ident in
      let (e1, ty1) = typecheck presub tyenv utast1 in
      let tyenv =
        let pty =
          if is_nonexpansive_expression e1 then
          (* If `e1` is polymorphically typeable: *)
            TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty1)
          else
          (* If `e1` should be typed monomorphically: *)
            TypeConv.lift_poly (TypeConv.erase_range_of_type ty1)
        in
        let ventry =
          {
            val_type  = pty;
            val_name  = Some(evid);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry
      in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      (LetNonRecIn(PVariable(evid), e1, e2), ty2)

  | UTLetIn(UTRec(utrecbinds), utast2) ->
      let quints = typecheck_letrec pre tyenv utrecbinds in
      let (tyenv, recbindacc) =
        quints |> List.fold_left (fun (tyenv, recbindacc) quint ->
          let (x, pty, evid, recbind) = quint in
          let tyenv =
            let ventry =
              {
                val_type  = pty;
                val_name  = Some(evid);
                val_stage = pre.stage;
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

  | UTLetIn(UTMutable(ident, utastI), utastA) ->
      let (tyenvI, evid, eI, tyI) = make_type_environment_by_let_mutable pre tyenv ident utastI in
      let (eA, tyA) = typecheck_iter tyenvI utastA in
      (LetMutableIn(evid, eI, eA), tyA)

  | UTOverwrite(ident, utastN) ->
      let (rng_var, _) = ident in
      begin
        match typecheck_iter tyenv (rng_var, UTContentOf([], ident)) with
        | (ContentOf(_, evid), tyvar) ->
            let (eN, tyN) = typecheck_iter tyenv utastN in
            unify tyvar (get_range utastN, RefType(tyN));
              (* Actually `get_range utastN` is not good
                 since the rhs expression has type `ty`, not `ref ty`  *)
            (Overwrite(evid, eN), (rng, BaseType(UnitType)))

        | _ ->
            assert false
      end

  | UTItemize(utitmz) ->
      let eitmz = typecheck_itemize pre tyenv utitmz in
      let ty = TypeConv.overwrite_range_of_type (Primitives.itemize_type ()) rng in
      (eitmz, ty)

  | UTListCons(utastH, utastT) ->
      let (eH, tyH) = typecheck_iter tyenv utastH in
      let (eT, tyT) = typecheck_iter tyenv utastT in
      unify tyT (Range.dummy "list-cons", ListType(tyH));
      let tyres = (rng, ListType(tyH)) in
      (PrimitiveListCons(eH, eT), tyres)

  | UTEndOfList ->
      let beta = fresh_type_variable rng pre in
      (ASTEndOfList, (rng, ListType(beta)))

  | UTTuple(utasts) ->
      let etys = TupleList.map (typecheck_iter tyenv) utasts in
      let es = TupleList.map fst etys in
      let tys = TupleList.map snd etys in
      let tyres = (rng, ProductType(tys)) in
      (PrimitiveTuple(es), tyres)

  | UTRecord(fields) ->
      typecheck_record rng pre tyenv fields

  | UTAccessField(utast1, (_, label)) ->
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let beta = fresh_type_variable rng pre in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoORFree(frid)) in
        RowCons((Range.dummy "UTAccessField", label), beta, RowVar(UpdatableRow(rvuref)))
      in
      unify ty1 (Range.dummy "UTAccessField", RecordType(row));
      (AccessField(e1, label), beta)

  | UTUpdateField(utast1, rlabel, utast2) ->
      let (_, label) = rlabel in
      let (e1, ty1) = typecheck_iter tyenv utast1 in
      let (e2, ty2) = typecheck_iter tyenv utast2 in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoORFree(frid)) in
        RowCons(rlabel, ty2, RowVar(UpdatableRow(rvuref)))
      in
      unify ty1 (Range.dummy "UTUpdateField", RecordType(row));
      (UpdateField(e1, label, e2), ty1)

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

  | UTNext(utast1) ->
      begin
        match pre.stage with
        | Stage0 ->
            let (e1, ty1) = typecheck_iter ~s:Stage1 tyenv utast1 in
            (Next(e1), (rng, CodeType(ty1)))

        | Stage1 | Persistent0 ->
            raise_error (InvalidExpressionAsToStaging(rng, Stage0))
      end

  | UTPrev(utast1) ->
      begin
        match pre.stage with
        | Stage0 | Persistent0 ->
            raise_error (InvalidExpressionAsToStaging(rng, Stage1))

        | Stage1 ->
            let (e1, ty1) = typecheck_iter ~s:Stage0 tyenv utast1 in
            let beta = fresh_type_variable rng pre in
            unify ty1 (Range.dummy "prev", CodeType(beta));
            (Prev(e1), beta)
      end


and typecheck_abstraction (pre : pre) (tyenv : Typeenv.t) (param_units : untyped_parameter_unit list) : Typeenv.t * (EvalVarID.t LabelMap.t * pattern_tree * mono_type LabelMap.t * mono_type) list =
  let (tyenv, acc) =
    param_units |> List.fold_left (fun (tyenv, acc) param_unit ->
      let UTParameterUnit(opt_params, utpat, mnty_opt) = param_unit in
      let (ty_labmap, evid_labmap, tyenv) =
        let cons (_, label) ty ty_labmap =
          ty_labmap |> LabelMap.add label ty
        in
        let nil = LabelMap.empty in
        add_optionals_to_type_environment ~cons ~nil tyenv pre opt_params
      in
      let (pat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
      mnty_opt |> Option.map (fun mnty ->
        let typat_annot = decode_manual_type pre tyenv mnty in
        unify typat typat_annot;
      ) |> ignore;
      let tyenv = add_pattern_var_mono pre tyenv patvarmap in
      (tyenv, Alist.extend acc (evid_labmap, pat, ty_labmap, typat))
    ) (tyenv, Alist.empty)
  in
  (tyenv, acc |> Alist.to_list)



and typecheck_command_arguments (tycmd : mono_type) (rngcmdapp : Range.t) (pre : pre) (tyenv : Typeenv.t) (utcmdargs : untyped_command_argument list) (cmdargtys : mono_command_argument_type list) : (abstract_tree LabelMap.t * abstract_tree) list =
  let zipped =
    try List.combine utcmdargs cmdargtys with
    | Invalid_argument(_) ->
      let arity_expected = List.length cmdargtys in
      let arity_actual = List.length utcmdargs in
      raise_error (InvalidArityOfCommandApplication(rngcmdapp, arity_expected, arity_actual))
  in
  zipped |> List.map (fun (utcmdarg, cmdargty) ->
    let UTCommandArg(labeled_utasts, utast1) = utcmdarg in
    let utast_labmap =
      labeled_utasts |> List.fold_left (fun utast_labmap ((rng, label), utast) ->
        if utast_labmap |> LabelMap.mem label then
          raise_error (LabelUsedMoreThanOnce(rng, label))
        else
          utast_labmap |> LabelMap.add label (utast, rng)
      ) LabelMap.empty
    in
    let CommandArgType(ty_labmap, ty2) = cmdargty in
    let e_labmap =
      LabelMap.merge (fun label utast_and_rng_opt ty_opt ->
        match (utast_and_rng_opt, ty_opt) with
        | (Some((utast1, _)), Some(ty2)) ->
            let (e1, ty1) = typecheck pre tyenv utast1 in
            unify ty1 ty2;
            Some(e1)

        | (None, Some(_)) ->
            None

        | (Some((_, rng)), None) ->
            raise_error (UnexpectedOptionalLabel(rng, label, tycmd))

        | (None, None) ->
            assert false
      ) utast_labmap ty_labmap
    in
    let (e1, ty1) = typecheck pre tyenv utast1 in
    unify ty1 ty2;
    (e_labmap, e1)
  )


and typecheck_math (pre : pre) (tyenv : Typeenv.t) (utmes : untyped_input_math_element list) : input_math_element list =

  let iter (b, utmes) = typecheck_math pre tyenv utmes in

  utmes |> List.map (fun utme ->
    let (rng, UTInputMathElement{ base = utbase; sub = utsub_opt; sup = utsup_opt }) = utme in
    let base =
      match utbase with
      | UTInputMathChar(uch) ->
          InputMathChar(uch)

      | UTInputMathApplyCommand(utastcmd, utcmdarglst) ->
          let (ecmd, tycmd) = typecheck pre tyenv utastcmd in
          let (_, tycmdmain) = tycmd in
          begin
            match tycmdmain with
            | MathCommandType(cmdargtylstreq) ->
                let args = typecheck_command_arguments tycmd rng pre tyenv utcmdarglst cmdargtylstreq in
                InputMathApplyCommand{
                  command   = ecmd;
                  arguments = args;
                }

            | HorzCommandType(_) ->
                let (rngcmd, _) = utastcmd in
                raise_error (HorzCommandInMath(rngcmd))

            | _ ->
                failwith (Printf.sprintf "unexpected type: %s" (Display.show_mono_type tycmd))
          end

      | UTInputMathContent(utast0) ->
          let (e0, ty0) = typecheck pre tyenv utast0 in
          unify ty0 (Range.dummy "math-embedded-var", BaseType(TextMathType));
          InputMathContent(e0)
    in
    let sub = utsub_opt |> Option.map iter in
    let sup = utsup_opt |> Option.map iter in
    InputMathElement{ base; sub; sup }
  )


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
              let eapp =
                let args = typecheck_command_arguments tycmd rngcmdapp pre tyenv utcmdarglst cmdargtylstreq in
                args |> List.fold_left (fun eacc (e_labmap, e_arg) ->
                  Apply(e_labmap, eacc, e_arg)
                ) ecmdctx
              in
              let eabs = abstraction evid eapp in
              aux (Alist.extend acc (InputVertEmbedded(eabs))) tail

          | _ ->
              assert false
        end

    | (_, UTInputVertContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify ty0 (Range.dummy "UTInputVertContent", BaseType(TextColType));
        aux (Alist.extend acc (InputVertContent(e0))) tail

    | (rng_app, UTInputVertMacro(bmacro, utmacargs)) :: tail ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              raise_error (InvalidExpressionAsToStaging(rng_app, Stage1))

          | Stage1 ->
              let (rng_all, modidents, cs) = bmacro in
              let (rng_cs, _csnm) = cs in
              let macentry = find_macro tyenv modidents cs in
              let macty = TypeConv.instantiate_macro_type pre.level pre.quantifiability macentry.macro_type in
              let macparamtys =
                match macty with
                | VertMacroType(macparamtys) -> macparamtys
                | _                          -> assert false
              in
              let evid =
                match macentry.macro_name with
                | Some(evid) -> evid
                | None       -> assert false
              in
              let eargs = typecheck_macro_arguments rng_app pre tyenv macparamtys utmacargs in
              let eapp = apply_tree_of_list (ContentOf(rng_cs, evid)) eargs in
              let iv = InputVertContent(Prev(eapp)) in
              aux (Alist.extend acc iv) tail
        end
  in
  aux Alist.empty utivlst


and typecheck_input_horz (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (utihlst : untyped_input_horz_element list) : input_horz_element list =
  let rec aux acc utihlst =
    match utihlst with
    | [] ->
        Alist.to_list acc

    | (rng_cmdapp, UTInputHorzApplyCommand(utast_cmd, utcmdargs)) :: tail ->
        let (e_cmd, ty_cmd) = typecheck pre tyenv utast_cmd in
        let cmdargtys =
          match ty_cmd with
          | (_, HorzCommandType(cmdargtys)) ->
              cmdargtys

          | (_, MathCommandType(_)) ->
              let (rng_cmd, _) = utast_cmd in
              raise_error (MathCommandInHorz(rng_cmd))

          | _ ->
              assert false
        in
        let evid = EvalVarID.fresh (Range.dummy "ctx-horz", "%ctx-horz") in
        let e_cmdctx = Apply(LabelMap.empty, e_cmd, ContentOf(Range.dummy "ctx-horz", evid)) in
        let eapp =
          let args = typecheck_command_arguments ty_cmd rng_cmdapp pre tyenv utcmdargs cmdargtys in
          args |> List.fold_left (fun eacc (e_labmap, e_arg) ->
            Apply(e_labmap, eacc, e_arg)
          ) e_cmdctx
        in
        let e_abs = abstraction evid eapp in
        aux (Alist.extend acc (InputHorzApplyCommand(e_abs))) tail

    | (_, UTInputHorzEmbeddedMath(utast_math)) :: tail ->
        let (emath, tymath) = typecheck pre tyenv utast_math in
        unify tymath (Range.dummy "ut-input-horz-embedded-math", BaseType(TextMathType));
        aux (Alist.extend acc (InputHorzEmbeddedMath(emath))) tail

    | (_, UTInputHorzEmbeddedCodeArea(s)) :: tail ->
        aux (Alist.extend acc (InputHorzEmbeddedCodeArea(s))) tail

    | (_, UTInputHorzContent(utast0)) :: tail ->
        let (e0, ty0) = typecheck pre tyenv utast0 in
        unify ty0 (Range.dummy "ut-input-horz-content", BaseType(TextRowType));
        aux (Alist.extend acc (InputHorzContent(e0))) tail

    | (_, UTInputHorzText(s)) :: tail ->
        aux (Alist.extend acc (InputHorzText(s))) tail

    | (rng_app, UTInputHorzMacro(hmacro, utmacargs)) :: tail ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              raise_error (InvalidExpressionAsToStaging(rng_app, Stage1))

          | Stage1 ->
              let (rng_all, modidents, cs) = hmacro in
              let (rng_cs, _csnm) = cs in
              let macentry = find_macro tyenv modidents cs in
              let macty = TypeConv.instantiate_macro_type pre.level pre.quantifiability macentry.macro_type in
              let macparamtys =
                match macty with
                | HorzMacroType(macparamtys) -> macparamtys
                | _                          -> assert false
              in
              let evid =
                match macentry.macro_name with
                | Some(evid) -> evid
                | None       -> assert false
              in
              let eargs = typecheck_macro_arguments rng_app pre tyenv macparamtys utmacargs in
              let eapp = apply_tree_of_list (ContentOf(rng_cs, evid)) eargs in
              let ih = InputHorzContent(Prev(eapp)) in
              aux (Alist.extend acc ih) tail
        end
  in
  aux Alist.empty utihlst


and typecheck_macro_arguments (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (macparamtys : mono_macro_parameter_type list) (utmacargs : untyped_macro_argument list) : abstract_tree list =
  let lenexp = List.length macparamtys in
  let lenact = List.length utmacargs in
  if (lenexp <> lenact) then
    raise_error (InvalidNumberOfMacroArguments(rng, macparamtys))
  else
    let argacc =
      List.fold_left2 (fun argacc macparamty utmacarg ->
        match macparamty with
        | LateMacroParameter(tyexp) ->
            begin
              match utmacarg with
              | UTLateMacroArg(utast) ->
                  let (earg, tyarg) = typecheck pre tyenv utast in
                  unify tyarg tyexp;
                  Alist.extend argacc (Next(earg))
                    (* Late arguments are converted to quoted arguments. *)

              | UTEarlyMacroArg((rngarg, _)) ->
                  raise_error (LateMacroArgumentExpected(rngarg, tyexp))
            end

        | EarlyMacroParameter(tyexp) ->
            begin
              match utmacarg with
              | UTLateMacroArg((rngarg, _)) ->
                  raise_error (EarlyMacroArgumentExpected(rngarg, tyexp))

              | UTEarlyMacroArg(utast) ->
                  let (earg, tyarg) = typecheck { pre with stage = Stage0 } tyenv utast in
                  unify tyarg tyexp;
                  Alist.extend argacc earg
            end

      ) Alist.empty macparamtys utmacargs
    in
    Alist.to_list argacc


and typecheck_record (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (fields : (label ranged * untyped_abstract_tree) list) =
  let (easc, row) =
    fields |> List.fold_left (fun (easc, row) (rlabel, utast) ->
      let (rng_label, label) = rlabel in
      if easc |> LabelMap.mem label then
        raise_error (LabelUsedMoreThanOnce(rng_label, label))
      else
        let (e, ty) = typecheck pre tyenv utast in
        (easc |> LabelMap.add label e, RowCons(rlabel, ty, row))
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
  let UTPatternBranch(utpat, utast1) = utpatbr in
  let (epat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
  let tyenvpat = add_pattern_var_mono pre tyenv patvarmap in
  let (e1, ty1) = typecheck pre tyenvpat utast1 in
  (PatternBranch(epat, e1), typat, ty1)


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
            (* If 'varnm' also occurs in `utpat1`: *)
              raise_error (MultiplePatternVariable(rngsub, rng, varnm))

          | None ->
              let evid = EvalVarID.fresh (rng, varnm) in
              (PAsVariable(evid, epat1), typat1, patvarmap1 |> PatternVarMap.add varnm (rng, evid, beta))
        end

    | UTPConstructor(constrnm, utpat1) ->
        let (tyargs, tyid, tyc) = find_constructor_and_instantiate pre tyenv constrnm rng in
        let (epat1, typat1, tyenv1) = iter utpat1 in
        unify tyc typat1;
        (PConstructor(constrnm, epat1), (rng, DataType(tyargs, tyid)), tyenv1)


and typecheck_letrec (pre : pre) (tyenv : Typeenv.t) (utrecbinds : untyped_let_binding list) : (var_name * poly_type * EvalVarID.t * letrec_binding) list =

  (* First, adds a type variable for each bound identifier. *)
  let (tyenv, utrecacc) =
    utrecbinds |> List.fold_left (fun (tyenv, utrecacc) utrecbind ->
      let ((varrng, varnm), astdef) = utrecbind in
      let tvuref =
        let tvid = fresh_free_id pre.quantifiability (Level.succ pre.level) in
        ref (MonoFree(tvid))
      in
      let tv = Updatable(tvuref) in
      let rng = get_range astdef in
      let beta = (rng, TypeVariable(tv)) in
      let pbeta = TypeConv.lift_poly beta in
      let evid = EvalVarID.fresh (varrng, varnm) in
      let tyenv =
        let ventry =
          {
            val_type  = pbeta;
            val_name  = Some(evid);
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
      let (((_, varnm), utast1), beta, evid) = utrec in
      let (e1, ty1) = typecheck { pre with level = Level.succ pre.level; } tyenv utast1 in
      begin
        match e1 with
        | Function(evid_labmap, patbr1) ->
            if LabelMap.cardinal evid_labmap = 0 then begin
              unify ty1 beta;
(*
              mntyopt |> Option.map (fun mnty ->
                let tyin = decode_manual_type pre tyenv mnty in
                unify tyin beta
              ) |> Option.value ~default:();
*)
              let recbind = LetRecBinding(evid, patbr1) in
              let tupleacc = Alist.extend tupleacc (varnm, beta, evid, recbind) in
              tupleacc
            end else
            let (rng1, _) = utast1 in
            raise_error (BreaksValueRestriction(rng1))

        | _ ->
            let (rng1, _) = utast1 in
            raise_error (BreaksValueRestriction(rng1))
      end
    ) Alist.empty
  in

  tupleacc |> Alist.to_list |> List.map (fun (varnm, ty, evid, recbind) ->
    let pty = TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty) in
    (varnm, pty, evid, recbind)
  )


and make_type_environment_by_let_mutable (pre : pre) (tyenv : Typeenv.t) (ident : var_name ranged) (utastI : untyped_abstract_tree) =
  let (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
  let (rng_var, varnm) = ident in
  let evid = EvalVarID.fresh ident in
  let tyenvI =
    let ventry =
      {
        val_type  = TypeConv.lift_poly (rng_var, RefType(tyI));
        val_name  = Some(evid);
        val_stage = pre.stage;
      }
    in
    tyenv |> Typeenv.add_value varnm ventry
  in
  (tyenvI, evid, eI, tyI)


and decode_manual_type (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : mono_type =
  let invalid rng tynm ~expect:len_expected ~actual:len_actual =
    raise_error (IllegalNumberOfTypeArguments(rng, tynm, len_expected, len_actual))
  in
  let rec aux (rng, mtymain) =
    let tymain =
      match mtymain with
      | MTypeName(modidents, tyident, mtyargs) ->
          let tyargs = mtyargs |> List.map aux in
          let len_actual = List.length tyargs in
          let (rng_tynm, tynm) = tyident in
          begin
            match modidents with
            | [] ->
                begin
                  match tyenv |> Typeenv.find_type tynm with
                  | None ->
                      begin
                        match base_type_map |> TypeNameMap.find_opt tynm with
                        | None ->
                            begin
                              match tynm with
                              | "code" ->
                                  begin
                                    match tyargs with
                                    | [ ty ] -> CodeType(ty)
                                    | _      -> invalid rng "code" ~expect:1 ~actual:len_actual
                                  end

                              | "list" ->
                                  begin
                                    match tyargs with
                                    | [ ty ] -> ListType(ty)
                                    | _      -> invalid rng "list" ~expect:1 ~actual:len_actual
                                  end

                              | "ref" ->
                                  begin
                                    match tyargs with
                                    | [ ty ] -> RefType(ty)
                                    | _      -> invalid rng "ref" ~expect:1 ~actual:len_actual
                                  end

                              | _ ->
                                  raise_error (UndefinedTypeName(rng_tynm, tynm))
                            end

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

            | modident0 :: proj ->
                let modchain = (modident0, proj) in
                let mentry = find_module_chain tyenv modchain in
                begin
                  match mentry.mod_signature with
                  | ConcFunctor(fsig) ->
                      raise_error (NotAStructureSignature(rng, fsig))
                        (* TODO (enhance): give a better code range to this error *)

                  | ConcStructure(ssig) ->
                      begin
                        match ssig |> StructSig.find_type tynm with
                        | None ->
                            raise_error (UndefinedTypeName(rng_tynm, tynm))

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
                end
          end

      | MTypeParam(typaram) ->
          begin
            match pre.type_parameters |> TypeParameterMap.find_opt typaram with
            | None ->
                raise_error (UndefinedTypeVariable(rng, typaram))

            | Some(mbbid) ->
                TypeVariable(MustBeBound(mbbid))
          end

      | MFuncType(mnfields, rowvar_opt, mtydom, mtycod) ->
          FuncType(aux_row mnfields rowvar_opt, aux mtydom, aux mtycod)

      | MProductType(mntys) ->
          ProductType(TupleList.map aux mntys)

      | MRecordType(mnfields, rowvar) ->
          RecordType(aux_row mnfields rowvar)

      | MHorzCommandType(mncmdargtys) -> HorzCommandType(aux_cmd_list mncmdargtys)
      | MVertCommandType(mncmdargtys) -> VertCommandType(aux_cmd_list mncmdargtys)
      | MMathCommandType(mncmdargtys) -> MathCommandType(aux_cmd_list mncmdargtys)
    in
    (rng, tymain)

  and aux_cmd_list (mncmdargtys : manual_command_argument_type list) : mono_command_argument_type list =
    List.map (fun mncmdargty ->
      let MArgType(mnfields, mnty) = mncmdargty in
      let tylabmap =
        mnfields |> List.fold_left (fun tylabmap (rlabel, mnty) ->
          let (rng, label) = rlabel in
          if tylabmap |> LabelMap.mem label then
            raise_error (LabelUsedMoreThanOnce(rng, label))
          else
            let ty = aux mnty in
            tylabmap |> LabelMap.add label ty
        ) LabelMap.empty
      in
      let ty = aux mnty in
      CommandArgType(tylabmap, ty)
    ) mncmdargtys

  and aux_row (mnfields : (label ranged * manual_type) list) (rowvar_opt : (row_variable_name ranged) option) =
    let row_end =
      match rowvar_opt with
      | None ->
          RowEmpty

      | Some((rng, rowparam)) ->
          begin
            match pre.row_parameters |> RowParameterMap.find_opt rowparam with
            | None ->
                raise_error (UndefinedRowVariable(rng, rowparam))

            | Some(mbbrid) ->
                RowVar(MustBeBoundRow(mbbrid))
          end
    in
    let (_, row) =
      mnfields |> List.fold_left (fun (labset, row) (rlabel, mnty) ->
        let (rng, label) = rlabel in
        if labset |> LabelSet.mem label then
          raise_error (LabelUsedMoreThanOnce(rng, label))
        else
          let row = RowCons(rlabel, aux mnty, row) in
          (labset |> LabelSet.add label, row)
      ) (LabelSet.empty, row_end)
    in
    row
  in
  aux mty


and decode_manual_base_kind (mnbkd : manual_base_kind) : base_kind =
  let MKindName((rng, kdnm)) = mnbkd in
  match kdnm with
  | "o" -> TypeKind
  | _   -> raise_error (UndefinedKindName(rng, kdnm))


and decode_manual_kind (pre : pre) (tyenv : Typeenv.t) (mnkd : manual_kind) : kind =
  let MKind(mnbkds_dom, mnbkd_cod) = mnkd in
  let kds_dom = mnbkds_dom |> List.map decode_manual_base_kind in
  let TypeKind = decode_manual_base_kind mnbkd_cod in
  Kind(kds_dom)


and decode_manual_macro_type (pre : pre) (tyenv : Typeenv.t) (mmacty : manual_macro_type) : mono_macro_type =
  match mmacty with
  | MHorzMacroType(mmacparamtys) ->
      HorzMacroType(mmacparamtys |> List.map (decode_manual_macro_parameter_type pre tyenv))

  | MVertMacroType(mmacparamtys) ->
      VertMacroType(mmacparamtys |> List.map (decode_manual_macro_parameter_type pre tyenv))


and decode_manual_macro_parameter_type (pre : pre) (tyenv : Typeenv.t) (mmacparamty : manual_macro_parameter_type) : mono_macro_parameter_type =
  match mmacparamty with
  | MLateMacroParameter(mty) ->
      let ty = decode_manual_type pre tyenv mty in
      LateMacroParameter(ty)

  | MEarlyMacroParameter(mty) ->
      let ty = decode_manual_type pre tyenv mty in
      EarlyMacroParameter(ty)


and make_constructor_branch_map (pre : pre) (tyenv : Typeenv.t) (utctorbrs : constructor_branch list) =
  utctorbrs |> List.fold_left (fun ctormap utctorbr ->
    match utctorbr with
    | UTConstructorBranch((rng, ctornm), mty_opt) ->
        let ty =
          match mty_opt with
          | Some(mty) -> decode_manual_type pre tyenv mty
          | None      -> (Range.dummy "unit", BaseType(UnitType))
        in
        let pty = TypeConv.generalize pre.level ty in
        ctormap |> ConstructorMap.add ctornm pty
  ) ConstructorMap.empty


and typecheck_module (tyenv : Typeenv.t) (utmod : untyped_module) : signature abstracted * binding list =
  let (rng, utmodmain) = utmod in
  match utmodmain with
  | UTModVar(modchain) ->
      let mentry = find_module_chain tyenv modchain in
      let modsig = mentry.mod_signature in
      ((OpaqueIDMap.empty, modsig), [])

  | UTModBinds(utbinds) ->
      let ((quant, ssig), binds) = typecheck_binding_list tyenv utbinds in
      ((quant, ConcStructure(ssig)), binds)

  | UTModFunctor(modident1, utsig1, utmod2) ->
      let (_, modnm1) = modident1 in
      let absmodsig1 = typecheck_signature tyenv utsig1 in
      let (quant1, modsig1) = absmodsig1 in
      let (absmodsig2, _binds2) =
        let mentry1 = { mod_signature = modsig1; } in
        let tyenv = tyenv |> Typeenv.add_module modnm1 mentry1 in
        typecheck_module tyenv utmod2
      in
      let fsig =
        {
          opaques  = quant1;
          domain   = modsig1;
          codomain = absmodsig2;
          closure  = Some((modident1, utmod2, tyenv));
        }
      in
      let absmodsig = (OpaqueIDMap.empty, ConcFunctor(fsig)) in
      (absmodsig, [])

  | UTModApply(modchain1, modchain2) ->
      let mentry1 = find_module_chain tyenv modchain1 in
      let mentry2 = find_module_chain tyenv modchain2 in
      begin
        match mentry1.mod_signature with
        | ConcStructure(ssig) ->
            let rng = make_range_from_module_chain modchain1 in
            raise_error (NotAFunctorSignature(rng, ssig))

        | ConcFunctor(fsig1) ->
            begin
              match fsig1 with
              | {
                  opaques  = quant1;
                  domain   = modsig_dom1;
                  codomain = absmodsig_cod1;
                  closure  = Some(((_, modnm0), utmod0, tyenv0))
                } ->
                  let modsig2 = mentry2.mod_signature in
                  let subst = subtype_concrete_with_abstract rng modsig2 (quant1, modsig_dom1) in
                  let ((_, modsig0), binds) =
                    let mentry0 = { mod_signature = modsig2; } in
                    typecheck_module (tyenv0 |> Typeenv.add_module modnm0 mentry0) utmod0
                  in
                  let (quant1_subst, modsig_cod1_subst) = absmodsig_cod1 |> substitute_abstract subst in
                  let absmodsig = (quant1_subst, copy_contents modsig0 modsig_cod1_subst) in
                  (absmodsig, binds)

              | _ ->
                  assert false
            end
      end

  | UTModCoerce(modident1, utsig2) ->
      let mentry1 = find_module tyenv modident1 in
      let modsig1 = mentry1.mod_signature in
      let absmodsig2 = typecheck_signature tyenv utsig2 in
      let absmodsig = coerce_signature rng modsig1 absmodsig2 in
      (absmodsig, [])


and typecheck_signature (tyenv : Typeenv.t) (utsig : untyped_signature) : signature abstracted =
  let (rng, utsigmain) = utsig in
  match utsigmain with
  | UTSigVar(signm) ->
      begin
        match tyenv |> Typeenv.find_signature signm with
        | None            -> raise_error (UndefinedSignatureName(rng, signm))
        | Some(absmodsig) -> absmodsig
      end

  | UTSigPath(modchain1, (rng_signm, signm2)) ->
      let mentry1 = find_module_chain tyenv modchain1 in
      begin
        match mentry1.mod_signature with
        | ConcFunctor(fsig) ->
            let rng = make_range_from_module_chain modchain1 in
            raise_error (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig1) ->
            begin
              match ssig1 |> StructSig.find_signature signm2 with
              | None             -> raise_error (UndefinedSignatureName(rng_signm, signm2))
              | Some(absmodsig2) -> absmodsig2
            end
      end

  | UTSigDecls(utdecls) ->
      let (quant, ssig) = typecheck_declaration_list tyenv utdecls in
      (quant, ConcStructure(ssig))

  | UTSigFunctor((_, modnm), utsig1, utsig2) ->
      let (quant1, modsig1) = typecheck_signature tyenv utsig1 in
      let absmodsig2 =
        let mentry = { mod_signature = modsig1; } in
        let tyenv = tyenv |> Typeenv.add_module modnm mentry in
        typecheck_signature tyenv utsig2
      in
      let fsig =
        {
          opaques  = quant1;
          domain   = modsig1;
          codomain = absmodsig2;
          closure  = None;
        }
      in
      (OpaqueIDMap.empty, ConcFunctor(fsig))

  | UTSigWith(utsig0, modidents, tybinds) ->
      let (quant0, modsig0) = typecheck_signature tyenv utsig0 in
      let ssig =
        match modsig0 with
        | ConcFunctor(fsig) ->
            raise_error (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig0) ->
            modidents |> List.fold_left (fun ssig (rng, modnm) ->
              match ssig |> StructSig.find_module modnm with
              | None ->
                  raise_error (UndefinedModuleName(rng, modnm))

              | Some(mentry) ->
                  begin
                    match mentry.mod_signature with
                    | ConcFunctor(fsig)   -> raise_error (NotAStructureSignature(rng, fsig))
                    | ConcStructure(ssig) -> ssig
                  end
            ) ssig0
      in
      let (tydefs, _ctordefs) = bind_types tyenv tybinds in
      let (subst, quant) =
        tydefs |> List.fold_left (fun (subst, quant) (tynm, tentry) ->
          let (tyid, kd_expected) =
            match ssig |> StructSig.find_type tynm with
            | None ->
                raise_error (UndefinedTypeName(rng, tynm))

            | Some(tentry) ->
                begin
                  match TypeConv.get_opaque_type tentry.type_scheme with
                  | None ->
                      raise_error (CannotRestrictTransparentType(rng, tynm))

                  | Some(tyid) ->
                      assert (quant |> OpaqueIDMap.mem tyid);
                      (tyid, tentry.type_kind)
                end
          in
          let kd_actual = tentry.type_kind in
          if kind_equal kd_expected kd_actual then
            let subst = subst |> SubstMap.add tyid tentry.type_scheme in
            let quant = quant |> OpaqueIDMap.remove tyid in
            (subst, quant)
          else
            raise_error (KindContradiction(rng, tynm, kd_expected, kd_actual))
        ) (SubstMap.empty, quant0)
      in
      let modsig = modsig0 |> substitute_concrete subst in
        (* TODO: use `ctordefs` to update `modsig` *)
      (quant, modsig)


and lookup_type_entry (tentry1 : type_entry) (tentry2 : type_entry) : substitution option =
  let Kind(bkds1) = tentry1.type_kind in
  let Kind(bkds2) = tentry2.type_kind in
  if List.length bkds1 = List.length bkds2 then
    let subst =
      match TypeConv.get_opaque_type tentry2.type_scheme with
      | None        -> SubstMap.empty
      | Some(tyid2) -> SubstMap.empty |> SubstMap.add tyid2 tentry1.type_scheme
    in
    Some(subst)
  else
    None


and lookup_struct (rng : Range.t) (modsig1 : signature) (modsig2 : signature) : substitution =
  let take_left = (fun _tyid to1 _to2 -> Some(to1)) in
  match (modsig1, modsig2) with
  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun _x2 _ventry2 subst ->
            subst
          )
          ~a:(fun _csnm _macentry2 subst ->
            subst
          )
          ~c:(fun _ctornm2 _centry2 subst ->
            subst
          )
          ~f:(fun _tynm2 _pty subst ->
            subst
          )
          ~t:(fun tynm2 tentry2 subst ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let (bids, _) = tentry2.type_scheme in
                raise_error (MissingRequiredTypeName(rng, tynm2, List.length bids))

            | Some(tentry1) ->
                begin
                  match lookup_type_entry tentry1 tentry2 with
                  | None ->
                      raise_error (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))

                  | Some(subst0) ->
                      SubstMap.union take_left subst0 subst
                end
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } subst ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                raise_error (MissingRequiredModuleName(rng, modnm2, modsig2))

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
  let (quant, modsig) = absmodsig in
  let modsig = substitute_concrete subst modsig in
  (quant, modsig)
    (* Strictly speaking, we should assert that `quant` and the domain of `subst` be disjoint. *)


and substitute_concrete (subst : substitution) (modsig : signature) : signature =
  match modsig with
  | ConcFunctor(fsig) ->
      let
        {
          opaques = quant;
          domain  = modsig1;
          codomain = absmodsig2
        } = fsig
      in
      let modsig1 = modsig1 |> substitute_concrete subst in
      let absmodsig2 = absmodsig2 |> substitute_abstract subst in
      let fsig =
        {
          opaques  = quant;
          domain   = modsig1;
          codomain = absmodsig2;
          closure  = failwith "TODO";
        }
      in
      ConcFunctor(fsig)
        (* Strictly speaking, we should assert that `quant` and the domain of `subst` be disjoint. *)

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
      | VertCommandType(pargs) -> VertCommandType(pargs |> List.map aux_command_arg)
      | MathCommandType(pargs) -> MathCommandType(pargs |> List.map aux_command_arg)

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

                  | Some(Poly((_, ptymain))) ->
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


and substitute_type_id (subst : substitution) (tyid_from : TypeID.t) : TypeID.t =
  match subst |> SubstMap.find_opt tyid_from with
  | None ->
      tyid_from

  | Some(tyscheme) ->
      begin
        match TypeConv.get_opaque_type tyscheme with
        | None          -> assert false
        | Some(tyid_to) -> tyid_to
      end


and substitute_macro_type (subst : substitution) (pmacty : poly_macro_type) : poly_macro_type =
  match pmacty with
  | HorzMacroType(pmacparamtys) -> HorzMacroType(pmacparamtys |> List.map (substitute_macro_parameter_type subst))
  | VertMacroType(pmacparamtys) -> VertMacroType(pmacparamtys |> List.map (substitute_macro_parameter_type subst))


and substitute_macro_parameter_type (subst : substitution) = function
  | LateMacroParameter(pty) ->
      let Poly(pty) = Poly(pty) |> substitute_poly_type subst in
      LateMacroParameter(pty)

  | EarlyMacroParameter(pty) ->
      let Poly(pty) = Poly(pty) |> substitute_poly_type subst in
      EarlyMacroParameter(pty)


and substitute_struct (subst : substitution) (ssig : StructSig.t) : StructSig.t =
  ssig |> StructSig.map
      ~v:(fun _x ventry ->
        { ventry with val_type = ventry.val_type |> substitute_poly_type subst }
      )
      ~a:(fun csnm macentry ->
        { macentry with macro_type = macentry.macro_type |> substitute_macro_type subst }
      )
      ~c:(fun _ctornm centry ->
        let (bids, pty_body) = centry.ctor_parameter in
        {
          ctor_belongs_to = centry.ctor_belongs_to |> substitute_type_id subst;
          ctor_parameter  = (bids, pty_body |> substitute_poly_type subst);
        }
      )
      ~f:(fun _tynm pty ->
        pty |> substitute_poly_type subst
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
        { mod_signature = modsig; }
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
          opaques  = quant1;
          domain   = modsigdom1;
          codomain = absmodsigcod1;
        } = fsig1
      in
      let
        {
          opaques  = quant2;
          domain   = modsigdom2;
          codomain = absmodsigcod2;
        } = fsig2
      in
      let subst = subtype_concrete_with_abstract rng modsigdom2 (quant1, modsigdom1) in
      let absmodsigcod1 = absmodsigcod1 |> substitute_abstract subst in
      subtype_abstract_with_abstract rng absmodsigcod1 absmodsigcod2

  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun x2 { val_type = pty2; val_stage = stage2; _ } () ->
            match ssig1 |> StructSig.find_value x2 with
            | None ->
                raise_error (MissingRequiredValueName(rng, x2, pty2))

            | Some({ val_type = pty1; val_stage = stage1; _ }) ->
                match (stage1, stage2) with
                | (Persistent0, Persistent0)
                | (Persistent0, Stage0)
                | (Persistent0, Stage1)
                | (Stage0, Stage0)
                | (Stage1, Stage1) ->
                    if subtype_poly_type pty1 pty2 then
                      ()
                    else
                      raise_error (NotASubtypeAboutValue(rng, x2, pty1, pty2))

                | _ ->
                    raise_error (NotASubtypeAboutValueStage(rng, x2, stage1, stage2))
          )
          ~a:(fun csnm2 { macro_type = macty2; _ } () ->
            match ssig1 |> StructSig.find_macro csnm2 with
            | None ->
                raise_error (MissingRequiredMacroName(rng, csnm2, macty2))

            | Some({ macro_type = macty1; _ }) ->
                if subtype_macro_type macty1 macty2 then
                  ()
                else
                  raise_error (NotASubtypeAboutMacro(rng, csnm2, macty1, macty2))
          )
          ~c:(fun ctornm2 centry2 () ->
            match ssig1 |> StructSig.find_constructor ctornm2 with
            | None ->
                raise_error (MissingRequiredConstructorName(rng, ctornm2, centry2))

            | Some(centry1) ->
                let tyscheme1 = centry1.ctor_parameter in
                let tyscheme2 = centry2.ctor_parameter in
                if subtype_type_scheme tyscheme1 tyscheme2 then
                  ()
                else
                  raise_error (NotASubtypeAboutConstructor(rng, ctornm2, tyscheme1, tyscheme2))
          )
          ~f:(fun tynm2 pty2 () ->
            match ssig1 |> StructSig.find_dummy_fold tynm2 with
            | None ->
                begin
                  match ssig2 |> StructSig.find_type tynm2 with
                  | None ->
                      assert false

                  | Some(tentry2) ->
                      let (bids, _) = tentry2.type_scheme in
                      let arity = List.length bids in
                      raise_error (MissingRequiredTypeName(rng, tynm2, arity))
                end

            | Some(pty1) ->
                if subtype_poly_type pty1 pty2 then
                  ()
                else
                  begin
                    match (ssig1 |> StructSig.find_type tynm2, ssig2 |> StructSig.find_type tynm2) with
                    | (Some(tentry1), Some(tentry2)) ->
                        raise_error (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))

                    | _ ->
                        assert false
                  end
          )
          ~t:(fun tynm2 tentry2 () ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let arity =
                  let (bids, _) = tentry2.type_scheme in
                  List.length bids
                in
                raise_error (MissingRequiredTypeName(rng, tynm2, arity))

            | Some(tentry1) ->
                let tyscheme1 = tentry1.type_scheme in
                let tyscheme2 = tentry2.type_scheme in
                let b1 = subtype_type_scheme tyscheme1 tyscheme2 in
                let b2 = subtype_type_scheme tyscheme2 tyscheme1 in
                if b1 && b2 then
                  ()
                else
                  raise_error (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } () ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                raise_error (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some({ mod_signature = modsig1; _ }) ->
                subtype_concrete_with_concrete rng modsig1 modsig2
          )
          ~s:(fun signm2 absmodsig2 () ->
            match ssig1 |> StructSig.find_signature signm2 with
            | None ->
                raise_error (MissingRequiredSignatureName(rng, signm2, absmodsig2))

            | Some(absmodsig1) ->
                subtype_abstract_with_abstract rng absmodsig1 absmodsig2;
                subtype_abstract_with_abstract rng absmodsig2 absmodsig1;
                ()
          )
          ()

  | _ ->
      raise_error (NotASubtypeSignature(rng, modsig1, modsig2))


and subtype_concrete_with_abstract (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : substitution =
  let (_quant2, modsig2) = absmodsig2 in
  let subst = lookup_struct rng modsig1 modsig2 in
  let modsig2 = modsig2 |> substitute_concrete subst in
  subtype_concrete_with_concrete rng modsig1 modsig2;
  subst


and subtype_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) =
  subtype_concrete_with_abstract rng modsig1 absmodsig2


and subtype_poly_type_impl (internbid : type_intern) (internbrid : row_intern) (Poly(pty1) : poly_type) (Poly(pty2) : poly_type) : bool =
  let rec aux (pty1 : poly_type_body) (pty2 : poly_type_body) =
    let (_, ptymain1) = pty1 in
    let (_, ptymain2) = pty2 in
    match (ptymain1, ptymain2) with
    | (TypeVariable(PolyFree(_)), _)
    | (_, TypeVariable(PolyFree(_))) ->
        false

    | (TypeVariable(PolyBound(bid1)), _) ->
        internbid bid1 (Poly(pty2))

    | (FuncType(poptrow1, ptydom1, ptycod1), FuncType(poptrow2, ptydom2, ptycod2)) ->
        subtype_row_with_equal_domain internbid internbrid poptrow1 poptrow2 &&
          aux ptydom1 ptydom2 && aux ptycod1 ptycod2

    | (ProductType(ptys1), ProductType(ptys2)) ->
        aux_list (TupleList.to_list ptys1) (TupleList.to_list ptys2)

    | (RecordType(prow1), RecordType(prow2)) ->
        subtype_row_with_equal_domain internbid internbrid prow1 prow2

    | (DataType(ptys1, tyid1), DataType(ptys2, tyid2)) ->
        if TypeID.equal tyid1 tyid2 then
          aux_list ptys1 ptys2
        else
          false

    | (ListType(pty1), ListType(pty2)) -> aux pty1 pty2
    | (RefType(pty1), RefType(pty2))   -> aux pty1 pty2
    | (BaseType(bty1), BaseType(bty2)) -> bty1 = bty2

    | (HorzCommandType(cmdargtys1), HorzCommandType(cmdargtys2)) -> aux_cmd_list cmdargtys1 cmdargtys2
    | (VertCommandType(cmdargtys1), VertCommandType(cmdargtys2)) -> aux_cmd_list cmdargtys1 cmdargtys2
    | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2)) -> aux_cmd_list cmdargtys1 cmdargtys2

    | (CodeType(pty1), CodeType(pty2)) ->
        aux pty1 pty2

    | _ ->
        false

  and aux_list (ptys1 : poly_type_body list) (ptys2 : poly_type_body list) =
    match List.combine ptys1 ptys2 with
    | exception Invalid_argument(_) -> false
    | zipped                        -> zipped |> List.for_all (fun (pty1, pty2) -> aux pty1 pty2)

  and aux_cmd_list (cmdargtys1 : poly_command_argument_type list) (cmdargtys2 : poly_command_argument_type list) =
    match List.combine cmdargtys1 cmdargtys2 with
    | exception Invalid_argument(_) ->
        false

    | zipped ->
        zipped |> List.for_all (fun (cmdargty1, cmdargty2) ->
          let CommandArgType(pty_labmap1, pty1) = cmdargty1 in
          let CommandArgType(pty_labmap2, pty2) = cmdargty2 in
          subtype_label_map_with_equal_domain internbid internbrid pty_labmap1 pty_labmap2 && aux pty1 pty2
        )

  in
  aux pty1 pty2


and subtype_row_with_equal_domain (internbid : type_intern) (internbrid : row_intern) (prow1 : poly_row) (prow2 : poly_row) : bool =
  let NormalizedRow(pty_labmap1, rowvar1_opt) = TypeConv.normalize_poly_row prow1 in
  let NormalizedRow(pty_labmap2, rowvar2_opt) = TypeConv.normalize_poly_row prow2 in
  match (rowvar1_opt, rowvar2_opt) with
  | (None, None) ->
      subtype_label_map_with_equal_domain internbid internbrid pty_labmap1 pty_labmap2

  | (Some(PolyORFree(_)), _) | (_, Some(PolyORFree(_))) ->
      assert false

  | (None, Some(PolyORBound(_brid2))) ->
      false

  | (Some(PolyORBound(brid1)), _) ->
      let opt = subtype_label_map_inclusive internbid internbrid pty_labmap1 pty_labmap2 in
      begin
        match opt with
        | None                  -> false
        | Some(pty_labmap_diff) -> internbrid brid1 (NormalizedRow(pty_labmap_diff, rowvar2_opt))
      end


and subtype_label_map_with_equal_domain (internbid : type_intern) (internbrid : row_intern) (pty_labmap1 : poly_type_body LabelMap.t) (pty_labmap2 : poly_type_body LabelMap.t) : bool =
  LabelMap.merge (fun label pty1_opt pty2_opt ->
    match (pty1_opt, pty2_opt) with
    | (Some(pty1), Some(pty2)) -> Some(subtype_poly_type_impl internbid internbrid (Poly(pty1)) (Poly(pty2)))
    | _                        -> Some(false)
  ) pty_labmap1 pty_labmap2 |> LabelMap.for_all (fun _label b -> b)


(* Checks that `dom pty_labmap1 ⊆ dom pty_labmap2` and
   `∀label ∈ dom pty_labmap1. pty_labmap1(label) <: pty_labmap2(label)`
   by referring and updating `internbid` and `internbrid`. *)
and subtype_label_map_inclusive (internbid : type_intern) (internbrid : row_intern) (pty_labmap1 : poly_type_body LabelMap.t) (pty_labmap2 : poly_type_body LabelMap.t) : (poly_type_body LabelMap.t) option =
  let merged =
    LabelMap.merge (fun label pty1_opt pty2_opt ->
      match (pty1_opt, pty2_opt) with
      | (Some(pty1), Some(pty2)) -> Some(Ok(subtype_poly_type_impl internbid internbrid (Poly(pty1)) (Poly(pty2))))
      | (None, Some(pty2))       -> Some(Error(pty2))
      | _                        -> Some(Ok(false))
    ) pty_labmap1 pty_labmap2
  in
  if merged |> LabelMap.for_all (fun _label res -> Result.value ~default:true res) then
    let pty_labmap_diff =
      merged |> LabelMap.filter_map (fun _label res ->
        match res with
        | Ok(_)       -> None
        | Error(pty2) -> Some(pty2)
      )
    in
    Some(pty_labmap_diff)
  else
    None


and subtype_poly_type (pty1 : poly_type) (pty2 : poly_type) : bool =
  let bid_ht = BoundIDHashTable.create 32 in
  let brid_ht = BoundRowIDHashTable.create 32 in
  let internbid (bid1 : BoundID.t) (pty2 : poly_type) : bool =
    match BoundIDHashTable.find_opt bid_ht bid1 with
    | None ->
        BoundIDHashTable.add bid_ht bid1 pty2;
        true

    | Some(pty) ->
        poly_type_equal pty pty2
  in
  let internbrid (brid1 : BoundRowID.t) (nomprow2 : normalized_poly_row) : bool =
    match BoundRowIDHashTable.find_opt brid_ht brid1 with
    | None ->
        BoundRowIDHashTable.add brid_ht brid1 nomprow2;
        true

    | Some(nomprow) ->
        normalized_poly_row_equal nomprow nomprow2
  in
  subtype_poly_type_impl internbid internbrid pty1 pty2


and subtype_type_scheme (tyscheme1 : type_scheme) (tyscheme2 : type_scheme) : bool =
  let (typarams1, pty1) = tyscheme1 in
  let (typarams2, pty2) = tyscheme2 in
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
      let internbrid (brid1 : BoundRowID.t) (nomprow2 : normalized_poly_row) : bool =
        false
      in
      subtype_poly_type_impl internbid internbrid pty1 pty2


and poly_row_equal (prow1 : poly_row) (prow2 : poly_row) : bool =
  normalized_poly_row_equal (TypeConv.normalize_poly_row prow1) (TypeConv.normalize_poly_row prow2)


and normalized_poly_row_equal (nomrow1 : normalized_poly_row) (nomrow2 : normalized_poly_row) : bool =
  let NormalizedRow(plabmap1, rowvar1_opt) = nomrow1 in
  let NormalizedRow(plabmap2, rowvar2_opt) = nomrow2 in
  let bmap =
    LabelMap.merge (fun _ ptyopt1 ptyopt2 ->
      match (ptyopt1, ptyopt2) with
      | (None, None)             -> None
      | (Some(pty1), Some(pty2)) -> Some(poly_type_equal (Poly(pty1)) (Poly(pty2)))
      | _                        -> Some(false)
    ) plabmap1 plabmap2 |> LabelMap.for_all (fun _label b -> b)
  in
  bmap && begin
    match (rowvar1_opt, rowvar2_opt) with
    | (None, None)                                         -> true
    | (Some(PolyORBound(brid1)), Some(PolyORBound(brid2))) -> BoundRowID.equal brid1 brid2
    | _                                                    -> false
  end


and poly_type_equal (Poly(pty1) : poly_type) (Poly(pty2) : poly_type) : bool =
  let rec aux (pty1 : poly_type_body) (pty2 : poly_type_body) =
    let (_, ptymain1) = pty1 in
    let (_, ptymain2) = pty2 in
    match (ptymain1, ptymain2) with
    | (BaseType(bty1), BaseType(bty2)) ->
        bty1 = bty2

    | (FuncType(poptrow1, ptydom1, ptycod1), FuncType(poptrow2, ptydom2, ptycod2)) ->
        poly_row_equal poptrow1 poptrow2 && aux ptydom1 ptydom2 && aux ptycod1 ptycod2

    | (ListType(pty1), ListType(pty2)) ->
        aux pty1 pty2

    | (RefType(pty1), RefType(pty2)) ->
        aux pty1 pty2

    | (ProductType(ptys1), ProductType(ptys2)) ->
        aux_list (TupleList.to_list ptys1) (TupleList.to_list ptys2)

    | (TypeVariable(PolyBound(bid1)), TypeVariable(PolyBound(bid2))) ->
        BoundID.equal bid1 bid2

    | (TypeVariable(PolyFree(_)), _)
    | (_, TypeVariable(PolyFree(_))) ->
        false

    | (DataType(ptys1, tyid1), DataType(ptys2, tyid2)) ->
        TypeID.equal tyid1 tyid2 && aux_list ptys1 ptys2

    | (RecordType(prow1), RecordType(prow2)) ->
        poly_row_equal prow1 prow2

    | (HorzCommandType(cmdargtys1), HorzCommandType(cmdargtys2))
    | (VertCommandType(cmdargtys1), HorzCommandType(cmdargtys2))
    | (MathCommandType(cmdargtys1), HorzCommandType(cmdargtys2)) ->
        aux_cmd_list cmdargtys1 cmdargtys2

    | (CodeType(pty1), CodeType(pty2)) ->
        aux pty1 pty2

    | _ ->
        false

  and aux_list (ptys1 : poly_type_body list) (ptys2 : poly_type_body list) : bool =
    try
      List.fold_left2 (fun b pty1 pty2 ->
        b && aux pty1 pty2
      ) true ptys1 ptys2
    with
    | Invalid_argument(_) ->
        false

  and aux_cmd_list (cmdargtys1 : poly_command_argument_type list) (cmdargtys2 : poly_command_argument_type list) : bool =
    try
      List.fold_left2 (fun b cmdargty1 cmdargty2 ->
        b && begin
          let CommandArgType(ptylabmap1, pty1) = cmdargty1 in
          let CommandArgType(ptylabmap2, pty2) = cmdargty2 in
          aux_labeled_map ptylabmap1 ptylabmap2 && aux pty1 pty2
        end
      ) true cmdargtys1 cmdargtys2
    with
    | Invalid_argument(_) ->
        false

  and aux_labeled_map ptylabmap1 ptylabmap2 =
    let labmap =
      LabelMap.merge (fun label pty1_opt pty2_opt ->
        match (pty1_opt, pty2_opt) with
        | (Some(pty1), Some(pty2)) -> Some(aux pty1 pty2)
        | _                        -> Some(false)
      ) ptylabmap1 ptylabmap2
    in
    labmap |> LabelMap.for_all (fun _label b -> b)

  in
  aux pty1 pty2


and kind_equal (kd1 : kind) (kd2 : kind) : bool =
  let Kind(bkds1) = kd1 in
  let Kind(bkds2) = kd2 in
  match List.combine bkds1 bkds2 with
  | exception Invalid_argument(_) -> false
  | zipped                        -> zipped |> List.for_all (fun (TypeKind, TypeKind) -> true)


and subtype_macro_type (macty1 : poly_macro_type) (macty2 : poly_macro_type) : bool =
  let bid_ht = BoundIDHashTable.create 32 in
  let brid_ht = BoundRowIDHashTable.create 32 in
  let internbid (bid1 : BoundID.t) (pty2 : poly_type) : bool =
    match BoundIDHashTable.find_opt bid_ht bid1 with
    | None ->
        BoundIDHashTable.add bid_ht bid1 pty2;
        true

    | Some(pty) ->
        poly_type_equal pty pty2
  in
  let internbrid (brid1 : BoundRowID.t) (nomprow2 : normalized_poly_row) : bool =
    match BoundRowIDHashTable.find_opt brid_ht brid1 with
    | None ->
        BoundRowIDHashTable.add brid_ht brid1 nomprow2;
        true

    | Some(nomprow) ->
        normalized_poly_row_equal nomprow nomprow2
  in
  let aux macparamtys1 macparamtys2 =
    match List.combine macparamtys1 macparamtys2 with
    | exception Invalid_argument(_) ->
        false

    | zipped ->
        zipped |> List.for_all (function
        | (LateMacroParameter(pty1), LateMacroParameter(pty2)) ->
            subtype_poly_type_impl internbid internbrid (Poly(pty1)) (Poly(pty2))

        | (EarlyMacroParameter(pty1), EarlyMacroParameter(pty2)) ->
            subtype_poly_type_impl internbid internbrid (Poly(pty1)) (Poly(pty2))

        | _ ->
            false
        )
  in
  match (macty1, macty2) with
  | (HorzMacroType(macparamtys1), HorzMacroType(macparamtys2)) -> aux macparamtys1 macparamtys2
  | (VertMacroType(macparamtys1), VertMacroType(macparamtys2)) -> aux macparamtys1 macparamtys2
  | _                                                          -> false


(* Given `modsig1` and `modsig2` which are already known to satisfy `modsig1 <= modsig2`,
   `copy_contents` copies every target name occurred in `modsig1`
   into the corresponding occurrence in `modsig2`. *)
and copy_contents (modsig1 : signature) (modsig2 : signature) : signature =
  match (modsig1, modsig2) with
  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      let ssig2_new = copy_closure_in_structure ssig1 ssig2 in
      (ConcStructure(ssig2_new))

  | (ConcFunctor(fsig1), ConcFunctor(fsig2)) ->
      let { opaques = quant_dom1; domain = modsig_dom1; codomain = absmodsig_cod1; closure = closure } = fsig1 in
      let { opaques = quant_dom2; domain = modsig_dom2; codomain = absmodsig_cod2 } = fsig2 in
      let modsig_dom2_new = copy_contents modsig_dom1 modsig_dom2 in
      let absmodsig_cod2_new =
        let (_, modsig_cod1) = absmodsig_cod1 in
        let (quant_cod2, modsig_cod2) = absmodsig_cod2 in
        let modsig_cod2_new = copy_contents modsig_cod1 modsig_cod2 in
        (quant_cod2, modsig_cod2_new)
      in
      ConcFunctor({ fsig2 with
        domain   = modsig_dom2_new;
        codomain = absmodsig_cod2_new;
        closure  = closure;
      })

  | _ ->
      assert false


and copy_closure_in_structure (ssig1 : StructSig.t) (ssig2 : StructSig.t) : StructSig.t =
  ssig2 |> StructSig.map
    ~v:(fun x ventry2 ->
      match ssig1 |> StructSig.find_value x with
      | None          -> assert false
      | Some(ventry1) -> { ventry2 with val_name = ventry1.val_name }
    )
    ~a:(fun csnm macentry2 ->
      match ssig1 |> StructSig.find_macro csnm with
      | None            -> assert false
      | Some(macentry1) -> { macentry2 with macro_name = macentry1.macro_name }
    )
    ~c:(fun _ctornm centry2 -> centry2)
    ~f:(fun _tynm pty2 -> pty2)
    ~t:(fun _tynm tentry2 -> tentry2)
    ~m:(fun modnm mentry2 ->
      match ssig1 |> StructSig.find_module modnm with
      | None          -> assert false
      | Some(mentry1) -> { mod_signature = copy_contents mentry1.mod_signature mentry2.mod_signature }
    )
    ~s:(fun _signm sentry2 -> sentry2)


and coerce_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : signature abstracted =
  let _ = subtype_signature rng modsig1 absmodsig2 in
  let (quant2, modsig2) = absmodsig2 in
  (quant2, copy_contents modsig1 modsig2)


and add_to_type_environment_by_signature (ssig : StructSig.t) (tyenv : Typeenv.t) =
  ssig |> StructSig.fold
    ~v:(fun x ventry -> Typeenv.add_value x ventry)
    ~a:(fun csnm macentry -> Typeenv.add_macro csnm macentry)
    ~c:(fun ctornm centry -> Typeenv.add_constructor ctornm centry)
    ~f:(fun _tynm _pty tyenv -> tyenv)
    ~t:(fun tynm tentry -> Typeenv.add_type tynm tentry)
    ~m:(fun modnm mentry -> Typeenv.add_module modnm mentry)
    ~s:(fun signm absmodsig -> Typeenv.add_signature signm absmodsig)
    tyenv


and typecheck_declaration_list (tyenv : Typeenv.t) (utdecls : untyped_declaration list) : StructSig.t abstracted =
  let (quantacc, ssigacc, _) =
    utdecls |> List.fold_left (fun (quantacc, ssigacc, tyenv) utdecl ->
      let (quant, ssig) = typecheck_declaration tyenv utdecl in
      let quantacc = unify_quantifier quantacc quant in
      let ssigacc =
        match StructSig.union ssigacc ssig with
        | Ok(ssigacc) -> ssigacc
        | Error(s)    -> raise_error (ConflictInSignature(Range.dummy "TODO (error): add range to declarations", s))
      in
      let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
      (quantacc, ssigacc, tyenv)
    ) (OpaqueIDMap.empty, StructSig.empty, tyenv)
  in
  (quantacc, ssigacc)


and typecheck_declaration (tyenv : Typeenv.t) (utdecl : untyped_declaration) : StructSig.t abstracted =
  match utdecl with
  | UTDeclValue(stage, (_, x), (typarams, rowparams), mty) ->
      let pre =
        let (typarammap, _) = TypeParameterMap.empty |> add_type_parameters (Level.succ Level.bottom) typarams in
        let (rowparammap, _) = RowParameterMap.empty |> add_row_parameters (Level.succ Level.bottom) rowparams in
        {
          stage           = stage;
          level           = Level.succ Level.bottom;
          type_parameters = typarammap;
          row_parameters  = rowparammap;
          quantifiability = Quantifiable;
        }
      in
      let ty = decode_manual_type pre tyenv mty in
      let pty = TypeConv.generalize Level.bottom ty in
      let ventry =
        {
          val_type  = pty;
          val_name  = None;
          val_stage = stage;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_value x ventry in
      (OpaqueIDMap.empty, ssig)

  | UTDeclTypeOpaque((_, tynm), mnkd) ->
      let pre_init =
        {
          stage           = Stage0;
          level           = Level.bottom;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
        }
      in
      let tyid = TypeID.fresh tynm in
      let arity =
        let MKind(mnbkds, _) = mnkd in
        List.length mnbkds
      in
      let kd = decode_manual_kind pre_init tyenv mnkd in
      let tentry =
        {
          type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
          type_kind   = kd;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_type tynm tentry in
      (OpaqueIDMap.singleton tyid kd, ssig)

  | UTDeclModule((_, modnm), utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let (quant, modsig) = absmodsig in
      let mentry = { mod_signature = modsig; } in
      let ssig = StructSig.empty |> StructSig.add_module modnm mentry in
      (quant, ssig)

  | UTDeclSignature((_, signm), utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      (OpaqueIDMap.empty, ssig)

  | UTDeclInclude(utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(fsig) ->
            let (rng, _) = utsig in
            raise_error (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig) ->
            (quant, ssig)
      end

  | UTDeclMacro((rng_cs, csnm), (_, mmacty)) ->
      let pre_init =
        {
          stage           = Stage0;
          level           = Level.bottom;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
        }
      in
      let macty = decode_manual_macro_type pre_init tyenv mmacty in
      let pmacty = TypeConv.generalize_macro_type macty in
      let macentry = { macro_type = pmacty; macro_name = None; } in
      let ssig = StructSig.empty |> StructSig.add_macro csnm macentry in
      (OpaqueIDMap.empty, ssig)


and typecheck_binding_list (tyenv : Typeenv.t) (utbinds : untyped_binding list) : StructSig.t abstracted * binding list =
  let (binds, (quant, ssig)) =
    let (bindacc, _tyenv, quantacc, ssigacc) =
      utbinds |> List.fold_left (fun (bindacc, tyenv, quantacc, ssigacc) utbind ->
        let (binds, (quant, ssig)) = typecheck_binding tyenv utbind in
        let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
        let bindacc = Alist.append bindacc binds in
        let quantacc = unify_quantifier quantacc quant in
        match StructSig.union ssigacc ssig with
        | Ok(ssigacc) -> (bindacc, tyenv, quantacc, ssigacc)
        | Error(s)    -> let (rng, _) = utbind in raise_error (ConflictInSignature(rng, s))
      ) (Alist.empty, tyenv, OpaqueIDMap.empty, StructSig.empty)
    in
    (Alist.to_list bindacc, (quantacc, ssigacc))
  in
  ((quant, ssig), binds)


and get_dependency_on_synonym_types (known_syns : SynonymDependencyGraph.Vertex.t SynonymNameMap.t) (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : SynonymVertexSet.t =
  let hashset = SynonymVertexHashSet.create 32 in
    (* A hash set is created on every (non-partial) call. *)
  let register_if_needed (tynm : type_name) : unit =
    match known_syns |> SynonymNameMap.find_opt tynm with
    | Some(vertex) -> SynonymVertexHashSet.add hashset vertex ()
    | None         -> ()
  in
  let rec aux ((_, mtymain) : manual_type) : unit =
    match mtymain with
    | MTypeName(_ :: _, _, _) ->
        ()

    | MTypeName([], (_, tynm), mtyargs) ->
        List.iter aux mtyargs;
        register_if_needed tynm

    | MFuncType(mfields, _, mtydom, mtycod) ->
        aux_row mfields;
        aux mtydom;
        aux mtycod

    | MProductType(mtys) ->
        mtys |> TupleList.to_list |> List.iter aux

    | MRecordType(mfields, _) ->
        aux_row mfields

    | MTypeParam(typaram) ->
        ()

    | MHorzCommandType(mcmdargtys) -> mcmdargtys |> List.iter aux_cmd_arg
    | MVertCommandType(mcmdargtys) -> mcmdargtys |> List.iter aux_cmd_arg
    | MMathCommandType(mcmdargtys) -> mcmdargtys |> List.iter aux_cmd_arg

  and aux_row (mfields : (label ranged * manual_type) list) : unit =
    mfields |> List.iter (fun (_, mty) -> aux mty)

  and aux_cmd_arg (mcmdargty : manual_command_argument_type) : unit =
    let MArgType(mfields, mty) = mcmdargty in
    aux_row mfields;
    aux mty

  in
  aux mty;
  SynonymVertexHashSet.fold (fun sid () set ->
    set |> SynonymVertexSet.add sid
  ) hashset SynonymVertexSet.empty


and bind_types (tyenv : Typeenv.t) (tybinds : untyped_type_binding list) =
  let pre =
    {
      stage           = Stage0;
      type_parameters = TypeParameterMap.empty;
      row_parameters  = RowParameterMap.empty;
      quantifiability = Quantifiable;
      level           = Level.bottom;
    }
  in

  (* Registers types to the type environment and the graph for detecting cyclic dependency. *)
  let (synacc, vntacc, known_syns, graph, tyenv) =
    tybinds |> List.fold_left (fun (synacc, vntacc, known_syns, graph, tyenv) tybind ->
      let (tyident, typarams, syn_or_vnt) = tybind in
      let (rng, tynm) = tyident in
      match syn_or_vnt with
      | UTBindSynonym(synbind) ->
          let data =
            SynonymDependencyGraph.{
              position        = rng;
              type_variables  = typarams;
              definition_body = synbind;
            }
          in
          let (graph, vertex) =
            match graph |> SynonymDependencyGraph.add_vertex tynm data with
            | Error((data_prev, _)) ->
                let rng_prev = data_prev.SynonymDependencyGraph.position in
                raise_error (MultipleSynonymTypeDefinition(tynm, rng_prev, rng))

            | Ok(pair) ->
                pair
          in
          let synacc = Alist.extend synacc (tyident, typarams, synbind, vertex) in
          (synacc, vntacc, known_syns |> SynonymNameMap.add tynm vertex, graph, tyenv)

      | UTBindVariant(vntbind) ->
          let tyid = TypeID.fresh tynm in
          let arity = List.length typarams in
          let tentry =
            {
              type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
              type_kind   = Kind(List.init arity (fun _ -> TypeKind));
            }
          in
          let tyenv = tyenv |> Typeenv.add_type tynm tentry in
          let vntacc = Alist.extend vntacc (tyident, typarams, vntbind, tyid, tentry) in
          (synacc, vntacc, known_syns, graph, tyenv)
    ) (Alist.empty, Alist.empty, SynonymNameMap.empty, SynonymDependencyGraph.empty, tyenv)
  in

  (* Traverse each definition of the synonym types and extract dependencies between them. *)
  let graph =
    synacc |> Alist.to_list |> List.fold_left (fun graph syn ->
      let ((_, tynm), tyvars, synbind, vertex) = syn in
      let dependencies = get_dependency_on_synonym_types known_syns pre tyenv synbind in
      let graph =
        graph |> SynonymVertexSet.fold (fun vertex_dep graph ->
          graph |> SynonymDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep
        ) dependencies
      in
      graph
    ) graph
  in

  (* Check that no cyclic dependency exists among synonym types. *)
  let syns =
    match SynonymDependencyGraph.topological_sort graph with
    | Error(cycle) -> raise_error (CyclicSynonymTypeDefinition(cycle))
    | Ok(syns)     -> syns
  in

  (* Add the definition of the synonym types to the type environment. *)
  let (tyenv, tydefacc) =
    syns |> List.fold_left (fun (tyenv, tydefacc) syn ->
      let (tynm, syndata) = syn in
      let
        SynonymDependencyGraph.{
          type_variables  = tyvars;
          definition_body = mty_body;
          _
        } = syndata
      in
      let (typarammap, bids) = pre.type_parameters |> add_type_parameters (Level.succ pre.level) tyvars in
      let pre = { pre with type_parameters = typarammap } in
      let ty_body = decode_manual_type pre tyenv mty_body in
      let pty_body = TypeConv.generalize Level.bottom ty_body in
      let tentry =
        {
          type_scheme = (bids, pty_body);
          type_kind   = Kind(bids |> List.map (fun _ -> TypeKind));
        }
      in
      let tyenv = tyenv |> Typeenv.add_type tynm tentry in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      (tyenv, tydefacc)
    ) (tyenv, Alist.empty)
  in

  (* Traverse each definition of the variant types. *)
  let (tydefacc, ctordefacc) =
    vntacc |> Alist.to_list |> List.fold_left (fun (tydefacc, ctordefacc) vnt ->
      let (tyident, tyvars, vntbind, tyid, tentry) = vnt in
      let (_, tynm) = tyident in
      let (typarammap, bids) = pre.type_parameters |> add_type_parameters (Level.succ pre.level) tyvars in
      let pre = { pre with type_parameters = typarammap } in
      let ctorbrmap = make_constructor_branch_map pre tyenv vntbind in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      let ctordefacc = Alist.extend ctordefacc (tynm, tyid, bids, ctorbrmap) in
      (tydefacc, ctordefacc)
    ) (tydefacc, Alist.empty)
  in
  (tydefacc |> Alist.to_list, ctordefacc |> Alist.to_list)


and typecheck_binding (tyenv : Typeenv.t) (utbind : untyped_binding) : binding list * StructSig.t abstracted =
  let (_, utbindmain) = utbind in
  match utbindmain with
  | UTBindValue(stage, valbind) ->
      let pre =
        {
          stage           = stage;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in
      let (rec_or_nonrecs, ssig) =
        match valbind with
        | UTNonRec(ident, utast1) ->
            let presub = { pre with level = Level.succ pre.level; } in
            let (_, varnm) = ident in
            let evid = EvalVarID.fresh ident in
            let (e1_raw, ty1) = typecheck presub tyenv utast1 in
            let e1 = e1_raw in
(*
            tyannot |> Option.map (fun mnty ->
              let tyA = decode_manual_type pre tyenv mnty in
              unify ty1 tyA
            ) |> ignore;
*)
(*
            let should_be_polymorphic = is_nonexpansive_expression e1 in
*)
            let should_be_polymorphic = true in
            let ssig =
              let pty =
                if should_be_polymorphic then
                  TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty1)
                else
                  TypeConv.lift_poly (TypeConv.erase_range_of_type ty1)
              in
              let ventry =
                {
                  val_type  = pty;
                  val_name  = Some(evid);
                  val_stage = pre.stage;
                }
              in
              StructSig.empty |> StructSig.add_value varnm ventry
            in
            ([ NonRec(evid, e1) ], ssig)

        | UTRec(utrecbinds) ->
            let quints = typecheck_letrec pre tyenv utrecbinds in
            let (recbindacc, ssig) =
              quints |> List.fold_left (fun (recbindacc, ssig) quint ->
                let (x, pty, evid, recbind) = quint in
                let ssig =
                  let ventry =
                    {
                      val_type  = pty;
                      val_name  = Some(evid);
                      val_stage = stage;
                    }
                  in
                  ssig |> StructSig.add_value x ventry
                in
                let recbindacc = Alist.extend recbindacc recbind in
                (recbindacc, ssig)
              ) (Alist.empty, StructSig.empty)
            in
            ([ Rec(recbindacc |> Alist.to_list) ], ssig)

        | UTMutable((rng, varnm) as var, utastI) ->
            let (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
            let evid = EvalVarID.fresh var in
            let pty = TypeConv.lift_poly (rng, RefType(tyI)) in
            let ssig =
              let ventry =
                {
                  val_type  = pty;
                  val_name  = Some(evid);
                  val_stage = pre.stage;
                }
              in
              StructSig.empty |> StructSig.add_value varnm ventry
            in
            ([ Mutable(evid, eI) ], ssig)
      in
      let binds = rec_or_nonrecs |> List.map (fun rec_or_nonrec -> Bind(stage, rec_or_nonrec)) in
      (binds, (OpaqueIDMap.empty, ssig))

  | UTBindType([]) ->
      assert false

  | UTBindType(tybinds) ->
      let (tydefs, ctordefs) = bind_types tyenv tybinds in
      let ssig =
        tydefs |> List.fold_left (fun ssig (tynm, tentry) ->
          ssig |> StructSig.add_type tynm tentry
        ) StructSig.empty
      in
      let ssig = ssig |> add_constructor_definitions ctordefs in
      ([], (OpaqueIDMap.empty, ssig))

  | UTBindModule(modident, utsigopt2, utmod1) ->
      let (rng_mod, modnm) = modident in
      let (absmodsig1, binds1) = typecheck_module tyenv utmod1 in
      let (quant, modsig) =
        match utsigopt2 with
        | None ->
            absmodsig1

        | Some(utsig2) ->
            let (_, modsig1) = absmodsig1 in
            let absmodsig2 = typecheck_signature tyenv utsig2 in
            coerce_signature rng_mod modsig1 absmodsig2
      in
      let ssig =
        let mentry = { mod_signature = modsig; } in
        StructSig.empty |> StructSig.add_module modnm mentry
      in
      (binds1, (quant, ssig))

  | UTBindSignature((_, signm), utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      ([], (OpaqueIDMap.empty, ssig))

  | UTBindInclude(utmod) ->
      let (absmodsig, binds) = typecheck_module tyenv utmod in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcStructure(ssig) ->
            (binds, (quant, ssig))

        | ConcFunctor(fsig) ->
            let (rng_mod, _) = utmod in
            raise_error (NotAStructureSignature(rng_mod, fsig))
      end

  | UTBindHorzMacro((rng_cs, csnm), macparams, utast1) ->
      let pre =
        {
          stage           = Stage1;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in
      let (tyenv, evids, macparamtys) = add_macro_parameters_to_type_environment tyenv pre macparams in
      let macty = HorzMacroType(macparamtys) in
      let (e1, ty1) = typecheck pre tyenv utast1 in
      unify ty1 (Range.dummy "val-inline-macro", BaseType(TextRowType));
      let evid = EvalVarID.fresh (rng_cs, csnm) in
      let ssig =
        let macentry =
          {
            macro_type = TypeConv.generalize_macro_type macty;
            macro_name = Some(evid);
          }
        in
        StructSig.empty |> StructSig.add_macro csnm macentry
      in
      let binds = [ Bind(Stage0, NonRec(evid, abstraction_list evids (Next(e1)))) ] in
      (binds, (OpaqueIDMap.empty, ssig))

  | UTBindVertMacro((rng_cs, csnm), macparams, utast1) ->
      let pre =
        {
          stage           = Stage1;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in
      let (tyenv, evids, macparamtys) = add_macro_parameters_to_type_environment tyenv pre macparams in
      let macty = HorzMacroType(macparamtys) in
      let (e1, ty1) = typecheck pre tyenv utast1 in
      unify ty1 (Range.dummy "val-block-macro", BaseType(TextColType));
      let evid = EvalVarID.fresh (rng_cs, csnm) in
      let ssig =
        let macentry =
          {
            macro_type = TypeConv.generalize_macro_type macty;
            macro_name = Some(evid);
          }
        in
        StructSig.empty |> StructSig.add_macro csnm macentry
      in
      let binds = [ Bind(Stage0, NonRec(evid, abstraction_list evids (Next(e1)))) ] in
      (binds, (OpaqueIDMap.empty, ssig))


let main (stage : stage) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) : mono_type * abstract_tree =
  let pre =
    {
      stage           = stage;
      type_parameters = TypeParameterMap.empty;
      row_parameters  = RowParameterMap.empty;
      quantifiability = Quantifiable;
      level           = Level.bottom;
    }
  in
  let (e, ty) = typecheck pre tyenv utast in
  (ty, e)


let main_bindings (tyenv : Typeenv.t) (utsig_opt : untyped_signature option) (utbinds : untyped_binding list) : StructSig.t abstracted * binding list =
  match utsig_opt with
  | None ->
      typecheck_binding_list tyenv utbinds

  | Some(utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let ((_, ssig), binds) = typecheck_binding_list tyenv utbinds in
      let rng = Range.dummy "main_bindings" in (* TODO (enhance): give appropriate ranges *)
      let (quant, modsig) = coerce_signature rng (ConcStructure(ssig)) absmodsig in
      let ssig =
        match modsig with
        | ConcFunctor(_)      -> assert false
        | ConcStructure(ssig) -> ssig
      in
      ((quant, ssig), binds)


let are_unifiable (ty1 : mono_type) (ty2 : mono_type) : bool =
  try
    unify_sub ty1 ty2;
    true
  with
  | InternalContradictionError -> false
  | InternalInclusionError     -> false
