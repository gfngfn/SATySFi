
open SyntaxBase
open Types
open StaticEnv
open TypeError
open TypecheckUtil


module PatternVarMap = Map.Make(String)

type pattern_var_map = (Range.t * EvalVarID.t * mono_type) PatternVarMap.t


let find_constructor (rng : Range.t) (tyenv : Typeenv.t) (modidents : (module_name ranged) list) (ctornm : constructor_name) : constructor_entry ok =
  let open ResultMonad in
  let* centry_opt =
    match modidents with
    | [] ->
        return (tyenv |> Typeenv.find_constructor ctornm)

    | modident0 :: proj ->
        let modchain = (modident0, proj) in
        let* mentry = find_module_chain tyenv modchain in
        let* ssig =
          match mentry.mod_signature with
          | ConcStructure(ssig) -> return ssig
          | ConcFunctor(fsig)   -> err (NotAStructureSignature(rng, fsig))
              (*TODO (error): give a better code range to this error. *)
        in
        return (ssig |> StructSig.find_constructor ctornm)
  in
  match centry_opt with
  | None ->
      let cands = [] in (* TODO (error): find candidate constructors *)
      err (UndefinedConstructor(rng, ctornm, cands))

  | Some(centry) ->
      return centry


let instantiate_constructor (pre : pre) (centry : constructor_entry) : mono_type list * TypeID.t * mono_type =
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


let find_macro (tyenv : Typeenv.t) (modidents : (module_name ranged) list) ((rng_cs, csnm) : macro_name ranged) : macro_entry ok =
  let open ResultMonad in
  match modidents with
  | [] ->
      begin
        match tyenv |> Typeenv.find_macro csnm with
        | None           -> err (UndefinedMacro(rng_cs, csnm))
        | Some(macentry) -> return macentry
      end

  | modident0 :: proj ->
      let modchain = (modident0, proj) in
      let* mentry = find_module_chain tyenv modchain in
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
            err (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig) ->
            begin
              match ssig |> StructSig.find_macro csnm with
              | None           -> err (UndefinedMacro(rng_cs, csnm))
              | Some(macentry) -> return macentry
            end
      end


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


let unite_pattern_var_map (patvarmap1 : pattern_var_map) (patvarmap2 : pattern_var_map) : pattern_var_map ok =
  let open ResultMonad in
  let resmap =
    PatternVarMap.merge (fun varnm patvar1_opt patvar2_opt ->
      match (patvar1_opt, patvar2_opt) with
      | (None, None) ->
          None

      | (Some(patvar1), None) ->
          Some(return patvar1)

      | (None, Some(patvar2)) ->
          Some(return patvar2)

      | (Some((rng1, _, _)), Some((rng2, _, _))) ->
          Some(err (MultiplePatternVariable(rng1, rng2, varnm)))

    ) patvarmap1 patvarmap2
  in
  PatternVarMap.fold (fun label patvar_res patvarmap_res ->
    patvarmap_res >>= fun patvarmap ->
    patvar_res >>= fun patvar ->
    return (patvarmap |> PatternVarMap.add label patvar)
  ) resmap (return PatternVarMap.empty)


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


(* `apply_tree_of_list` converts `e0` and `[e1; ...; eN]` into `(e0 e1 ... eN)`. *)
let apply_tree_of_list (astfunc : abstract_tree) (asts : abstract_tree list) =
  List.fold_left (fun astf astx -> Apply(LabelMap.empty, astf, astx)) astfunc asts


let fresh_type_variable (rng : Range.t) (pre : pre) : mono_type =
  let fid = fresh_free_id pre.quantifiability pre.level in
  let tvuref = ref (MonoFree(fid)) in
  (rng, TypeVariable(Updatable(tvuref)))


let base bc =
  ASTBaseConstant(bc)


let rec typecheck_pattern (pre : pre) (tyenv : Typeenv.t) ((rng, utpatmain) : untyped_pattern_tree) : (pattern_tree * mono_type * pattern_var_map) ok =
  let open ResultMonad in
  let iter = typecheck_pattern pre tyenv in
    match utpatmain with
    | UTPIntegerConstant(nc) -> return (PIntegerConstant(nc), (rng, BaseType(IntType)), PatternVarMap.empty)
    | UTPBooleanConstant(bc) -> return (PBooleanConstant(bc), (rng, BaseType(BoolType)), PatternVarMap.empty)
    | UTPUnitConstant        -> return (PUnitConstant, (rng, BaseType(UnitType)), PatternVarMap.empty)

    | UTPStringConstant(sc) ->
        return (PStringConstant(sc), (rng, BaseType(StringType)), PatternVarMap.empty)

    | UTPListCons(utpat1, utpat2) ->
        let* (epat1, typat1, patvarmap1) = iter utpat1 in
        let* (epat2, typat2, patvarmap2) = iter utpat2 in
        let* () = unify typat2 (Range.dummy "pattern-list-cons", ListType(typat1)) in
        let* patvarmap = unite_pattern_var_map patvarmap1 patvarmap2 in
        return (PListCons(epat1, epat2), typat2, patvarmap)

    | UTPEndOfList ->
        let beta = fresh_type_variable rng pre in
        return (PEndOfList, (rng, ListType(beta)), PatternVarMap.empty)

    | UTPTuple(utpats) ->
        let* tris = TupleList.mapM iter utpats in
        let epats = tris |> TupleList.map (fun (epat, _, _) -> epat) in
        let typats = tris |> TupleList.map (fun (_, typat, _) -> typat) in
        let tyres = (rng, ProductType(typats)) in
        let* patvarmap =
          let patvarmaps = tris |> TupleList.to_list |> List.map (fun (_, _, patvarmap) -> patvarmap) in
          patvarmaps |> foldM unite_pattern_var_map PatternVarMap.empty
        in
        return (PTuple(epats), tyres, patvarmap)

    | UTPWildCard ->
        let beta = fresh_type_variable rng pre in
        return (PWildCard, beta, PatternVarMap.empty)

    | UTPVariable(varnm) ->
        let beta = fresh_type_variable rng pre in
        let evid = EvalVarID.fresh (rng, varnm) in
        return (PVariable(evid), beta, PatternVarMap.empty |> PatternVarMap.add varnm (rng, evid, beta))

    | UTPAsVariable(varnm, utpat1) ->
        let beta = fresh_type_variable rng pre in
        let* (epat1, typat1, patvarmap1) = iter utpat1 in
        begin
          match PatternVarMap.find_opt varnm patvarmap1 with
          | Some((rngsub, _, _)) ->
            (* If 'varnm' also occurs in `utpat1`: *)
              err (MultiplePatternVariable(rngsub, rng, varnm))

          | None ->
              let evid = EvalVarID.fresh (rng, varnm) in
              return (PAsVariable(evid, epat1), typat1, patvarmap1 |> PatternVarMap.add varnm (rng, evid, beta))
        end

    | UTPConstructor(modidents, ctornm, utpat1) ->
        let* centry = find_constructor rng tyenv modidents ctornm in
        let (tyargs, tyid, tyc) = instantiate_constructor pre centry in
        let* (epat1, typat1, tyenv1) = iter utpat1 in
        let* () = unify tyc typat1 in
        return (PConstructor(ctornm, epat1), (rng, DataType(tyargs, tyid)), tyenv1)


let rec typecheck (pre : pre) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_abstract_tree) : (abstract_tree * mono_type) ok =
  let open ResultMonad in
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
  | UTIntegerConstant(nc) -> return (base (BCInt(nc))   , (rng, BaseType(IntType))   )
  | UTFloatConstant(nc)   -> return (base (BCFloat(nc)) , (rng, BaseType(FloatType)) )
  | UTStringConstant(sc)  -> return (base (BCString(sc)), (rng, BaseType(StringType)))
  | UTBooleanConstant(bc) -> return (base (BCBool(bc))  , (rng, BaseType(BoolType))  )
  | UTUnitConstant        -> return (base BCUnit        , (rng, BaseType(UnitType))  )

  | UTPositionedString(ipos, s) ->
      begin
        match pre.stage with
        | Stage1 | Persistent0 ->
            err (InvalidExpressionAsToStaging(rng, Stage0))

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
            return (e, ty)
      end

  | UTLengthDescription(flt, unitnm) ->
        let* len =
          match unitnm with  (* temporary; ad-hoc handling of unit names *)
          | "pt"   -> return @@ Length.of_pdf_point flt
          | "cm"   -> return @@ Length.of_centimeter flt
          | "mm"   -> return @@ Length.of_millimeter flt
          | "inch" -> return @@ Length.of_inch flt
          | _      -> err (UnknownUnitOfLength(rng, unitnm))
        in
        return (base (BCLength(len)), (rng, BaseType(LengthType)))

  | UTInlineText(utits) ->
      let* its = typecheck_inline_text rng pre tyenv utits in
      return (InlineText(its), (rng, BaseType(InlineTextType)))

  | UTBlockText(utbts) ->
      let* bts = typecheck_block_text rng pre tyenv utbts in
      return (BlockText(bts), (rng, BaseType(BlockTextType)))

  | UTMathText(utmts) ->
      let* mts = typecheck_math pre tyenv utmts in
      return (MathText(mts), (rng, BaseType(MathTextType)))

  | UTOpenIn((rng_mod, modnm), utast1) ->
      begin
        match tyenv |> Typeenv.find_module modnm with
        | None ->
            err (UndefinedModuleName(rng_mod, modnm))

        | Some(mentry) ->
            begin
              match mentry.mod_signature with
              | ConcFunctor(fsig) ->
                  err (NotAStructureSignature(rng_mod, fsig))

              | ConcStructure(ssig) ->
                  let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
                  typecheck_iter tyenv utast1
            end
      end

  | UTContentOf(modidents, (rng_var, varnm)) ->
      let* (pty, e) =
        match modidents with
        | [] ->
            let* ventry =
              match tyenv |> Typeenv.find_value varnm with
              | None ->
                  let cands = find_candidates_in_type_environment tyenv varnm in
                  err (UndefinedVariable(rng, varnm, cands))

              | Some(ventry) ->
                  return ventry
            in
            let evid =
              match ventry.val_name with
              | None       -> assert false
              | Some(evid) -> evid
            in
            let stage = ventry.val_stage in
            let* e =
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0) ->
                  return @@ ContentOf(rng, evid)

              | (Stage1, Persistent0) ->
                  return @@ Persistent(rng, evid)

              | (Stage0, Stage0)
              | (Stage1, Stage1) ->
                  return @@ ContentOf(rng, evid)

              | _ ->
                  err (InvalidOccurrenceAsToStaging(rng_var, varnm, stage))
            in
            return (ventry.val_type, e)

        | modident0 :: proj ->
            let modchain = (modident0, proj) in
            let* mentry = find_module_chain tyenv modchain in
            let* ssig =
              match mentry.mod_signature with
              | ConcStructure(ssig) -> return ssig
              | ConcFunctor(fsig)   -> err (NotAStructureSignature(rng, fsig))
                  (*TODO (error): give a better code range to this error. *)
            in
            let* ventry =
              match ssig |> StructSig.find_value varnm with
              | None ->
                  let cands = find_candidates_in_struct_sig ssig varnm in
                  err (UndefinedVariable(rng, varnm, cands))

              | Some(ventry) ->
                  return ventry
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
            let* e =
              match (pre.stage, stage) with
              | (Persistent0, Persistent0)
              | (Stage0, Persistent0) ->
                  return @@ ContentOf(rng, evid)

              | (Stage1, Persistent0) ->
                  return @@ Persistent(rng, evid)

              | (Stage0, Stage0)
              | (Stage1, Stage1) ->
                  return @@ ContentOf(rng, evid)

              | _ ->
                  err (InvalidOccurrenceAsToStaging(rng_var, varnm, stage))
            in
            return (ventry.val_type, e)
      in
      let tyfree = TypeConv.instantiate pre.level pre.quantifiability pty in
      let tyres = TypeConv.overwrite_range_of_type tyfree rng in
      return (e, tyres)

  | UTConstructor(modidents, ctornm, utast1) ->
      let* centry = find_constructor rng tyenv modidents ctornm in
      let (tyargs, tyid, tyc) = instantiate_constructor pre centry in
      let* (e1, ty1) = typecheck_iter tyenv utast1 in
      let* () = unify ty1 tyc in
      let tyres = (rng, DataType(tyargs, tyid)) in
      return (NonValueConstructor(ctornm, e1), tyres)

  | UTLambdaInlineCommand{
      parameters       = param_units;
      context_variable = ident_ctx;
      body             = utast_body;
    } ->
      let* (tyenv, params) = typecheck_abstraction pre tyenv param_units in
      let (rng_var, varnm_ctx) = ident_ctx in
      let (bsty_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, InlineBoxesType)
      in
      let evid_ctx = EvalVarID.fresh ident_ctx in
      let* (e_body, ty_body) =
        let tyenv =
          let ventry =
            {
              val_type  = Poly(rng_var, BaseType(bsty_var));
              val_name  = Some(evid_ctx);
              val_stage = pre.stage;
            }
          in
          tyenv |> Typeenv.add_value varnm_ctx ventry
        in
        typecheck_iter tyenv utast_body
      in
      let* () = unify ty_body (Range.dummy "lambda-inline-return", BaseType(bsty_ret)) in
      let e =
        List.fold_right (fun (evid_labmap, pat, _, _) e ->
          Function(evid_labmap, PatternBranch(pat, e))
        ) params (LambdaInline(evid_ctx, e_body))
      in
      let cmdargtys =
        params |> List.map (fun (_, _, ty_labmap, ty_pat) ->
          CommandArgType(ty_labmap, ty_pat)
        )
      in
      return (e, (rng, InlineCommandType(cmdargtys)))

  | UTLambdaBlockCommand{
      parameters       = param_units;
      context_variable = ident_ctx;
      body             = utast_body;
    } ->
      let* (tyenv, params) = typecheck_abstraction pre tyenv param_units in
      let (rng_var, varnm_ctx) = ident_ctx in
      let (bsty_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, BlockBoxesType)
      in
      let evid_ctx = EvalVarID.fresh ident_ctx in
      let* (e_body, ty_body) =
        let tyenv_sub =
          let ventry =
            {
              val_type  = Poly(rng_var, BaseType(bsty_var));
              val_name  = Some(evid_ctx);
              val_stage = pre.stage;
            }
          in
          tyenv |> Typeenv.add_value varnm_ctx ventry
        in
        typecheck_iter tyenv_sub utast_body
      in
      let* () = unify ty_body (Range.dummy "lambda-block-return", BaseType(bsty_ret)) in
      let e =
        List.fold_right (fun (evid_labmap, pat, _, _) e ->
          Function(evid_labmap, PatternBranch(pat, e))
        ) params (LambdaBlock(evid_ctx, e_body))
      in
      let cmdargtys =
        params |> List.map (fun (_, _, ty_labmap, ty_pat) ->
          CommandArgType(ty_labmap, ty_pat)
        )
      in
      return (e, (rng, BlockCommandType(cmdargtys)))

  | UTLambdaMathCommand{
      parameters       = param_units;
      context_variable = ident_ctx;
      script_variables = ident_pair_opt;
      body             = utast_body;
    } ->
      let* (tyenv, params) = typecheck_abstraction pre tyenv param_units in
      let (rng_ctx_var, varnm_ctx) = ident_ctx in
      let (bsty_ctx_var, bsty_ret) =
        if OptionState.is_text_mode () then
          (TextInfoType, StringType)
        else
          (ContextType, MathBoxesType)
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
              Poly(rng, snd (Primitives.option_type (Range.dummy "sub-or-sup", BaseType(MathTextType))))
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
      let* (e_body, ty_body) = typecheck_iter tyenv utast_body in
      let* () = unify ty_body (Range.dummy "lambda-math-return", BaseType(bsty_ret)) in
      let e =
        List.fold_right (fun (evid_labmap, pat, _, _) e ->
          Function(evid_labmap, PatternBranch(pat, e))
        ) params (LambdaMath(evid_ctx, evid_pair_opt, e_body))
      in
      let cmdargtys =
        params |> List.map (fun (_, _, ty_labmap, ty_pat) ->
          CommandArgType(ty_labmap, ty_pat)
        )
      in
      return (e, (rng, MathCommandType(cmdargtys)))

  | UTApply(opts, utast1, utast2) ->
      let* (e1, ty1) = typecheck_iter tyenv utast1 in
      let* (e2, ty2) = typecheck_iter tyenv utast2 in
      let frid = FreeRowID.fresh pre.level LabelSet.empty in
      let* (e_labmap0, labset0, row0) =
        let rvref = ref (MonoRowFree(frid)) in
        opts |> foldM (fun (e_labmap, labset, row) (rlabel, utast0) ->
          let (_, label) = rlabel in
          let* (e0, ty0) = typecheck_iter tyenv utast0 in
          return (e_labmap |> LabelMap.add label e0, labset |> LabelSet.add label, RowCons(rlabel, ty0, row))
        ) (LabelMap.empty, LabelSet.empty, RowVar(UpdatableRow(rvref)))
      in
      let labset = FreeRowID.get_label_set frid in
      FreeRowID.set_label_set frid (LabelSet.union labset labset0);
      let eret = Apply(e_labmap0, e1, e2) in
      begin
        match TypeConv.unlink ty1 with
        | (_, FuncType(row, tydom, tycod)) ->
            let* () = unify_row rng row0 row in
            let* () = unify ty2 tydom in
            let tycodnew = TypeConv.overwrite_range_of_type tycod rng in
            return (eret, tycodnew)

        | (_, TypeVariable(_)) as ty1 ->
            let beta = fresh_type_variable rng pre in
            let* () = unify ty1 (get_range utast1, FuncType(row0, ty2, beta)) in
            return (eret, beta)

        | ty1 ->
            let (rng1, _) = utast1 in
            err (ApplicationOfNonFunction(rng1, ty1))
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
      let* (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
      let* _unit_opt =
        mnty_opt |> optionM (fun mnty ->
          let* typat_annot = ManualTypeDecoder.decode_manual_type pre tyenv mnty in
          unify typat typat_annot
        )
      in
      return (Function(evid_labmap, patbr), (rng, FuncType(optrow, typat, ty1)))

  | UTPatternMatch(utastO, utpatbrs) ->
      let* (eO, tyO) = typecheck_iter tyenv utastO in
      let beta = fresh_type_variable (Range.dummy "ut-pattern-match") pre in
      let* patbrs = typecheck_pattern_branch_list pre tyenv utpatbrs tyO beta in
      Exhchecker.main rng patbrs tyO pre tyenv;
      return (PatternMatch(rng, eO, patbrs), beta)

  | UTLetIn(UTNonRec((ident, utast1)), utast2) ->
      let presub = { pre with level = Level.succ pre.level; } in
      let (_, varnm) = ident in
      let evid = EvalVarID.fresh ident in
      let* (e1, ty1) = typecheck presub tyenv utast1 in
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
      let* (e2, ty2) = typecheck_iter tyenv utast2 in
      return (LetNonRecIn(PVariable(evid), e1, e2), ty2)

  | UTLetIn(UTRec(utrecbinds), utast2) ->
      let* quints = typecheck_letrec pre tyenv utrecbinds in
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
      let* (e2, ty2) = typecheck_iter tyenv utast2 in
      return (LetRecIn(recbindacc |> Alist.to_list, e2), ty2)

  | UTIfThenElse(utastB, utast1, utast2) ->
      let* (eB, tyB) = typecheck_iter tyenv utastB in
      let* () = unify tyB (Range.dummy "if-bool", BaseType(BoolType)) in
      let* (e1, ty1) = typecheck_iter tyenv utast1 in
      let* (e2, ty2) = typecheck_iter tyenv utast2 in
      let* () = unify ty2 ty1 in
      return (IfThenElse(eB, e1, e2), ty1)

  | UTLetIn(UTMutable(ident, utastI), utastA) ->
      let* (tyenvI, evid, eI, _tyI) = typecheck_let_mutable pre tyenv ident utastI in
      let* (eA, tyA) = typecheck_iter tyenvI utastA in
      return (LetMutableIn(evid, eI, eA), tyA)

  | UTOverwrite(ident, utastN) ->
      let (rng_var, _) = ident in
      let* sub = typecheck_iter tyenv (rng_var, UTContentOf([], ident)) in
      begin
        match sub with
        | (ContentOf(_, evid), tyvar) ->
            let* (eN, tyN) = typecheck_iter tyenv utastN in
            let* () = unify tyvar (get_range utastN, RefType(tyN)) in
              (* Actually `get_range utastN` is not good
                 since the rhs expression has type `ty`, not `ref ty`. *)
            return (Overwrite(evid, eN), (rng, BaseType(UnitType)))

        | _ ->
            assert false
      end

  | UTItemize(utitmz) ->
      let* eitmz = typecheck_itemize pre tyenv utitmz in
      let ty = TypeConv.overwrite_range_of_type (Primitives.itemize_type ()) rng in
      return (eitmz, ty)

  | UTListCons(utastH, utastT) ->
      let* (eH, tyH) = typecheck_iter tyenv utastH in
      let* (eT, tyT) = typecheck_iter tyenv utastT in
      let* () = unify tyT (Range.dummy "list-cons", ListType(tyH)) in
      let tyres = (rng, ListType(tyH)) in
      return (PrimitiveListCons(eH, eT), tyres)

  | UTEndOfList ->
      let beta = fresh_type_variable rng pre in
      return (ASTEndOfList, (rng, ListType(beta)))

  | UTTuple(utasts) ->
      let* etys = TupleList.mapM (typecheck_iter tyenv) utasts in
      let es = TupleList.map fst etys in
      let tys = TupleList.map snd etys in
      let tyres = (rng, ProductType(tys)) in
      return (PrimitiveTuple(es), tyres)

  | UTRecord(fields) ->
      typecheck_record rng pre tyenv fields

  | UTAccessField(utast1, (_, label)) ->
      let* (e1, ty1) = typecheck_iter tyenv utast1 in
      let beta = fresh_type_variable rng pre in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoRowFree(frid)) in
        RowCons((Range.dummy "UTAccessField", label), beta, RowVar(UpdatableRow(rvuref)))
      in
      let* () = unify ty1 (Range.dummy "UTAccessField", RecordType(row)) in
      return (AccessField(e1, label), beta)

  | UTUpdateField(utast1, rlabel, utast2) ->
      let (_, label) = rlabel in
      let* (e1, ty1) = typecheck_iter tyenv utast1 in
      let* (e2, ty2) = typecheck_iter tyenv utast2 in
      let row =
        let frid = fresh_free_row_id pre.level (LabelSet.singleton label) in
        let rvuref = ref (MonoRowFree(frid)) in
        RowCons(rlabel, ty2, RowVar(UpdatableRow(rvuref)))
      in
      let* () = unify ty1 (Range.dummy "UTUpdateField", RecordType(row)) in
      return (UpdateField(e1, label, e2), ty1)

  | UTReadInline(utast_ctx, utastI) ->
      let* (e_ctx, ty_ctx) = typecheck_iter tyenv utast_ctx in
      let* (eI, tyI) = typecheck_iter tyenv utastI in
      let (e_ret, bsty_ctx, bsty_ret) =
        if OptionState.is_text_mode () then
          (PrimitiveStringifyInline(e_ctx, eI), TextInfoType, StringType)
        else
          (PrimitiveReadInline(e_ctx, eI), ContextType, InlineBoxesType)
      in
      let* () = unify ty_ctx (Range.dummy "ut-read-inline-1", BaseType(bsty_ctx)) in
      let* () = unify tyI (Range.dummy "ut-read-inline-2", BaseType(InlineTextType)) in
      return (e_ret, (rng, BaseType(bsty_ret)))

  | UTReadBlock(utast_ctx, utastB) ->
      let* (e_ctx, ty_ctx) = typecheck_iter tyenv utast_ctx in
      let* (eB, tyB) = typecheck_iter tyenv utastB in
      let (e_ret, bsty_ctx, bsty_ret) =
        if OptionState.is_text_mode () then
          (PrimitiveStringifyBlock(e_ctx, eB), TextInfoType, StringType)
        else
          (PrimitiveReadBlock(e_ctx, eB), ContextType, BlockBoxesType)
      in
      let* () = unify ty_ctx (Range.dummy "ut-read-block-1", BaseType(bsty_ctx)) in
      let* () = unify tyB (Range.dummy "ut-read-block-2", BaseType(BlockTextType)) in
      return (e_ret, (rng, BaseType(bsty_ret)))

  | UTNext(utast1) ->
      begin
        match pre.stage with
        | Stage0 ->
            let* (e1, ty1) = typecheck_iter ~s:Stage1 tyenv utast1 in
            return (Next(e1), (rng, CodeType(ty1)))

        | Stage1 | Persistent0 ->
            err (InvalidExpressionAsToStaging(rng, Stage0))
      end

  | UTPrev(utast1) ->
      begin
        match pre.stage with
        | Stage0 | Persistent0 ->
            err (InvalidExpressionAsToStaging(rng, Stage1))

        | Stage1 ->
            let* (e1, ty1) = typecheck_iter ~s:Stage0 tyenv utast1 in
            let beta = fresh_type_variable rng pre in
            let* () = unify ty1 (Range.dummy "prev", CodeType(beta)) in
            return (Prev(e1), beta)
      end


and typecheck_abstraction (pre : pre) (tyenv : Typeenv.t) (param_units : untyped_parameter_unit list) : (Typeenv.t * (EvalVarID.t LabelMap.t * pattern_tree * mono_type LabelMap.t * mono_type) list) ok =
  let open ResultMonad in
  let* (tyenv, acc) =
    param_units |> foldM (fun (tyenv, acc) param_unit ->
      let UTParameterUnit(opt_params, utpat, mnty_opt) = param_unit in
      let (ty_labmap, evid_labmap, tyenv) =
        let cons (_, label) ty ty_labmap =
          ty_labmap |> LabelMap.add label ty
        in
        let nil = LabelMap.empty in
        add_optionals_to_type_environment ~cons ~nil tyenv pre opt_params
      in
      let* (pat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
      let* _unit_opt =
        mnty_opt |> optionM (fun mnty ->
          let* typat_annot = ManualTypeDecoder.decode_manual_type pre tyenv mnty in
          unify typat typat_annot
        )
      in
      let tyenv = add_pattern_var_mono pre tyenv patvarmap in
      return (tyenv, Alist.extend acc (evid_labmap, pat, ty_labmap, typat))
    ) (tyenv, Alist.empty)
  in
  return (tyenv, acc |> Alist.to_list)



and typecheck_command_arguments (tycmd : mono_type) (rngcmdapp : Range.t) (pre : pre) (tyenv : Typeenv.t) (utcmdargs : untyped_command_argument list) (cmdargtys : mono_command_argument_type list) : ((abstract_tree LabelMap.t * abstract_tree) list) ok =
  let open ResultMonad in
  let* zipped =
    try
      return @@ List.combine utcmdargs cmdargtys
    with
    | Invalid_argument(_) ->
      let arity_expected = List.length cmdargtys in
      let arity_actual = List.length utcmdargs in
      err (InvalidArityOfCommandApplication(rngcmdapp, arity_expected, arity_actual))
  in
  zipped |> mapM (fun (utcmdarg, cmdargty) ->
    let UTCommandArg(labeled_utasts, utast1) = utcmdarg in
    let* utast_labmap =
      labeled_utasts |> foldM (fun utast_labmap ((rng, label), utast) ->
        if utast_labmap |> LabelMap.mem label then
          err (LabelUsedMoreThanOnce(rng, label))
        else
          return (utast_labmap |> LabelMap.add label (utast, rng))
      ) LabelMap.empty
    in
    let CommandArgType(ty_labmap, ty2) = cmdargty in
    let* e_labmap =
      LabelMap.mergeM (fun label utast_and_rng_opt ty_opt ->
        match (utast_and_rng_opt, ty_opt) with
        | (Some((utast1, _)), Some(ty2)) ->
            let* (e1, ty1) = typecheck pre tyenv utast1 in
            let* () = unify ty1 ty2 in
            return @@ Some(e1)

        | (Some((_, rng)), None) ->
            err (UnexpectedOptionalLabel(rng, label, tycmd))

        | (None, _) ->
            return None

      ) utast_labmap ty_labmap
    in
    let* (e1, ty1) = typecheck pre tyenv utast1 in
    let* () = unify ty1 ty2 in
    return (e_labmap, e1)
  )


and typecheck_math (pre : pre) (tyenv : Typeenv.t) (utmes : untyped_math_text_element list) : (math_text_element list) ok =
  let open ResultMonad in

  let iter (_b, utmes) = typecheck_math pre tyenv utmes in

  utmes |> mapM (fun utme ->
    let (rng, UTMathTextElement{ base = utbase; sub = utsub_opt; sup = utsup_opt }) = utme in
    let* base =
      match utbase with
      | UTMathTextChar(uch) ->
          return @@ MathTextChar(uch)

      | UTMathTextApplyCommand(utast_cmd, utcmdarglst) ->
          let* (e_cmd, ty_cmd) = typecheck pre tyenv utast_cmd in
          begin
            match ty_cmd with
            | (_, MathCommandType(cmdargtys)) ->
                let* args = typecheck_command_arguments ty_cmd rng pre tyenv utcmdarglst cmdargtys in
                return @@ MathTextApplyCommand{
                  command   = e_cmd;
                  arguments = args;
                }

            | (_, InlineCommandType(_)) ->
                let (rng_cmd, _) = utast_cmd in
                err (InlineCommandInMath(rng_cmd))

            | _ ->
                failwith (Printf.sprintf "unexpected type: %s" (Display.show_mono_type ty_cmd))
          end

      | UTMathTextContent(utast0) ->
          let* (e0, ty0) = typecheck pre tyenv utast0 in
          let* () = unify ty0 (Range.dummy "math-embedded-var", BaseType(MathTextType)) in
          return @@ MathTextContent(e0)
    in
    let* sub = utsub_opt |> optionM iter in
    let* sup = utsup_opt |> optionM iter in
    return @@ MathTextElement{ base; sub; sup }
  )


and typecheck_block_text (_rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (utbts : untyped_block_text_element list) : (block_text_element list) ok =
  let open ResultMonad in
  utbts |> foldM (fun acc utbt ->
    match utbt with
    | (rng_cmdapp, UTBlockTextApplyCommand(utast_cmd, utcmdargs)) ->
        let* (e_cmd, ty_cmd) = typecheck pre tyenv utast_cmd in
        let cmdargtys =
          match ty_cmd with
          | (_, BlockCommandType(cmdargtys)) -> cmdargtys
          | _                                -> assert false
        in
        let* args = typecheck_command_arguments ty_cmd rng_cmdapp pre tyenv utcmdargs cmdargtys in
        return @@ Alist.extend acc (BlockTextApplyCommand{ command = e_cmd; arguments = args })

    | (_, UTBlockTextContent(utast0)) ->
        let* (e0, ty0) = typecheck pre tyenv utast0 in
        let* () = unify ty0 (Range.dummy "UTBlockTextContent", BaseType(BlockTextType)) in
        return @@ Alist.extend acc (BlockTextContent(e0))

    | (rng_app, UTBlockTextMacro(bmacro, utmacargs)) ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              err (InvalidExpressionAsToStaging(rng_app, Stage1))

          | Stage1 ->
              let (_rng_all, modidents, cs) = bmacro in
              let (rng_cs, _csnm) = cs in
              let* macentry = find_macro tyenv modidents cs in
              let macty = TypeConv.instantiate_macro_type pre.level pre.quantifiability macentry.macro_type in
              let macparamtys =
                match macty with
                | BlockMacroType(macparamtys) -> macparamtys
                | _                           -> assert false
              in
              let evid =
                match macentry.macro_name with
                | Some(evid) -> evid
                | None       -> assert false
              in
              let* eargs = typecheck_macro_arguments rng_app pre tyenv macparamtys utmacargs in
              let eapp = apply_tree_of_list (ContentOf(rng_cs, evid)) eargs in
              return @@ Alist.extend acc (BlockTextContent(Prev(eapp)))
        end

  ) Alist.empty >>= fun acc ->
  return @@ Alist.to_list acc


and typecheck_inline_text (_rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (utits : untyped_inline_text_element list) : (inline_text_element list) ok =
  let open ResultMonad in
  utits |> foldM (fun acc utit ->
    match utit with
    | (rng_cmdapp, UTInlineTextApplyCommand(utast_cmd, utcmdargs)) ->
        let* (e_cmd, ty_cmd) = typecheck pre tyenv utast_cmd in
        let* cmdargtys =
          match ty_cmd with
          | (_, InlineCommandType(cmdargtys)) ->
              return cmdargtys

          | (_, MathCommandType(_)) ->
              let (rng_cmd, _) = utast_cmd in
              err (MathCommandInInline(rng_cmd))

          | _ ->
              assert false
        in
        let* args = typecheck_command_arguments ty_cmd rng_cmdapp pre tyenv utcmdargs cmdargtys in
        return @@ Alist.extend acc (InlineTextApplyCommand{ command = e_cmd; arguments = args })

    | (_, UTInlineTextEmbeddedMath(utast_math)) ->
        let* (emath, tymath) = typecheck pre tyenv utast_math in
        let* () = unify tymath (Range.dummy "ut-inline-text-embedded-math", BaseType(MathTextType)) in
        return @@ Alist.extend acc (InlineTextEmbeddedMath(emath))

    | (_, UTInlineTextEmbeddedCodeArea(s)) ->
        return @@ Alist.extend acc (InlineTextEmbeddedCodeArea(s))

    | (_, UTInlineTextContent(utast0)) ->
        let* (e0, ty0) = typecheck pre tyenv utast0 in
        let* () = unify ty0 (Range.dummy "ut-inline-text-content", BaseType(InlineTextType)) in
        return @@ Alist.extend acc (InlineTextContent(e0))

    | (_, UTInlineTextString(s)) ->
        return @@ Alist.extend acc (InlineTextString(s))

    | (rng_app, UTInlineTextMacro(hmacro, utmacargs)) ->
        begin
          match pre.stage with
          | Stage0 | Persistent0 ->
              err (InvalidExpressionAsToStaging(rng_app, Stage1))

          | Stage1 ->
              let (_rng_all, modidents, cs) = hmacro in
              let (rng_cs, _csnm) = cs in
              let* macentry = find_macro tyenv modidents cs in
              let macty = TypeConv.instantiate_macro_type pre.level pre.quantifiability macentry.macro_type in
              let macparamtys =
                match macty with
                | InlineMacroType(macparamtys) -> macparamtys
                | _                            -> assert false
              in
              let evid =
                match macentry.macro_name with
                | Some(evid) -> evid
                | None       -> assert false
              in
              let* eargs = typecheck_macro_arguments rng_app pre tyenv macparamtys utmacargs in
              let eapp = apply_tree_of_list (ContentOf(rng_cs, evid)) eargs in
              return @@ Alist.extend acc (InlineTextContent(Prev(eapp)))
        end

  ) Alist.empty >>= fun acc ->
  return @@ Alist.to_list acc


and typecheck_macro_arguments (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (macparamtys : mono_macro_parameter_type list) (utmacargs : untyped_macro_argument list) : (abstract_tree list) ok =
  let open ResultMonad in
  let* zipped =
    try
      return @@ List.combine macparamtys utmacargs
    with
    | Invalid_argument(_) ->
        err (InvalidNumberOfMacroArguments(rng, macparamtys))
  in
  let* argacc =
    zipped |> foldM (fun argacc (macparamty, utmacarg) ->
      match macparamty with
      | LateMacroParameter(tyexp) ->
          begin
            match utmacarg with
            | UTLateMacroArg(utast) ->
                let* (earg, tyarg) = typecheck pre tyenv utast in
                let* () = unify tyarg tyexp in
                return @@ Alist.extend argacc (Next(earg))
                  (* Late arguments are converted to quoted arguments. *)

            | UTEarlyMacroArg((rngarg, _)) ->
                err (LateMacroArgumentExpected(rngarg, tyexp))
          end

      | EarlyMacroParameter(tyexp) ->
          begin
            match utmacarg with
            | UTLateMacroArg((rngarg, _)) ->
                err (EarlyMacroArgumentExpected(rngarg, tyexp))

            | UTEarlyMacroArg(utast) ->
                let* (earg, tyarg) = typecheck { pre with stage = Stage0 } tyenv utast in
                let* () = unify tyarg tyexp in
                return @@ Alist.extend argacc earg
          end

    ) Alist.empty
  in
  return @@ Alist.to_list argacc


and typecheck_record (rng : Range.t) (pre : pre) (tyenv : Typeenv.t) (fields : (label ranged * untyped_abstract_tree) list) : (abstract_tree * mono_type) ok =
  let open ResultMonad in
  let* (easc, row) =
    fields |> foldM (fun (easc, row) (rlabel, utast) ->
      let (rng_label, label) = rlabel in
      if easc |> LabelMap.mem label then
        err (LabelUsedMoreThanOnce(rng_label, label))
      else
        let* (e, ty) = typecheck pre tyenv utast in
        return (easc |> LabelMap.add label e, RowCons(rlabel, ty, row))
    ) (LabelMap.empty, RowEmpty)
  in
  return (Record(easc), (rng, RecordType(row)))


and typecheck_itemize (pre : pre) (tyenv : Typeenv.t) (UTItem(utast1, utitmzlst)) : abstract_tree ok =
  let open ResultMonad in
  let* (e1, ty1) = typecheck pre tyenv utast1 in
  let* () = unify ty1 (Range.dummy "typecheck_itemize_string", BaseType(InlineTextType)) in
  let* e2 = typecheck_itemize_list pre tyenv utitmzlst in
  return (NonValueConstructor("Item", PrimitiveTuple(TupleList.make e1 e2 [])))


and typecheck_itemize_list (pre : pre) (tyenv : Typeenv.t) (utitmzlst : untyped_itemize list) : abstract_tree ok =
  let open ResultMonad in
  match utitmzlst with
  | [] ->
      return ASTEndOfList

  | hditmz :: tlitmzlst ->
      let* ehd = typecheck_itemize pre tyenv hditmz in
      let* etl = typecheck_itemize_list pre tyenv tlitmzlst in
      return @@ PrimitiveListCons(ehd, etl)


and typecheck_pattern_branch (pre : pre) (tyenv : Typeenv.t) (utpatbr : untyped_pattern_branch) : (pattern_branch * mono_type * mono_type) ok =
  let open ResultMonad in
  let UTPatternBranch(utpat, utast1) = utpatbr in
  let* (epat, typat, patvarmap) = typecheck_pattern pre tyenv utpat in
  let tyenvpat = add_pattern_var_mono pre tyenv patvarmap in
  let* (e1, ty1) = typecheck pre tyenvpat utast1 in
  return (PatternBranch(epat, e1), typat, ty1)


and typecheck_pattern_branch_list (pre : pre) (tyenv : Typeenv.t) (utpatbrs : untyped_pattern_branch list) (tyobj : mono_type) (tyres : mono_type) : (pattern_branch list) ok =
  let open ResultMonad in
  utpatbrs |> mapM (fun utpatbr ->
    let* (patbr, typat, ty1) = typecheck_pattern_branch pre tyenv utpatbr in
    let* () = unify typat tyobj in
    let* () = unify ty1 tyres in
    return patbr
  )


and typecheck_letrec (pre : pre) (tyenv : Typeenv.t) (utrecbinds : untyped_let_binding list) : ((var_name * poly_type * EvalVarID.t * letrec_binding) list) ok =
  let open ResultMonad in

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

  (* Typechecks each body of the definitions: *)
  let* tupleacc =
    utrecacc |> Alist.to_list |> foldM (fun tupleacc utrec ->
      let (((_, varnm), utast1), beta, evid) = utrec in
      let* (e1, ty1) = typecheck { pre with level = Level.succ pre.level; } tyenv utast1 in
      begin
        match e1 with
        | Function(evid_labmap, patbr1) ->
            if LabelMap.cardinal evid_labmap = 0 then begin
              let* () = unify ty1 beta in
(*
              mntyopt |> Option.map (fun mnty ->
                let tyin = decode_manual_type pre tyenv mnty in
                unify tyin beta
              ) |> Option.value ~default:();
*)
              let recbind = LetRecBinding(evid, patbr1) in
              let tupleacc = Alist.extend tupleacc (varnm, beta, evid, recbind) in
              return tupleacc
            end else
              let (rng1, _) = utast1 in
              err (BreaksValueRestriction(rng1))

        | _ ->
            let (rng1, _) = utast1 in
            err (BreaksValueRestriction(rng1))
      end
    ) Alist.empty
  in

  let tuples =
    tupleacc |> Alist.to_list |> List.map (fun (varnm, ty, evid, recbind) ->
      let pty = TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty) in
      (varnm, pty, evid, recbind)
    )
  in
  return tuples


and typecheck_let_mutable (pre : pre) (tyenv : Typeenv.t) (ident : var_name ranged) (utastI : untyped_abstract_tree) : (Typeenv.t * EvalVarID.t * abstract_tree * mono_type) ok =
  let open ResultMonad in
  let* (eI, tyI) = typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
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
  return (tyenvI, evid, eI, tyI)


let main (stage : stage) (tyenv : Typeenv.t) (utast : untyped_abstract_tree) : (mono_type * abstract_tree) ok =
  let open ResultMonad in
  let pre =
    {
      stage           = stage;
      type_parameters = TypeParameterMap.empty;
      row_parameters  = RowParameterMap.empty;
      quantifiability = Quantifiable;
      level           = Level.bottom;
    }
  in
  let* (e, ty) = typecheck pre tyenv utast in
  return (ty, e)


let are_unifiable (ty1 : mono_type) (ty2 : mono_type) : bool =
  match Unification.unify_type ty1 ty2 with
  | Error(_) -> false
  | Ok(_)    -> true
