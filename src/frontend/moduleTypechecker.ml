
open SyntaxBase
open Types
open StaticEnv
open TypeError
open TypecheckUtil


module SynonymNameMap = Map.Make(String)

module SynonymVertexHashSet = Hashtbl.Make(SynonymDependencyGraph.Vertex)

module SynonymVertexSet = SynonymDependencyGraph.VertexSet


let unify_quantifier (quant1 : quantifier) (quant2 : quantifier) : quantifier =
  OpaqueIDMap.union (fun _ _kd1 kd2 -> Some(kd2)) quant1 quant2


let abstraction (evid : EvalVarID.t) (ast : abstract_tree) : abstract_tree =
  Function(LabelMap.empty, PatternBranch(PVariable(evid), ast))


let abstraction_list (evids : EvalVarID.t list) (ast : abstract_tree) : abstract_tree =
  List.fold_right abstraction evids ast


let decode_manual_row_base_kind (mnrbkd : manual_row_base_kind) : row_base_kind ok =
  let open ResultMonad in
  mnrbkd |> foldM (fun labset (rng, label) ->
    if labset |> LabelSet.mem label then
      err (LabelUsedMoreThanOnce(rng, label))
    else
      return (labset |> LabelSet.add label)
  ) LabelSet.empty


let add_type_parameters (lev : Level.t) (tyvars : (type_variable_name ranged) list) (typarammap : type_parameter_map) : (type_parameter_map * BoundID.t list) ok =
  let open ResultMonad in
  let* (typarammap, bidacc) =
    tyvars |> foldM (fun (typarammap, bidacc) (rng, tyvarnm) ->
      if typarammap |> TypeParameterMap.mem tyvarnm then
        err (TypeParameterBoundMoreThanOnce(rng, tyvarnm))
      else
        let mbbid = MustBeBoundID.fresh lev in
        let bid = MustBeBoundID.to_bound_id mbbid in
        return (typarammap |> TypeParameterMap.add tyvarnm mbbid, Alist.extend bidacc bid)
    ) (typarammap, Alist.empty)
  in
  return (typarammap, Alist.to_list bidacc)


let add_row_parameters (lev : Level.t) (rowvars : (row_variable_name ranged * manual_row_base_kind) list) (rowparammap : row_parameter_map) : (row_parameter_map * BoundRowID.t list) ok =
  let open ResultMonad in
  let* (rowparammap, bridacc) =
    rowvars |> foldM (fun (rowparammap, bridacc) ((rng, rowvarnm), mnbrkd) ->
      if rowparammap |> RowParameterMap.mem rowvarnm then
        err (LabelUsedMoreThanOnce(rng, rowvarnm))
      else
        decode_manual_row_base_kind mnbrkd >>= fun labset ->
        let mbbrid = MustBeBoundRowID.fresh lev labset in
        let brid = MustBeBoundRowID.to_bound_id mbbrid in
        return (rowparammap |> RowParameterMap.add rowvarnm mbbrid, Alist.extend bridacc brid)
    ) (rowparammap, Alist.empty)
  in
  return (rowparammap, Alist.to_list bridacc)


let make_constructor_branch_map (pre : pre) (tyenv : Typeenv.t) (utctorbrs : constructor_branch list) : constructor_branch_map ok =
  let open ResultMonad in
  utctorbrs |> foldM (fun ctormap utctorbr ->
    match utctorbr with
    | UTConstructorBranch((_rng, ctornm), mty_opt) ->
        let* ty =
          match mty_opt with
          | Some(mty) -> ManualTypeDecoder.decode_manual_type pre tyenv mty
          | None      -> return (Range.dummy "unit", BaseType(UnitType))
        in
        let pty = TypeConv.generalize pre.level ty in
        return (ctormap |> ConstructorMap.add ctornm pty)
  ) ConstructorMap.empty


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


let rec update_subsignature (modnms : module_name list) (updater : signature -> signature) (modsig : signature) : signature =
  match modnms with
  | [] ->
      updater modsig

  | modnm0 :: proj ->
      begin
        match modsig with
        | ConcFunctor(_) ->
            assert false

        | ConcStructure(ssig) ->
            let ssig =
              ssig |> StructSig.map
                ~v:(fun _x ventry -> ventry)
                ~a:(fun _csnm macentry -> macentry)
                ~c:(fun _ctornm centry -> centry)
                ~f:(fun _tynm pty -> pty)
                ~t:(fun _tynm tentry -> tentry)
                ~m:(fun modnm mentry ->
                  if String.equal modnm modnm0 then
                    let modsig = mentry.mod_signature |> update_subsignature proj updater in
                    { mod_signature = modsig }
                  else
                    mentry
                )
                ~s:(fun _signm sentry -> sentry)
            in
            ConcStructure(ssig)
      end


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


let get_dependency_on_synonym_types (known_syns : SynonymDependencyGraph.Vertex.t SynonymNameMap.t) (_pre : pre) (_tyenv : Typeenv.t) (mty : manual_type) : SynonymVertexSet.t =
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

    | MTypeParam(_typaram) ->
        ()

    | MInlineCommandType(mcmdargtys)
    | MBlockCommandType(mcmdargtys)
    | MMathCommandType(mcmdargtys) ->
        mcmdargtys |> List.iter aux_cmd_arg

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


let bind_types (tyenv : Typeenv.t) (tybinds : untyped_type_binding list) : ((type_name * type_entry) list * (constructor_name * TypeID.t * BoundID.t list * constructor_branch_map) list) ok =
  let open ResultMonad in
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
  let* (synacc, vntacc, known_syns, graph, tyenv) =
    tybinds |> foldM (fun (synacc, vntacc, known_syns, graph, tyenv) tybind ->
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
          let* (graph, vertex) =
            match graph |> SynonymDependencyGraph.add_vertex tynm data with
            | Error((data_prev, _)) ->
                let rng_prev = data_prev.SynonymDependencyGraph.position in
                err (MultipleSynonymTypeDefinition(tynm, rng_prev, rng))

            | Ok(pair) ->
                return pair
          in
          let synacc = Alist.extend synacc (tyident, typarams, synbind, vertex) in
          return (synacc, vntacc, known_syns |> SynonymNameMap.add tynm vertex, graph, tyenv)

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
          return (synacc, vntacc, known_syns, graph, tyenv)
    ) (Alist.empty, Alist.empty, SynonymNameMap.empty, SynonymDependencyGraph.empty, tyenv)
  in

  (* Traverse each definition of the synonym types and extract dependencies between them. *)
  let graph =
    synacc |> Alist.to_list |> List.fold_left (fun graph syn ->
      let ((_, _tynm), _tyvars, synbind, vertex) = syn in
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
  let* syns =
    match SynonymDependencyGraph.topological_sort graph with
    | Error(cycle) -> err (CyclicSynonymTypeDefinition(cycle))
    | Ok(syns)     -> return syns
  in

  (* Add the definition of the synonym types to the type environment. *)
  let* (tyenv, tydefacc) =
    syns |> foldM (fun (tyenv, tydefacc) syn ->
      let (tynm, syndata) = syn in
      let
        SynonymDependencyGraph.{
          type_variables  = tyvars;
          definition_body = mty_body;
          _
        } = syndata
      in
      let* (typarammap, bids) = pre.type_parameters |> add_type_parameters (Level.succ pre.level) tyvars in
      let pre = { pre with type_parameters = typarammap } in
      let* ty_body = ManualTypeDecoder.decode_manual_type pre tyenv mty_body in
      let pty_body = TypeConv.generalize Level.bottom ty_body in
      let tentry =
        {
          type_scheme = (bids, pty_body);
          type_kind   = Kind(bids |> List.map (fun _ -> TypeKind));
        }
      in
      let tyenv = tyenv |> Typeenv.add_type tynm tentry in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      return (tyenv, tydefacc)
    ) (tyenv, Alist.empty)
  in

  (* Traverse each definition of the variant types. *)
  let* (tydefacc, ctordefacc) =
    vntacc |> Alist.to_list |> foldM (fun (tydefacc, ctordefacc) vnt ->
      let (tyident, tyvars, vntbind, tyid, tentry) = vnt in
      let (_, tynm) = tyident in
      let* (typarammap, bids) = pre.type_parameters |> add_type_parameters (Level.succ pre.level) tyvars in
      let pre = { pre with type_parameters = typarammap } in
      let* ctorbrmap = make_constructor_branch_map pre tyenv vntbind in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      let ctordefacc = Alist.extend ctordefacc (tynm, tyid, bids, ctorbrmap) in
      return (tydefacc, ctordefacc)
    ) (tydefacc, Alist.empty)
  in
  return (tydefacc |> Alist.to_list, ctordefacc |> Alist.to_list)


(* Given `modsig1` and `modsig2` which are already known to satisfy `modsig1 <= modsig2`,
   `copy_contents` copies every target name occurred in `modsig1`
   into the corresponding occurrence in `modsig2`. *)
let rec copy_contents (modsig1 : signature) (modsig2 : signature) : signature =
  match (modsig1, modsig2) with
  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      let ssig2_new = copy_closure_in_structure ssig1 ssig2 in
      (ConcStructure(ssig2_new))

  | (ConcFunctor(fsig1), ConcFunctor(fsig2)) ->
      let { opaques = _quant_dom1; domain = modsig_dom1; codomain = absmodsig_cod1; closure = closure } = fsig1 in
      let { opaques = _quant_dom2; domain = modsig_dom2; codomain = absmodsig_cod2; _ } = fsig2 in
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


let coerce_signature (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : (signature abstracted) ok =
  let open ResultMonad in
  let* _subst = SignatureSubtyping.subtype_concrete_with_abstract rng modsig1 absmodsig2 in
  let (quant2, modsig2) = absmodsig2 in
  return (quant2, copy_contents modsig1 modsig2)


let rec typecheck_signature (tyenv : Typeenv.t) (utsig : untyped_signature) : (signature abstracted) ok =
  let open ResultMonad in
  let (rng, utsigmain) = utsig in
  match utsigmain with
  | UTSigVar(signm) ->
      begin
        match tyenv |> Typeenv.find_signature signm with
        | None            -> err (UndefinedSignatureName(rng, signm))
        | Some(absmodsig) -> return absmodsig
      end

  | UTSigPath(modchain1, (rng_signm, signm2)) ->
      let* mentry1 = find_module_chain tyenv modchain1 in
      begin
        match mentry1.mod_signature with
        | ConcFunctor(fsig) ->
            let rng = make_range_from_module_chain modchain1 in
            err (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig1) ->
            begin
              match ssig1 |> StructSig.find_signature signm2 with
              | None             -> err (UndefinedSignatureName(rng_signm, signm2))
              | Some(absmodsig2) -> return absmodsig2
            end
      end

  | UTSigDecls(utdecls) ->
      let* (quant, ssig) = typecheck_declaration_list tyenv utdecls in
      return (quant, ConcStructure(ssig))

  | UTSigFunctor((_, modnm), utsig1, utsig2) ->
      let* (quant1, modsig1) = typecheck_signature tyenv utsig1 in
      let* absmodsig2 =
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
      return (OpaqueIDMap.empty, ConcFunctor(fsig))

  | UTSigWith(utsig0, modidents, tybinds) ->
      let* (quant0, modsig0) = typecheck_signature tyenv utsig0 in
      let* ssig =
        match modsig0 with
        | ConcFunctor(fsig) ->
            err (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig0) ->
            modidents |> foldM (fun ssig (rng, modnm) ->
              match ssig |> StructSig.find_module modnm with
              | None ->
                  err (UndefinedModuleName(rng, modnm))

              | Some(mentry) ->
                  begin
                    match mentry.mod_signature with
                    | ConcFunctor(fsig)   -> err (NotAStructureSignature(rng, fsig))
                    | ConcStructure(ssig) -> return ssig
                  end
            ) ssig0
      in
      let* (tydefs, ctordefs) = bind_types tyenv tybinds in
      let* (subst, quant) =
        tydefs |> foldM (fun (subst, quant) (tynm, tentry) ->
          let* (tyid, kd_expected) =
            match ssig |> StructSig.find_type tynm with
            | None ->
                err (UndefinedTypeName(rng, tynm))

            | Some(tentry) ->
                begin
                  match TypeConv.get_opaque_type tentry.type_scheme with
                  | None ->
                      err (CannotRestrictTransparentType(rng, tynm))

                  | Some(tyid) ->
                      assert (quant |> OpaqueIDMap.mem tyid);
                      return (tyid, tentry.type_kind)
                end
          in
          let kd_actual = tentry.type_kind in
          if TypeConv.kind_equal kd_expected kd_actual then
            let subst = subst |> SubstMap.add tyid tentry.type_scheme in
            let quant = quant |> OpaqueIDMap.remove tyid in
            return (subst, quant)
          else
            err (KindContradiction(rng, tynm, kd_expected, kd_actual))
        ) (SubstMap.empty, quant0)
      in
      let modsig = modsig0 |> SignatureSubtyping.substitute_concrete subst in
      let modsig =
        modsig |> update_subsignature (modidents |> List.map (fun (_, modnm) -> modnm)) (function
        | ConcFunctor(_) ->
            assert false

        | ConcStructure(ssig) ->
            ConcStructure(ssig |> add_constructor_definitions ctordefs)
        )
      in
      return (quant, modsig)


and typecheck_declaration_list (tyenv : Typeenv.t) (utdecls : untyped_declaration list) : (StructSig.t abstracted) ok =
  let open ResultMonad in
  let* (quantacc, ssigacc, _) =
    utdecls |> foldM (fun (quantacc, ssigacc, tyenv) utdecl ->
      let* (quant, ssig) = typecheck_declaration tyenv utdecl in
      let quantacc = unify_quantifier quantacc quant in
      let* ssigacc =
        match StructSig.union ssigacc ssig with
        | Ok(ssigacc) -> return ssigacc
        | Error(s)    -> err (ConflictInSignature(Range.dummy "TODO (error): add range to declarations", s))
      in
      let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
      return (quantacc, ssigacc, tyenv)
    ) (OpaqueIDMap.empty, StructSig.empty, tyenv)
  in
  return (quantacc, ssigacc)


and typecheck_declaration (tyenv : Typeenv.t) (utdecl : untyped_declaration) : (StructSig.t abstracted) ok =
  let open ResultMonad in
  match utdecl with
  | UTDeclValue(stage, (_, x), (typarams, rowparams), mty) ->
      let* (typarammap, _) = TypeParameterMap.empty |> add_type_parameters (Level.succ Level.bottom) typarams in
      let* (rowparammap, _) = RowParameterMap.empty |> add_row_parameters (Level.succ Level.bottom) rowparams in
      let pre =
        {
          stage           = stage;
          level           = Level.succ Level.bottom;
          type_parameters = typarammap;
          row_parameters  = rowparammap;
          quantifiability = Quantifiable;
        }
      in
      let* ty = ManualTypeDecoder.decode_manual_type pre tyenv mty in
      let pty = TypeConv.generalize Level.bottom ty in
      let ventry =
        {
          val_type  = pty;
          val_name  = None;
          val_stage = stage;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_value x ventry in
      return (OpaqueIDMap.empty, ssig)

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
      let* kd = ManualTypeDecoder.decode_manual_kind pre_init tyenv mnkd in
      let tentry =
        {
          type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
          type_kind   = kd;
        }
      in
      let ssig = StructSig.empty |> StructSig.add_type tynm tentry in
      return (OpaqueIDMap.singleton tyid kd, ssig)

  | UTDeclModule((_, modnm), utsig) ->
      let* absmodsig = typecheck_signature tyenv utsig in
      let (quant, modsig) = absmodsig in
      let mentry = { mod_signature = modsig; } in
      let ssig = StructSig.empty |> StructSig.add_module modnm mentry in
      return (quant, ssig)

  | UTDeclSignature((_, signm), utsig) ->
      let* absmodsig = typecheck_signature tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      return (OpaqueIDMap.empty, ssig)

  | UTDeclInclude(utsig) ->
      let* absmodsig = typecheck_signature tyenv utsig in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(fsig) ->
            let (rng, _) = utsig in
            err (NotAStructureSignature(rng, fsig))

        | ConcStructure(ssig) ->
            return (quant, ssig)
      end

  | UTDeclMacro((_rng_cs, csnm), (_, mmacty)) ->
      let pre_init =
        {
          stage           = Stage0;
          level           = Level.bottom;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
        }
      in
      let* macty = ManualTypeDecoder.decode_manual_macro_type pre_init tyenv mmacty in
      let pmacty = TypeConv.generalize_macro_type macty in
      let macentry = { macro_type = pmacty; macro_name = None; } in
      let ssig = StructSig.empty |> StructSig.add_macro csnm macentry in
      return (OpaqueIDMap.empty, ssig)


let rec typecheck_module (tyenv : Typeenv.t) (utmod : untyped_module) : (signature abstracted * binding list) ok =
  let open ResultMonad in
  let (rng, utmodmain) = utmod in
  match utmodmain with
  | UTModVar(modchain) ->
      let* mentry = find_module_chain tyenv modchain in
      let modsig = mentry.mod_signature in
      return ((OpaqueIDMap.empty, modsig), [])

  | UTModBinds(utbinds) ->
      let* ((quant, ssig), binds) = typecheck_binding_list tyenv utbinds in
      return ((quant, ConcStructure(ssig)), binds)

  | UTModFunctor(modident1, utsig1, utmod2) ->
      let (_, modnm1) = modident1 in
      let* absmodsig1 = typecheck_signature tyenv utsig1 in
      let (quant1, modsig1) = absmodsig1 in
      let* (absmodsig2, _binds2) =
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
      return (absmodsig, [])

  | UTModApply(modchain1, modchain2) ->
      let* mentry1 = find_module_chain tyenv modchain1 in
      let* mentry2 = find_module_chain tyenv modchain2 in
      begin
        match mentry1.mod_signature with
        | ConcStructure(ssig) ->
            let rng = make_range_from_module_chain modchain1 in
            err (NotAFunctorSignature(rng, ssig))

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
                  let* subst =
                    SignatureSubtyping.subtype_concrete_with_abstract rng modsig2 (quant1, modsig_dom1)
                  in
                  let* ((_, modsig0), binds) =
                    let mentry0 = { mod_signature = modsig2; } in
                    typecheck_module (tyenv0 |> Typeenv.add_module modnm0 mentry0) utmod0
                  in
                  let (quant1_subst, modsig_cod1_subst) =
                    absmodsig_cod1 |> SignatureSubtyping.substitute_abstract subst
                  in
                  let absmodsig = (quant1_subst, copy_contents modsig0 modsig_cod1_subst) in
                  return (absmodsig, binds)

              | _ ->
                  assert false
            end
      end

  | UTModCoerce(modident1, utsig2) ->
      let* mentry1 = find_module tyenv modident1 in
      let modsig1 = mentry1.mod_signature in
      let* absmodsig2 = typecheck_signature tyenv utsig2 in
      let* absmodsig = coerce_signature rng modsig1 absmodsig2 in
      return (absmodsig, [])


and typecheck_binding_list (tyenv : Typeenv.t) (utbinds : untyped_binding list) : (StructSig.t abstracted * binding list) ok =
  let open ResultMonad in
  let* (binds, (quant, ssig)) =
    let* (bindacc, _tyenv, quantacc, ssigacc) =
      utbinds |> foldM (fun (bindacc, tyenv, quantacc, ssigacc) utbind ->
        let* (binds, (quant, ssig)) = typecheck_binding tyenv utbind in
        let tyenv = tyenv |> add_to_type_environment_by_signature ssig in
        let bindacc = Alist.append bindacc binds in
        let quantacc = unify_quantifier quantacc quant in
        match StructSig.union ssigacc ssig with
        | Ok(ssigacc) -> return (bindacc, tyenv, quantacc, ssigacc)
        | Error(s)    -> let (rng, _) = utbind in err (ConflictInSignature(rng, s))
      ) (Alist.empty, tyenv, OpaqueIDMap.empty, StructSig.empty)
    in
    return (Alist.to_list bindacc, (quantacc, ssigacc))
  in
  return ((quant, ssig), binds)


and typecheck_nonrec (pre : pre) (tyenv : Typeenv.t) (ident : var_name ranged) (utast1 : untyped_abstract_tree) (ty_expected_opt : mono_type option) =
  let open ResultMonad in
  let presub = { pre with level = Level.succ pre.level; } in
  let evid = EvalVarID.fresh ident in
  let* (e1_raw, ty1) = Typechecker.typecheck presub tyenv utast1 in
  let e1 = e1_raw in
  let* () =
    match ty_expected_opt with
    | None              -> return ()
    | Some(ty_expected) -> unify ty1 ty_expected
  in
(*
  let should_be_polymorphic = is_nonexpansive_expression e1 in
*)
  let should_be_polymorphic = true in
    let pty =
      if should_be_polymorphic then
        TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty1)
      else
        TypeConv.lift_poly (TypeConv.erase_range_of_type ty1)
    in
  return (evid, e1, pty)


and typecheck_binding (tyenv : Typeenv.t) (utbind : untyped_binding) : (binding list * StructSig.t abstracted) ok =
  let open ResultMonad in
  let (_, utbindmain) = utbind in
  match utbindmain with
  | UTBindValue(attrs, stage, valbind) ->
      let pre =
        {
          stage           = stage;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in
      let* valattr =
        ValueAttribute.make attrs
          |> Result.map_error (fun e -> ValueAttributeError(e))
      in
      if valattr.ValueAttribute.is_test then
        match (stage, valbind) with
        | (Stage1, UTNonRec(ident, utast1)) ->
            let (_, test_name) = ident in
            let ty_expected = (Range.dummy "test", BaseType(UnitType)) in
            let* (evid, e1, _pty) = typecheck_nonrec pre tyenv ident utast1 (Some(ty_expected)) in
            return ([ BindTest(evid, test_name, e1) ], (OpaqueIDMap.empty, StructSig.empty))

        | _ ->
            let rng = Range.dummy "TODO (error): typecheck_binding, test" in
            err @@ TestMustBeStage1NonRec(rng)
      else
        let* (rec_or_nonrecs, ssig) =
          match valbind with
          | UTNonRec(ident, utast1) ->
              let* (evid, e1, pty) = typecheck_nonrec pre tyenv ident utast1 None in
              let ssig =
                let (_, varnm) = ident in
                let ventry =
                  {
                    val_type  = pty;
                    val_name  = Some(evid);
                    val_stage = pre.stage;
                  }
                in
                StructSig.empty |> StructSig.add_value varnm ventry
              in
              return ([ NonRec(evid, e1) ], ssig)

          | UTRec(utrecbinds) ->
              let* quints = Typechecker.typecheck_letrec pre tyenv utrecbinds in
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
              return ([ Rec(recbindacc |> Alist.to_list) ], ssig)

          | UTMutable((rng, varnm) as var, utastI) ->
              let* (eI, tyI) = Typechecker.typecheck { pre with quantifiability = Unquantifiable; } tyenv utastI in
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
              return ([ Mutable(evid, eI) ], ssig)
        in
        let binds = rec_or_nonrecs |> List.map (fun rec_or_nonrec -> Bind(stage, rec_or_nonrec)) in
        return (binds, (OpaqueIDMap.empty, ssig))

  | UTBindType([]) ->
      assert false

  | UTBindType(tybinds) ->
      let* (tydefs, ctordefs) = bind_types tyenv tybinds in
      let ssig =
        tydefs |> List.fold_left (fun ssig (tynm, tentry) ->
          ssig |> StructSig.add_type tynm tentry
        ) StructSig.empty
      in
      let ssig = ssig |> add_constructor_definitions ctordefs in
      return ([], (OpaqueIDMap.empty, ssig))

  | UTBindModule(modident, utsigopt2, utmod1) ->
      let (rng_mod, modnm) = modident in
      let* (absmodsig1, binds1) = typecheck_module tyenv utmod1 in
      let* (quant, modsig) =
        match utsigopt2 with
        | None ->
            return absmodsig1

        | Some(utsig2) ->
            let (_, modsig1) = absmodsig1 in
            let* absmodsig2 = typecheck_signature tyenv utsig2 in
            coerce_signature rng_mod modsig1 absmodsig2
      in
      let ssig =
        let mentry = { mod_signature = modsig; } in
        StructSig.empty |> StructSig.add_module modnm mentry
      in
      return (binds1, (quant, ssig))

  | UTBindSignature((_, signm), utsig) ->
      let* absmodsig = typecheck_signature tyenv utsig in
      let ssig = StructSig.empty |> StructSig.add_signature signm absmodsig in
      return ([], (OpaqueIDMap.empty, ssig))

  | UTBindInclude(utmod) ->
      let* (absmodsig, binds) = typecheck_module tyenv utmod in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcStructure(ssig) ->
            return (binds, (quant, ssig))

        | ConcFunctor(fsig) ->
            let (rng_mod, _) = utmod in
            err (NotAStructureSignature(rng_mod, fsig))
      end

  | UTBindInlineMacro(_attrs, (rng_cs, csnm), macparams, utast1) ->
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
      let macty = InlineMacroType(macparamtys) in
      let* (e1, ty1) = Typechecker.typecheck pre tyenv utast1 in
      let* () = unify ty1 (Range.dummy "val-inline-macro", BaseType(InlineTextType)) in
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
      return (binds, (OpaqueIDMap.empty, ssig))

  | UTBindBlockMacro(_attrs, (rng_cs, csnm), macparams, utast1) ->
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
      let macty = BlockMacroType(macparamtys) in
      let* (e1, ty1) = Typechecker.typecheck pre tyenv utast1 in
      let* () = unify ty1 (Range.dummy "val-block-macro", BaseType(BlockTextType)) in
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
      return (binds, (OpaqueIDMap.empty, ssig))


let main (tyenv : Typeenv.t) (absmodsig_opt : (signature abstracted) option) (utbinds : untyped_binding list) : (StructSig.t abstracted * binding list) ok =
  let open ResultMonad in
  match absmodsig_opt with
  | None ->
      typecheck_binding_list tyenv utbinds

  | Some(absmodsig) ->
      let* ((_, ssig), binds) = typecheck_binding_list tyenv utbinds in
      let rng = Range.dummy "main_bindings" in (* TODO (error): give appropriate ranges *)
      let* (quant, modsig) = coerce_signature rng (ConcStructure(ssig)) absmodsig in
      let ssig =
        match modsig with
        | ConcFunctor(_)      -> assert false
        | ConcStructure(ssig) -> ssig
      in
      return ((quant, ssig), binds)
