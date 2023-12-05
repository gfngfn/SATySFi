
open SyntaxBase
open Types
open StaticEnv
open TypeError
open TypecheckUtil


(* The type for predicates that takes [bid : BoundID.t] and [pty : poly_type]
   and judges whether [bid] is mapped to a type equal to [pty] in the current context. *)
type type_intern = (BoundID.t -> poly_type -> bool)

(* The type for predicates that takes [brid : BoundRowID.t] and [nomrow : normalized_poly_row]
   and judges whether [brid] is mapped to a row equal to [nomrow] in the current context. *)
type row_intern = (BoundRowID.t -> normalized_poly_row -> bool)


(* `lookup_type_entry σ_1 σ_2` returns:
   - `None` if `σ_1` cannot be a subtype of `θ(σ_2)` with any substitution `θ`,
   - `Some(ω_2 ↦ σ_1)` if `σ_2` is an opaque type `ω_2`, or
   - `Some(∅)` if `σ_2` is not an opaque type. *)
let lookup_type_entry (tentry1 : type_entry) (tentry2 : type_entry) : substitution option =
  if TypeConv.kind_equal tentry1.type_kind tentry2.type_kind then
    let subst =
      match TypeConv.get_opaque_type tentry2.type_scheme with
      | None        -> SubstMap.empty
      | Some(tyid2) -> SubstMap.empty |> SubstMap.add tyid2 tentry1.type_scheme
    in
    Some(subst)
  else
    None


(* `lookup_struct rng Σ_1 Σ_2` compares `Σ_1` and `Σ_2` by traversing only structures,
   and returns a candidate substitution `θ` that satisfies `Σ_1 <: θ(Σ_2)`. *)
let rec lookup_struct (rng : Range.t) (modsig1 : signature) (modsig2 : signature) : substitution ok =
  let open ResultMonad in
  let take_left = (fun _tyid to1 _to2 -> Some(to1)) in
  match (modsig1, modsig2) with
  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun _x2 _ventry2 res ->
            res
          )
          ~a:(fun _csnm _macentry2 res ->
            res
          )
          ~c:(fun _ctornm2 _centry2 res ->
            res
          )
          ~f:(fun _tynm2 _pty res ->
            res
          )
          ~t:(fun tynm2 tentry2 res ->
            res >>= fun subst ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let (bids, _) = tentry2.type_scheme in
                err (MissingRequiredTypeName(rng, tynm2, List.length bids))

            | Some(tentry1) ->
                begin
                  match lookup_type_entry tentry1 tentry2 with
                  | None ->
                      err (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))

                  | Some(subst0) ->
                      return @@ SubstMap.union take_left subst0 subst
                end
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } res ->
            res >>= fun subst ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                err (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some({ mod_signature = modsig1; _ }) ->
                let* subst0 = lookup_struct rng modsig1 modsig2 in
                return @@ SubstMap.union take_left subst0 subst
          )
          ~s:(fun _ _ res ->
            res
          )
          (return SubstMap.empty)

  | _ ->
      return SubstMap.empty


let rec substitute_abstract (subst : substitution) (absmodsig : signature abstracted) : signature abstracted =
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
          codomain = absmodsig2;
          _
        } = fsig
      in
      let modsig1 = modsig1 |> substitute_concrete subst in
      let absmodsig2 = absmodsig2 |> substitute_abstract subst in
      let fsig =
        {
          opaques  = quant;
          domain   = modsig1;
          codomain = absmodsig2;
          closure  = failwith "TODO: substitute_concrete, closure";
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

      | InlineCommandType(pargs) -> InlineCommandType(pargs |> List.map aux_command_arg)
      | BlockCommandType(pargs)  -> BlockCommandType(pargs |> List.map aux_command_arg)
      | MathCommandType(pargs)   -> MathCommandType(pargs |> List.map aux_command_arg)

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
  | InlineMacroType(pmacparamtys) -> InlineMacroType(pmacparamtys |> List.map (substitute_macro_parameter_type subst))
  | BlockMacroType(pmacparamtys)  -> BlockMacroType(pmacparamtys |> List.map (substitute_macro_parameter_type subst))


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
      ~a:(fun _csnm macentry ->
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


let rec subtype_abstract_with_abstract (rng : Range.t) (absmodsig1 : signature abstracted) (absmodsig2 : signature abstracted) : unit ok =
  let open ResultMonad in
  let (_, modsig1) = absmodsig1 in
  let* _ = subtype_concrete_with_abstract rng modsig1 absmodsig2 in
  return ()


and subtype_concrete_with_concrete (rng : Range.t) (modsig1 : signature) (modsig2 : signature) : unit ok =
  let open ResultMonad in
  match (modsig1, modsig2) with
  | (ConcFunctor(fsig1), ConcFunctor(fsig2)) ->
      let
        {
          opaques  = quant1;
          domain   = modsigdom1;
          codomain = absmodsigcod1;
          _
        } = fsig1
      in
      let
        {
          opaques  = _quant2;
          domain   = modsigdom2;
          codomain = absmodsigcod2;
          _
        } = fsig2
      in
      let* subst = subtype_concrete_with_abstract rng modsigdom2 (quant1, modsigdom1) in
      let absmodsigcod1 = absmodsigcod1 |> substitute_abstract subst in
      subtype_abstract_with_abstract rng absmodsigcod1 absmodsigcod2

  | (ConcStructure(ssig1), ConcStructure(ssig2)) ->
      ssig2 |> StructSig.fold
          ~v:(fun x2 { val_type = pty2; val_stage = stage2; _ } res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_value x2 with
            | None ->
                err (MissingRequiredValueName(rng, x2, pty2))

            | Some({ val_type = pty1; val_stage = stage1; _ }) ->
                match (stage1, stage2) with
                | (Persistent0, Persistent0)
                | (Persistent0, Stage0)
                | (Persistent0, Stage1)
                | (Stage0, Stage0)
                | (Stage1, Stage1) ->
                    if subtype_poly_type pty1 pty2 then
                      return ()
                    else
                      err (NotASubtypeAboutValue(rng, x2, pty1, pty2))

                | _ ->
                    err (NotASubtypeAboutValueStage(rng, x2, stage1, stage2))
          )
          ~a:(fun csnm2 { macro_type = macty2; _ } res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_macro csnm2 with
            | None ->
                err (MissingRequiredMacroName(rng, csnm2, macty2))

            | Some({ macro_type = macty1; _ }) ->
                if subtype_macro_type macty1 macty2 then
                  return ()
                else
                  err (NotASubtypeAboutMacro(rng, csnm2, macty1, macty2))
          )
          ~c:(fun ctornm2 centry2 res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_constructor ctornm2 with
            | None ->
                err (MissingRequiredConstructorName(rng, ctornm2, centry2))

            | Some(centry1) ->
                let tyscheme1 = centry1.ctor_parameter in
                let tyscheme2 = centry2.ctor_parameter in
                if subtype_type_scheme tyscheme1 tyscheme2 then
                  return ()
                else
                  err (NotASubtypeAboutConstructor(rng, ctornm2, tyscheme1, tyscheme2))
          )
          ~f:(fun tynm2 pty2 res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_dummy_fold tynm2 with
            | None ->
                begin
                  match ssig2 |> StructSig.find_type tynm2 with
                  | None ->
                      assert false

                  | Some(tentry2) ->
                      let (bids, _) = tentry2.type_scheme in
                      let arity = List.length bids in
                      err (MissingRequiredTypeName(rng, tynm2, arity))
                end

            | Some(pty1) ->
                if subtype_poly_type pty1 pty2 then
                  return ()
                else
                  begin
                    match (ssig1 |> StructSig.find_type tynm2, ssig2 |> StructSig.find_type tynm2) with
                    | (Some(tentry1), Some(tentry2)) ->
                        err (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))

                    | _ ->
                        assert false
                  end
          )
          ~t:(fun tynm2 tentry2 res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_type tynm2 with
            | None ->
                let arity =
                  let (bids, _) = tentry2.type_scheme in
                  List.length bids
                in
                err (MissingRequiredTypeName(rng, tynm2, arity))

            | Some(tentry1) ->
                let tyscheme1 = tentry1.type_scheme in
                let tyscheme2 = tentry2.type_scheme in
                let b1 = subtype_type_scheme tyscheme1 tyscheme2 in
                let b2 = subtype_type_scheme tyscheme2 tyscheme1 in
                if b1 && b2 then
                  return ()
                else
                  err (NotASubtypeAboutType(rng, tynm2, tentry1, tentry2))
          )
          ~m:(fun modnm2 { mod_signature = modsig2; _ } res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_module modnm2 with
            | None ->
                err (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some({ mod_signature = modsig1; _ }) ->
                subtype_concrete_with_concrete rng modsig1 modsig2
          )
          ~s:(fun signm2 absmodsig2 res ->
            res >>= fun () ->
            match ssig1 |> StructSig.find_signature signm2 with
            | None ->
                err (MissingRequiredSignatureName(rng, signm2, absmodsig2))

            | Some(absmodsig1) ->
                let* () = subtype_abstract_with_abstract rng absmodsig1 absmodsig2 in
                let* () = subtype_abstract_with_abstract rng absmodsig2 absmodsig1 in
                return ()
          )
          (return ())

  | _ ->
      err (NotASubtypeSignature(rng, modsig1, modsig2))


and subtype_concrete_with_abstract (rng : Range.t) (modsig1 : signature) (absmodsig2 : signature abstracted) : substitution ok =
  let open ResultMonad in
  let (_quant2, modsig2) = absmodsig2 in
  let* subst = lookup_struct rng modsig1 modsig2 in
  let modsig2 = modsig2 |> substitute_concrete subst in
  let* () = subtype_concrete_with_concrete rng modsig1 modsig2 in
  return subst


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

    | (InlineCommandType(cmdargtys1), InlineCommandType(cmdargtys2)) -> aux_cmd_list cmdargtys1 cmdargtys2
    | (BlockCommandType(cmdargtys1), BlockCommandType(cmdargtys2))   -> aux_cmd_list cmdargtys1 cmdargtys2
    | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2))     -> aux_cmd_list cmdargtys1 cmdargtys2

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

  | (Some(PolyRowFree(_)), _) | (_, Some(PolyRowFree(_))) ->
      assert false

  | (None, Some(PolyRowBound(_brid2))) ->
      false

  | (Some(PolyRowBound(brid1)), _) ->
      let opt = subtype_label_map_inclusive internbid internbrid pty_labmap1 pty_labmap2 in
      begin
        match opt with
        | None                  -> false
        | Some(pty_labmap_diff) -> internbrid brid1 (NormalizedRow(pty_labmap_diff, rowvar2_opt))
      end


and subtype_label_map_with_equal_domain (internbid : type_intern) (internbrid : row_intern) (pty_labmap1 : poly_type_body LabelMap.t) (pty_labmap2 : poly_type_body LabelMap.t) : bool =
  LabelMap.merge (fun _label pty1_opt pty2_opt ->
    match (pty1_opt, pty2_opt) with
    | (Some(pty1), Some(pty2)) -> Some(subtype_poly_type_impl internbid internbrid (Poly(pty1)) (Poly(pty2)))
    | _                        -> Some(false)
  ) pty_labmap1 pty_labmap2 |> LabelMap.for_all (fun _label b -> b)


(* Checks that `dom pty_labmap1 ⊆ dom pty_labmap2` and
   `∀label ∈ dom pty_labmap1. pty_labmap1(label) <: pty_labmap2(label)`
   by referring and updating `internbid` and `internbrid`. *)
and subtype_label_map_inclusive (internbid : type_intern) (internbrid : row_intern) (pty_labmap1 : poly_type_body LabelMap.t) (pty_labmap2 : poly_type_body LabelMap.t) : (poly_type_body LabelMap.t) option =
  let merged =
    LabelMap.merge (fun _label pty1_opt pty2_opt ->
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
        TypeConv.poly_type_equal pty pty2
  in
  let internbrid (brid1 : BoundRowID.t) (nomprow2 : normalized_poly_row) : bool =
    match BoundRowIDHashTable.find_opt brid_ht brid1 with
    | None ->
        BoundRowIDHashTable.add brid_ht brid1 nomprow2;
        true

    | Some(nomprow) ->
        TypeConv.normalized_poly_row_equal nomprow nomprow2
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
              match map |> BoundIDMap.find_opt bid2 with
              | None      -> false
              | Some(bid) -> BoundID.equal bid bid1
            end

        | _ ->
            false
      in
      let internbrid (_brid1 : BoundRowID.t) (_nomprow2 : normalized_poly_row) : bool =
        false
      in
      subtype_poly_type_impl internbid internbrid pty1 pty2


and subtype_macro_type (macty1 : poly_macro_type) (macty2 : poly_macro_type) : bool =
  let bid_ht = BoundIDHashTable.create 32 in
  let brid_ht = BoundRowIDHashTable.create 32 in
  let internbid (bid1 : BoundID.t) (pty2 : poly_type) : bool =
    match BoundIDHashTable.find_opt bid_ht bid1 with
    | None ->
        BoundIDHashTable.add bid_ht bid1 pty2;
        true

    | Some(pty) ->
        TypeConv.poly_type_equal pty pty2
  in
  let internbrid (brid1 : BoundRowID.t) (nomprow2 : normalized_poly_row) : bool =
    match BoundRowIDHashTable.find_opt brid_ht brid1 with
    | None ->
        BoundRowIDHashTable.add brid_ht brid1 nomprow2;
        true

    | Some(nomprow) ->
        TypeConv.normalized_poly_row_equal nomprow nomprow2
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
  | (InlineMacroType(macparamtys1), InlineMacroType(macparamtys2)) -> aux macparamtys1 macparamtys2
  | (BlockMacroType(macparamtys1), BlockMacroType(macparamtys2))   -> aux macparamtys1 macparamtys2
  | _                                                              -> false
