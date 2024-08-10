
open SyntaxBase
open Types
open StaticEnv


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let lift_argument_type f = function
  | CommandArgType(tylabmap, ty) -> CommandArgType(tylabmap |> LabelMap.map f, f ty)


let rec unlink ((_, tymain) as ty) =
  match tymain with
  | TypeVariable(Updatable{contents = MonoLink(ty)}) -> unlink ty
  | _                                                -> ty


let rec erase_range_of_type (ty : mono_type) : mono_type =
  let iter = erase_range_of_type in
  let rng = Range.dummy "erased" in
  let (_, tymain) = ty in
    match tymain with
    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable(tvref) ->
              begin
                match !tvref with
                | MonoFree(_fid) -> (rng, tymain)
                | MonoLink(ty)   -> erase_range_of_type ty
              end

        | MustBeBound(_) ->
            (rng, tymain)
      end

    | BaseType(_)                       -> (rng, tymain)
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(erase_range_of_row optrow, iter tydom, iter tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map iter tys))
    | RecordType(row)                   -> (rng, RecordType(erase_range_of_row row))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map iter tyargs, tyid))
    | ListType(tycont)                  -> (rng, ListType(iter tycont))
    | RefType(tycont)                   -> (rng, RefType(iter tycont))
    | InlineCommandType(tys)            -> (rng, InlineCommandType(List.map (lift_argument_type iter) tys))
    | BlockCommandType(tys)             -> (rng, BlockCommandType(List.map (lift_argument_type iter) tys))
    | MathCommandType(tys)              -> (rng, MathCommandType(List.map (lift_argument_type iter) tys))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))


and erase_range_of_row (row : mono_row) =
  match row with
  | RowEmpty ->
      RowEmpty

  | RowCons((_, label), ty, tail) ->
      let rlabel = (Range.dummy "erased", label) in
      RowCons(rlabel, erase_range_of_type ty, erase_range_of_row tail)

  | RowVar(UpdatableRow{contents = MonoRowLink(optrow)}) ->
      erase_range_of_row optrow

  | RowVar(UpdatableRow{contents = MonoRowFree(_)}) ->
      row

  | RowVar(MustBeBoundRow(_)) ->
      row


let rec instantiate_impl  : 'a 'b. (Range.t -> poly_type_variable -> ('a, 'b) typ) -> (poly_row_variable -> ('a, 'b) row) -> poly_type_body -> ('a, 'b) typ =
fun intern_ty intern_row pty ->
  let aux = instantiate_impl intern_ty intern_row in
  let aux_row = instantiate_row_impl intern_ty intern_row in
  let (rng, ptymain) = pty in
  match ptymain with
  | TypeVariable(ptv)              -> intern_ty rng ptv
  | FuncType(optrow, tydom, tycod) -> (rng, FuncType(aux_row optrow, aux tydom, aux tycod))
  | ProductType(tys)               -> (rng, ProductType(TupleList.map aux tys))
  | RecordType(row)                -> (rng, RecordType(aux_row row))
  | DataType(tyargs, tyid)         -> (rng, DataType(List.map aux tyargs, tyid))
  | ListType(tysub)                -> (rng, ListType(aux tysub))
  | RefType(tysub)                 -> (rng, RefType(aux tysub))
  | BaseType(bty)                  -> (rng, BaseType(bty))
  | InlineCommandType(tys)         -> (rng, InlineCommandType(List.map (lift_argument_type aux) tys))
  | BlockCommandType(tys)          -> (rng, BlockCommandType(List.map (lift_argument_type aux) tys))
  | MathCommandType(tys)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tys))
  | CodeType(tysub)                -> (rng, CodeType(aux tysub))


and instantiate_row_impl : 'a 'b. (Range.t -> poly_type_variable -> ('a, 'b) typ) -> (poly_row_variable -> ('a, 'b) row) -> poly_row -> ('a, 'b) row =
fun intern_ty intern_row prow ->
  let aux = instantiate_impl intern_ty intern_row in
  let aux_row = instantiate_row_impl intern_ty intern_row in
  match prow with
  | RowEmpty                   -> RowEmpty
  | RowCons(rlabel, pty, tail) -> RowCons(rlabel, aux pty, aux_row tail)
  | RowVar(prv)                -> intern_row prv


let make_type_instantiation_intern (lev : level) (qtfbl : quantifiability) (bid_ht : mono_type_variable BoundIDHashTable.t) =
  let intern_ty (rng : Range.t) (ptv : poly_type_variable) : mono_type =
    match ptv with
    | PolyFree(tvuref) ->
        (rng, TypeVariable(Updatable(tvuref)))

    | PolyBound(bid) ->
        begin
          match BoundIDHashTable.find_opt bid_ht bid with
          | Some(tv_new) ->
              (rng, TypeVariable(tv_new))

          | None ->
              let tv =
                let fid = FreeID.fresh lev (qtfbl = Quantifiable) in
                let tvref = ref (MonoFree(fid)) in
                Updatable(tvref)
              in
              BoundIDHashTable.add bid_ht bid tv;
              (rng, TypeVariable(tv))
        end
  in
  intern_ty


let make_row_instantiation_intern (lev : level) (brid_ht : mono_row_variable BoundRowIDHashTable.t) =
  let intern_row (prv : poly_row_variable) : mono_row =
    match prv with
    | PolyRowFree(rvref) ->
        RowVar(rvref)

    | PolyRowBound(brid) ->
        begin
          match BoundRowIDHashTable.find_opt brid_ht brid with
          | Some(rv) ->
              RowVar(rv)

          | None ->
              let rv =
                let frid = FreeRowID.fresh lev (BoundRowID.get_label_set brid) in
                let rvref = ref (MonoRowFree(frid)) in
                UpdatableRow(rvref)
              in
              BoundRowIDHashTable.add brid_ht brid rv;
              RowVar(rv)
        end
  in
  intern_row


let instantiate (lev : level) (qtfbl : quantifiability) ((Poly(pty)) : poly_type) : mono_type =
  let bid_ht = BoundIDHashTable.create 32 in
  let brid_ht = BoundRowIDHashTable.create 32 in
  let intern_ty = make_type_instantiation_intern lev qtfbl bid_ht in
  let intern_row = make_row_instantiation_intern lev brid_ht in
  instantiate_impl intern_ty intern_row pty


let instantiate_by_map_mono (bidmap : mono_type BoundIDMap.t) (Poly(pty) : poly_type) : mono_type =
  let intern_ty (rng : Range.t) (ptv : poly_type_variable) =
    match ptv with
    | PolyFree(tvuref) ->
        (rng, TypeVariable(Updatable(tvuref)))

    | PolyBound(bid) ->
        begin
          match bidmap |> BoundIDMap.find_opt bid with
          | None     -> assert false
          | Some(ty) -> ty
        end
  in
  let intern_row (prv : poly_row_variable) =
    match prv with
    | PolyRowFree(rvref) -> RowVar(rvref)
    | PolyRowBound(_)    -> assert false
  in
  instantiate_impl intern_ty intern_row pty


let instantiate_by_map_poly (bidmap : poly_type_body BoundIDMap.t) (Poly(pty) : poly_type) : poly_type =
  let intern_ty (rng : Range.t) (ptv : poly_type_variable) : poly_type_body =
    match ptv with
    | PolyFree(_) ->
        (rng, TypeVariable(ptv))

    | PolyBound(bid) ->
        begin
          match bidmap |> BoundIDMap.find_opt bid with
          | None      -> assert false
          | Some(pty) -> pty
        end
  in
  let intern_row prv =
    RowVar(prv)
  in
  Poly(instantiate_impl intern_ty intern_row pty)


let instantiate_macro_type (lev : level) (qtfbl : quantifiability) (pmacty : poly_macro_type) : mono_macro_type =
  let bid_ht = BoundIDHashTable.create 32 in
  let brid_ht = BoundRowIDHashTable.create 32 in
  let intern_ty = make_type_instantiation_intern lev qtfbl bid_ht in
  let intern_row = make_row_instantiation_intern lev brid_ht in
  let aux = function
    | LateMacroParameter(pty)  -> LateMacroParameter(instantiate_impl intern_ty intern_row pty)
    | EarlyMacroParameter(pty) -> EarlyMacroParameter(instantiate_impl intern_ty intern_row pty)
  in
  match pmacty with
  | InlineMacroType(pmacparamtys) -> InlineMacroType(pmacparamtys |> List.map aux)
  | BlockMacroType(pmacparamtys)  -> BlockMacroType(pmacparamtys |> List.map aux)


let lift_poly_general (intern_ty : FreeID.t -> BoundID.t option) (intern_row : FreeRowID.t -> LabelSet.t -> BoundRowID.t option) (ty : mono_type) : poly_type_body =
  let rec iter ((rng, tymain) : mono_type) =
    match tymain with
    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable(tvuref) ->
              begin
                match !tvuref with
                | MonoLink(tyl) ->
                    iter tyl

                | MonoFree(fid) ->
                    let ptvi =
                      match intern_ty fid with
                      | None      -> PolyFree(tvuref)
                      | Some(bid) -> PolyBound(bid)
                    in
                    (rng, TypeVariable(ptvi))
              end

          | MustBeBound(mbbid) ->
              let bid = MustBeBoundID.to_bound_id mbbid in
              (rng, TypeVariable(PolyBound(bid)))
        end

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(generalize_row LabelSet.empty optrow, iter tydom, iter tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map iter tys))
    | RecordType(row)                   -> (rng, RecordType(generalize_row LabelSet.empty row))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map iter tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(iter tysub))
    | RefType(tysub)                    -> (rng, RefType(iter tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | InlineCommandType(tys)            -> (rng, InlineCommandType(List.map (lift_argument_type iter) tys))
    | BlockCommandType(tys)             -> (rng, BlockCommandType(List.map (lift_argument_type iter) tys))
    | MathCommandType(tys)              -> (rng, MathCommandType(List.map (lift_argument_type iter) tys))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))

  and generalize_row (labset : LabelSet.t) = function
    | RowEmpty ->
        RowEmpty

    | RowCons(rlabel, ty, tail) ->
        let (_, label) = rlabel in
        RowCons(rlabel, iter ty, generalize_row (labset |> LabelSet.add label) tail)

    | RowVar(UpdatableRow(orviref) as rv0) ->
        begin
          match !orviref with
          | MonoRowFree(frid) ->
              begin
                match intern_row frid labset with
                | None ->
                    RowVar(PolyRowFree(rv0))

                | Some(brid) ->
                    RowVar(PolyRowBound(brid))
              end

          | MonoRowLink(row) ->
              generalize_row labset row
        end

    | RowVar(MustBeBoundRow(mbbrid)) ->
        let brid = MustBeBoundRowID.to_bound_id mbbrid in
        RowVar(PolyRowBound(brid))
  in
  iter ty


let make_type_generalization_intern (lev : level) (tvid_ht : BoundID.t FreeIDHashTable.t) =
  let intern_ty (fid : FreeID.t) : BoundID.t option =
    if not (FreeID.get_quantifiability fid && Level.less_than lev (FreeID.get_level fid)) then
      None
    else
      match FreeIDHashTable.find_opt tvid_ht fid with
      | Some(bid) ->
          Some(bid)

      | None ->
          let bid = BoundID.fresh () in
          FreeIDHashTable.add tvid_ht fid bid;
          Some(bid)
  in
  intern_ty


let make_row_generalization_intern (lev : level) (rvid_ht : BoundRowID.t FreeRowIDHashTable.t) =
  let intern_row (frid : FreeRowID.t) (labset : LabelSet.t) : BoundRowID.t option =
    if not (Level.less_than lev (FreeRowID.get_level frid)) then
      None
    else
      match FreeRowIDHashTable.find_opt rvid_ht frid with
      | Some(brid) ->
          Some(brid)

      | None ->
          let brid = BoundRowID.fresh labset in
          FreeRowIDHashTable.add rvid_ht frid brid;
          Some(brid)
  in
  intern_row


let generalize (lev : level) (ty : mono_type) : poly_type =
  let tvid_ht = FreeIDHashTable.create 32 in
  let rvid_ht = FreeRowIDHashTable.create 32 in
  let intern_ty = make_type_generalization_intern lev tvid_ht in
  let intern_row = make_row_generalization_intern lev rvid_ht in
  Poly(lift_poly_general intern_ty intern_row ty)


let lift_poly_body =
  lift_poly_general (fun _ -> None) (fun _ _ -> None)


let lift_poly (ty : mono_type) : poly_type =
  Poly(lift_poly_body ty)


let generalize_macro_type (macty : mono_macro_type) : poly_macro_type =
  let tvid_ht = FreeIDHashTable.create 32 in
  let rvid_ht = FreeRowIDHashTable.create 32 in
  let intern_ty = make_type_generalization_intern Level.bottom tvid_ht in
  let intern_row = make_row_generalization_intern Level.bottom rvid_ht in
  let aux = function
    | LateMacroParameter(ty)  -> LateMacroParameter(lift_poly_general intern_ty intern_row ty)
    | EarlyMacroParameter(ty) -> EarlyMacroParameter(lift_poly_general intern_ty intern_row ty)
  in
  match macty with
  | InlineMacroType(macparamtys) -> InlineMacroType(macparamtys |> List.map aux)
  | BlockMacroType(macparamtys)  -> BlockMacroType(macparamtys |> List.map aux)


(* Normalizes the polymorphic row `prow`. Here, `MonoRow` is not supposed to occur in `prow`. *)
let normalize_poly_row (prow : poly_row) : normalized_poly_row =
  let rec aux plabmap = function
    | RowCons((_, label), pty, prow) -> aux (plabmap |> LabelMap.add label pty) prow
    | RowVar(prv)                    -> NormalizedRow(plabmap, Some(prv))
    | RowEmpty                       -> NormalizedRow(plabmap, None)
  in
  aux LabelMap.empty prow


let normalize_mono_row (row : mono_row) : normalized_mono_row =
  let rec aux labmap = function
    | RowCons((_, label), ty, row)                       -> aux (labmap |> LabelMap.add label ty) row
    | RowVar(UpdatableRow{contents = MonoRowLink(row)})  -> aux labmap row
    | RowVar(UpdatableRow{contents = MonoRowFree(frid)}) -> NormalizedRow(labmap, Some(NormFreeRow(frid)))
    | RowVar(MustBeBoundRow(mbbrid))                     -> NormalizedRow(labmap, Some(NormMustBeBoundRow(mbbrid)))
    | RowEmpty                                           -> NormalizedRow(labmap, None)
  in
  aux LabelMap.empty row


let apply_type_scheme_poly (tyscheme : type_scheme) (ptys : poly_type_body list) : poly_type option =
  let (bids, pty_body) = tyscheme in
  match List.combine bids ptys with
  | exception Invalid_argument(_) ->
      None

  | zipped ->
      let bidmap =
        zipped |> List.fold_left (fun bidmap (bid, pty) ->
          bidmap |> BoundIDMap.add bid pty
        ) BoundIDMap.empty
      in
      Some(instantiate_by_map_poly bidmap pty_body)


let apply_type_scheme_mono (tyscheme : type_scheme) (tys : mono_type list) : mono_type option =
  let (bids, pty_body) = tyscheme in
  match List.combine bids tys with
  | exception Invalid_argument(_) ->
      None

  | zipped ->
      let bidmap =
        zipped |> List.fold_left (fun bidmap (bid, ty) ->
          bidmap |> BoundIDMap.add bid ty
        ) BoundIDMap.empty
      in
      Some(instantiate_by_map_mono bidmap pty_body)


let make_opaque_type_scheme (arity : int) (tyid : TypeID.t) : type_scheme =
  let rng = Range.dummy "add_variant_types" in
  let bids = List.init arity (fun _ -> BoundID.fresh ()) in
  let ptys = bids |> List.map (fun bid -> (rng, TypeVariable(PolyBound(bid)))) in
  (bids, Poly((rng, DataType(ptys, tyid))))


let get_opaque_type (tyscheme : type_scheme) : TypeID.t option =
  let (bids, Poly(pty_body)) = tyscheme in
  match pty_body with
  | (_, DataType(ptys, tyid)) ->
      begin
        match List.combine bids ptys with
        | exception Invalid_argument(_) ->
            None

        | zipped ->
            if
              zipped |> List.for_all (fun (bid, pty) ->
                match pty with
                | (_, TypeVariable(PolyBound(bid0))) -> BoundID.equal bid bid0
                | _                                  -> false
              )
            then
              Some(tyid)
            else
              None
      end

  | _ ->
      None


let kind_equal (kd1 : kind) (kd2 : kind) : bool =
  let Kind(bkds1) = kd1 in
  let Kind(bkds2) = kd2 in
  match List.combine bkds1 bkds2 with
  | exception Invalid_argument(_) -> false
  | zipped                        -> zipped |> List.for_all (fun (TypeKind, TypeKind) -> true)


let rec poly_type_equal (Poly(pty1) : poly_type) (Poly(pty2) : poly_type) : bool =
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

    | (InlineCommandType(cmdargtys1), InlineCommandType(cmdargtys2))
    | (BlockCommandType(cmdargtys1), BlockCommandType(cmdargtys2))
    | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2)) ->
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
      LabelMap.merge (fun _label pty1_opt pty2_opt ->
        match (pty1_opt, pty2_opt) with
        | (Some(pty1), Some(pty2)) -> Some(aux pty1 pty2)
        | _                        -> Some(false)
      ) ptylabmap1 ptylabmap2
    in
    labmap |> LabelMap.for_all (fun _label b -> b)

  in
  aux pty1 pty2


and poly_row_equal (prow1 : poly_row) (prow2 : poly_row) : bool =
  normalized_poly_row_equal (normalize_poly_row prow1) (normalize_poly_row prow2)


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
    | (None, None)                                           -> true
    | (Some(PolyRowBound(brid1)), Some(PolyRowBound(brid2))) -> BoundRowID.equal brid1 brid2
    | _                                                      -> false
  end
