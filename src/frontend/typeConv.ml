
open SyntaxBase
open Types
open StaticEnv


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let lift_argument_type f = function
  | CommandArgType(tylabmap, ty) -> CommandArgType(tylabmap |> LabelMap.map f, f ty)


let lift_manual_common f = function
  | MMandatoryArgumentType(mnty) -> f mnty
  | MOptionalArgumentType(mnty)  -> f mnty


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
                | MonoFree(fid) -> (rng, tymain)
                | MonoLink(ty)  -> erase_range_of_type ty
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
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))


and erase_range_of_row (row : mono_row) =
  match row with
  | RowEmpty ->
      RowEmpty

  | RowCons((_, label), ty, tail) ->
      let rlabel = (Range.dummy "erased", label) in
      RowCons(rlabel, erase_range_of_type ty, erase_range_of_row tail)

  | RowVar(UpdatableRow{contents = MonoORLink(optrow)}) ->
      erase_range_of_row optrow

  | RowVar(UpdatableRow{contents = MonoORFree(_)}) ->
      row

  | RowVar(MustBeBoundRow(_)) ->
      row


let rec instantiate_aux (bid_ht : mono_type_variable BoundIDHashTable.t) (brid_ht : mono_row_variable BoundRowIDHashTable.t) lev qtfbl ((rng, ptymain) : poly_type_body) =
  let aux = instantiate_aux bid_ht brid_ht lev qtfbl in
  let aux_row = instantiate_row_aux bid_ht brid_ht lev qtfbl in
    match ptymain with
    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tv) ->
              (rng, TypeVariable(tv))

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
        end
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_row optrow, aux tydom, aux tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map aux tys))
    | RecordType(row)                   -> (rng, RecordType(aux_row row))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map aux tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(aux tysub))


and instantiate_row_aux bid_ht brid_ht lev qtfbl row : mono_row =
  let aux = instantiate_aux bid_ht brid_ht lev qtfbl in
  let aux_row = instantiate_row_aux bid_ht brid_ht lev qtfbl in
  match row with
  | RowEmpty ->
      RowEmpty

  | RowCons(rlabel, pty, tail) ->
      RowCons(rlabel, aux pty, aux_row tail)

  | RowVar(PolyORFree(rvref)) ->
      RowVar(rvref)

  | RowVar(PolyORBound(brid)) ->
      begin
        match BoundRowIDHashTable.find_opt brid_ht brid with
        | Some(rv) ->
            RowVar(rv)

        | None ->
            let rv =
              let frid = FreeRowID.fresh lev (BoundRowID.get_label_set brid) in
              let rvref = ref (MonoORFree(frid)) in
              UpdatableRow(rvref)
            in
            BoundRowIDHashTable.add brid_ht brid rv;
            RowVar(rv)
      end



let instantiate (lev : level) (qtfbl : quantifiability) ((Poly(pty)) : poly_type) : mono_type =
  let bid_ht : mono_type_variable BoundIDHashTable.t = BoundIDHashTable.create 32 in
  let brid_ht : mono_row_variable BoundRowIDHashTable.t = BoundRowIDHashTable.create 32 in
  instantiate_aux bid_ht brid_ht lev qtfbl pty


let instantiate_type_scheme (type a) (type b) (freef : Range.t -> mono_type_variable -> (a, b) typ) (orfreef : mono_row_variable -> (a, b) row) (pairlst : ((a, b) typ * BoundID.t) list) (Poly(pty) : poly_type) : (a, b) typ =
  let bid_to_type_ht : ((a, b) typ) BoundIDHashTable.t = BoundIDHashTable.create 32 in

  let rec aux ((rng, ptymain) : poly_type_body) : (a, b) typ =
    match ptymain with
    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tvref) ->
              freef rng tvref

          | PolyBound(bid) ->
              begin
                match BoundIDHashTable.find_opt bid_to_type_ht bid with
                | None        -> assert false
                | Some(tysub) -> tysub
              end
        end

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_row optrow, aux tydom, aux tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map aux tys))
    | RecordType(row)                   -> (rng, RecordType(aux_row row))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map aux tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bt)                      -> (rng, BaseType(bt))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(aux tysub))

  and aux_row = function
    | RowEmpty                  -> RowEmpty
    | RowCons(rlabel, ty, tail) -> RowCons(rlabel, aux ty, aux_row tail)
    | RowVar(PolyORFree(rvref)) -> orfreef rvref
    | RowVar(PolyORBound(brid)) -> failwith "TODO: instantiate_type_scheme, RowVar, PolyORBound"
  in
  begin
    pairlst |> List.iter (fun (tyarg, bid) -> BoundIDHashTable.add bid_to_type_ht bid tyarg);
    aux pty
  end


let lift_poly_general (ptv : FreeID.t -> bool) (prv : FreeRowID.t -> bool) (ty : mono_type) : poly_type_body =
  let tvidht = FreeIDHashTable.create 32 in
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
                      if not (ptv fid) then
                        PolyFree(tv)
                      else
                        begin
                          match FreeIDHashTable.find_opt tvidht fid with
                          | Some(bid) ->
                              PolyBound(bid)

                          | None ->
                              let bid = BoundID.fresh () in
                              FreeIDHashTable.add tvidht fid bid;
                              PolyBound(bid)
                        end
                    in
                      (rng, TypeVariable(ptvi))
              end

          | MustBeBound(mbbid) ->
              let bid = MustBeBoundID.to_bound_id mbbid in
              (rng, TypeVariable(PolyBound(bid)))
        end

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(generalize_row optrow, iter tydom, iter tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map iter tys))
    | RecordType(row)                   -> (rng, RecordType(generalize_row row))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map iter tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(iter tysub))
    | RefType(tysub)                    -> (rng, RefType(iter tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))

  and generalize_row = function
    | RowEmpty ->
        RowEmpty

    | RowCons(rlabel, ty, tail) ->
        RowCons(rlabel, iter ty, generalize_row tail)

    | RowVar(UpdatableRow(orviref) as rv0) ->
        begin
          match !orviref with
          | MonoORFree(frid) ->
              if prv frid then
                RowEmpty
              else
                RowVar(PolyORFree(rv0))

          | MonoORLink(row) ->
              generalize_row row
        end

    | RowVar(MustBeBoundRow(mbbrid)) ->
        let brid = MustBeBoundRowID.to_bound_id mbbrid in
        RowVar(PolyORBound(brid))
  in
  iter ty


let check_level (lev : Level.t) (ty : mono_type) : bool =
  let rec iter (_, tymain) =
    match tymain with
    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable(tvuref) ->
              begin
                match !tvuref with
                | MonoLink(ty)  -> iter ty
                | MonoFree(fid) -> Level.less_than lev (FreeID.get_level fid)
              end

          | MustBeBound(mbbid) ->
              Level.less_than lev (MustBeBoundID.get_level mbbid)
        end

    | ProductType(tys)               -> tys |> TupleList.to_list |> List.for_all iter
    | RecordType(row)                -> iter_row row
    | FuncType(optrow, tydom, tycod) -> iter_row optrow && iter tydom && iter tycod
    | RefType(tycont)                -> iter tycont
    | BaseType(_)                    -> true
    | ListType(tycont)               -> iter tycont
    | DataType(tyargs, _)            -> List.for_all iter tyargs

    | HorzCommandType(cmdargtylst)
    | VertCommandType(cmdargtylst)
    | MathCommandType(cmdargtylst) ->
        List.for_all iter_cmd cmdargtylst

    | CodeType(tysub) ->
        iter tysub

  and iter_cmd = function
    | CommandArgType(tylabmap, ty) ->
        tylabmap |> LabelMap.for_all (fun _label -> iter) && iter ty

  and iter_row = function
    | RowEmpty ->
        true

    | RowCons(_, ty, tail) ->
        iter ty && iter_row tail

    | RowVar(UpdatableRow(rvref)) ->
        begin
          match !rvref with
          | MonoORFree(frid) -> Level.less_than lev (FreeRowID.get_level frid)
          | MonoORLink(row)  -> iter_row row
        end

    | RowVar(MustBeBoundRow(mbbrid)) ->
        Level.less_than lev (MustBeBoundRowID.get_level mbbrid)

  in
  iter ty


let generalize (lev : level) (ty : mono_type) : poly_type =
  let ptv (fid : FreeID.t) : bool =
    FreeID.get_quantifiability fid && Level.less_than lev (FreeID.get_level fid)
  in
  let prv (frid : FreeRowID.t) : bool =
    not (Level.less_than lev (FreeRowID.get_level frid))
  in
  Poly(lift_poly_general ptv prv ty)


let lift_poly_body =
  lift_poly_general (fun _ -> false) (fun _ -> false)


let lift_poly (ty : mono_type) : poly_type =
  Poly(lift_poly_body ty)


let rec unlift_aux pty =
  let aux = unlift_aux in
  let (rng, ptymain) = pty in
  let ptymainnew =
    match ptymain with
    | BaseType(bt) -> BaseType(bt)

    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tvref) -> TypeVariable(tvref)
          | PolyBound(_)    -> raise Exit
        end

    | FuncType(poptrow, pty1, pty2)   -> FuncType(unlift_aux_row poptrow, aux pty1, aux pty2)
    | ProductType(ptys)               -> ProductType(TupleList.map aux ptys)
    | RecordType(prow)                -> RecordType(unlift_aux_row prow)
    | ListType(ptysub)                -> ListType(aux ptysub)
    | RefType(ptysub)                 -> RefType(aux ptysub)
    | DataType(ptyargs, tyid)         -> DataType(List.map aux ptyargs, tyid)
    | HorzCommandType(catyl)          -> HorzCommandType(List.map unlift_aux_cmd catyl)
    | VertCommandType(catyl)          -> VertCommandType(List.map unlift_aux_cmd catyl)
    | MathCommandType(catyl)          -> MathCommandType(List.map unlift_aux_cmd catyl)
    | CodeType(ptysub)                -> CodeType(aux ptysub)
  in
  (rng, ptymainnew)


and unlift_aux_cmd = function
  | CommandArgType(ptylabmap, pty) ->
      CommandArgType(ptylabmap |> LabelMap.map unlift_aux, unlift_aux pty)


and unlift_aux_row = function
  | RowEmpty                   -> RowEmpty
  | RowCons(rlabel, pty, tail) -> RowCons(rlabel, unlift_aux pty, unlift_aux_row tail)
  | RowVar(PolyORFree(rvref))  -> RowVar(rvref)
  | RowVar(PolyORBound(_))     -> raise Exit


let unlift_poly (pty : poly_type_body) : mono_type option =
  try Some(unlift_aux pty) with
  | Exit -> None


let unlift_row poptrow =
  try Some(unlift_aux_row poptrow) with
  | Exit -> None


let apply_type_scheme_poly (tyscheme : type_scheme) (ptys : poly_type_body list) =
  failwith "TODO: apply_type_scheme_poly"


let apply_type_scheme_mono (tyscheme : type_scheme) (ptys : mono_type list) : mono_type option =
  failwith "TODO: apply_type_scheme_mono"


let make_opaque_type_scheme (arity : int) (tyid : TypeID.t) =
  failwith "TODO: make_opaque_type_scheme"
