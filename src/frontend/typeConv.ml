
open Types
open StaticEnv


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let lift_argument_type f = function
  | MandatoryArgumentType(ty) -> MandatoryArgumentType(f ty)
  | OptionalArgumentType(ty)  -> OptionalArgumentType(f ty)


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
                | MonoFree(fid) ->
                    let fentry = KindStore.get_free_id fid in
                    KindStore.set_free_id fid { fentry with mono_kind = erase_range_of_kind fentry.mono_kind };
                    (rng, tymain)

                | MonoLink(ty) ->
                    erase_range_of_type ty
              end

        | MustBeBound(_) ->
            (rng, tymain)
      end

    | BaseType(_)                       -> (rng, tymain)
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(erase_range_of_option_row optrow, iter tydom, iter tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map iter tys))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value iter tyasc))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map iter tyargs, tyid))
    | ListType(tycont)                  -> (rng, ListType(iter tycont))
    | RefType(tycont)                   -> (rng, RefType(iter tycont))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))


and erase_range_of_kind (kd : mono_kind) =
  match kd with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value erase_range_of_type asc)


and erase_range_of_option_row (optrow : mono_option_row) =
  match optrow with
  | OptionRowEmpty ->
      optrow

  | OptionRowCons(ty, tail) ->
      OptionRowCons(erase_range_of_type ty, erase_range_of_option_row tail)

  | OptionRowVariable(UpdatableRow{contents = MonoORLink(optrow)}) ->
      erase_range_of_option_row optrow

  | OptionRowVariable(UpdatableRow{contents = MonoORFree(_)}) ->
      optrow


let rec instantiate_aux (bid_ht : mono_type_variable BoundIDHashTable.t) lev qtfbl ((rng, ptymain) : poly_type_body) =
  let aux = instantiate_aux bid_ht lev qtfbl in
  let aux_or = instantiate_option_row_aux bid_ht lev qtfbl in
    match ptymain with
    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tv) ->
              (rng, TypeVariable(tv))

          | PolyBound(bid) ->
              begin
                match BoundIDHashTable.find_opt bid_ht bid with
                | Some(tvrefnew) ->
                    (rng, TypeVariable(tvrefnew))

                | None ->
                    let bentry = KindStore.get_bound_id bid in
                    let kd = bentry.KindStore.poly_kind in
                    let kdfree = instantiate_kind_aux bid_ht lev qtfbl kd in
                    let tv =
                      let fid = FreeID.fresh () in
                      let fentry =
                        KindStore.{
                          level           = lev;
                          quantifiability = qtfbl;
                          mono_kind       = kdfree;
                        }
                      in
                      KindStore.set_free_id fid fentry;
                      let tvref = ref (MonoFree(fid)) in
                      Updatable(tvref)
                    in
                    begin
                      BoundIDHashTable.add bid_ht bid tv;
                      (rng, TypeVariable(tv))
                    end
              end
        end
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_or optrow, aux tydom, aux tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map aux tys))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map aux tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(aux tysub))


and instantiate_kind_aux bid_ht lev qtfbl (kd : poly_kind) : mono_kind =
  let aux = instantiate_aux bid_ht lev qtfbl in
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value aux tyasc)


and instantiate_option_row_aux bid_ht lev qtfbl optrow : mono_option_row =
  let aux = instantiate_aux bid_ht lev qtfbl in
  let aux_or = instantiate_option_row_aux bid_ht lev qtfbl in
    match optrow with
    | OptionRowEmpty                         -> OptionRowEmpty
    | OptionRowCons(pty, tail)               -> OptionRowCons(aux pty, aux_or tail)
    | OptionRowVariable(PolyORFree(orviref)) -> OptionRowVariable(orviref)


let instantiate (lev : level) (qtfbl : quantifiability) ((Poly(pty)) : poly_type) : mono_type =
  let bid_ht : mono_type_variable BoundIDHashTable.t = BoundIDHashTable.create 32 in
  instantiate_aux bid_ht lev qtfbl pty


let instantiate_kind (lev : level) (qtfbl : quantifiability) (pkd : poly_kind) : mono_kind =
  let bid_ht : mono_type_variable BoundIDHashTable.t = BoundIDHashTable.create 32 in
  instantiate_kind_aux bid_ht lev qtfbl pkd


let instantiate_type_scheme (type a) (type b) (freef : Range.t -> mono_type_variable -> (a, b) typ) (orfreef : mono_option_row_variable -> (a, b) option_row) (pairlst : ((a, b) typ * BoundID.t) list) (Poly(pty) : poly_type) : (a, b) typ =
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

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_or optrow, aux tydom, aux tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map aux tys))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map aux tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bt)                      -> (rng, BaseType(bt))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(aux tysub))

  and aux_or optrow =
    match optrow with
    | OptionRowEmpty                         -> OptionRowEmpty
    | OptionRowCons(ty, tail)                -> OptionRowCons(aux ty, aux_or tail)
    | OptionRowVariable(PolyORFree(orviref)) -> orfreef orviref
  in
  begin
    pairlst |> List.iter (fun (tyarg, bid) -> BoundIDHashTable.add bid_to_type_ht bid tyarg);
    aux pty
  end


let lift_poly_general (ptv : FreeID.t -> bool) (porv : OptionRowVarID.t -> bool) (ty : mono_type) : poly_type_body =
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
                              let fentry = KindStore.get_free_id fid in
                              let kd = fentry.KindStore.mono_kind in
                              let pkd = generalize_kind kd in
                              let bid = BoundID.fresh () in
                              KindStore.set_bound_id bid KindStore.{ poly_kind = pkd };
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

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(generalize_option_row optrow, iter tydom, iter tycod))
    | ProductType(tys)                  -> (rng, ProductType(TupleList.map iter tys))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value iter tyasc))
    | DataType(tyargs, tyid)            -> (rng, DataType(List.map iter tyargs, tyid))
    | ListType(tysub)                   -> (rng, ListType(iter tysub))
    | RefType(tysub)                    -> (rng, RefType(iter tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))

  and generalize_kind kd =
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value iter tyasc)

  and generalize_option_row optrow =
    match optrow with
    | OptionRowEmpty             -> OptionRowEmpty
    | OptionRowCons(ty, tail)    -> OptionRowCons(iter ty, generalize_option_row tail)

    | OptionRowVariable(UpdatableRow(orviref) as orv0) ->
        begin
          match !orviref with
          | MonoORFree(orv) ->
              if porv orv then
                OptionRowEmpty
              else
                OptionRowVariable(PolyORFree(orv0))

          | MonoORLink(optraw) ->
              generalize_option_row optrow
        end
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
                | MonoLink(ty) ->
                    iter ty

                | MonoFree(fid) ->
                    let fentry = KindStore.get_free_id fid in
                    Level.less_than lev fentry.KindStore.level
              end

          | MustBeBound(mbbid) ->
              Level.less_than lev (MustBeBoundID.get_level mbbid)
        end

    | ProductType(tys)               -> tys |> TupleList.to_list |> List.for_all iter
    | RecordType(tyasc)              -> Assoc.fold_value (fun b ty -> b && iter ty) true tyasc
    | FuncType(optrow, tydom, tycod) -> iter_or optrow && iter tydom && iter tycod
    | RefType(tycont)                -> iter tycont
    | BaseType(_)                    -> true
    | ListType(tycont)               -> iter tycont
    | DataType(tyargs, _)            -> List.for_all iter tyargs

    | HorzCommandType(cmdargtylst)
    | VertCommandType(cmdargtylst)
    | MathCommandType(cmdargtylst)
      ->
        List.for_all iter_cmd cmdargtylst

    | CodeType(tysub)                -> iter tysub

  and iter_cmd = function
    | MandatoryArgumentType(ty) -> iter ty
    | OptionalArgumentType(ty)  -> iter ty

  and iter_or = function
    | OptionRowEmpty ->
        true

    | OptionRowCons(ty, tail) ->
        iter ty && iter_or tail

    | OptionRowVariable(UpdatableRow(orvuref)) ->
        begin
          match !orvuref with
          | MonoORFree(orv)    -> Level.less_than lev (OptionRowVarID.get_level orv)
          | MonoORLink(optrow) -> iter_or optrow
        end

  in
  iter ty


let generalize (lev : level) (ty : mono_type) : poly_type =
  let ptv fid =
    let fentry = KindStore.get_free_id fid in
    let bkd =
      match fentry.KindStore.mono_kind with
      | UniversalKind   -> true
      | RecordKind(asc) -> Assoc.fold_value (fun b ty -> b && (check_level lev ty)) true asc
    in
    let is_quantifiable =
      match fentry.KindStore.quantifiability with
      | Quantifiable   -> true
      | Unquantifiable -> false
    in
    is_quantifiable && Level.less_than lev fentry.KindStore.level && bkd
  in
  let porv orv =
    not (Level.less_than lev (OptionRowVarID.get_level orv))
  in
  Poly(lift_poly_general ptv porv ty)


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

    | FuncType(optrow, pty1, pty2)    -> FuncType(unlift_aux_or optrow, aux pty1, aux pty2)
    | ProductType(ptys)               -> ProductType(TupleList.map aux ptys)
    | RecordType(ptyasc)              -> RecordType(Assoc.map_value aux ptyasc)
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
  | MandatoryArgumentType(pty) -> MandatoryArgumentType(unlift_aux pty)
  | OptionalArgumentType(pty)  -> OptionalArgumentType(unlift_aux pty)


and unlift_aux_or = function
  | OptionRowEmpty                         -> OptionRowEmpty
  | OptionRowCons(pty, tail)               -> OptionRowCons(unlift_aux pty, unlift_aux_or tail)
  | OptionRowVariable(PolyORFree(orviref)) -> OptionRowVariable(orviref)


let unlift_poly (pty : poly_type_body) : mono_type option =
  try Some(unlift_aux pty) with
  | Exit -> None


let unlift_option_row poptrow =
  try Some(unlift_aux_or poptrow) with
  | Exit -> None


let apply_type_scheme_poly (tyscheme : type_scheme) (ptys : poly_type_body list) =
  failwith "TODO: apply_type_scheme_poly"
