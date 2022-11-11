
open SyntaxBase
open Types
open TypeError


(* Checks whether `fid` occurs in `ty`. Through the checking,
   the level of each type variable occuring in `ty` will be changed to
   the level `lev` of `fid` if higher than `lev`. *)
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

    | InlineCommandType(cmdargtys)
    | BlockCommandType(cmdargtys)
    | MathCommandType(cmdargtys) ->
        iter_cmd_list cmdargtys

    | CodeType(tysub) ->
        iter tysub

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
          | MonoRowLink(row) ->
              iter_row row

          | MonoRowFree(frid0) ->
              let lev0 = FreeRowID.get_level frid0 in
              if Level.less_than lev lev0 then FreeRowID.set_level frid0 lev; (* Updates the level *)
              false
        end

    | RowVar(MustBeBoundRow(_)) ->
        false
  in
  iter ty


(* Checks whether `frid` occurs in `row`. Through the checking,
   the level of each row variable occuring in `row` will be changed to
   the level `lev` of `frid` if higher than `lev`. *)
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

    | InlineCommandType(cmdargtys)
    | BlockCommandType(cmdargtys)
    | MathCommandType(cmdargtys) ->
        iter_cmd_list cmdargtys

    | CodeType(tysub) ->
        iter tysub

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
          | MonoRowLink(row) ->
              iter_row row

          | MonoRowFree(frid0) ->
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


let rec unify_type ((rng1, tymain1) as ty1 : mono_type) ((rng2, tymain2) as ty2 : mono_type) : (unit, unification_error) result =
  let open ResultMonad in
  match (tymain1, tymain2) with
  | (BaseType(bty1), BaseType(bty2)) ->
      if bty1 = bty2 then
        return ()
      else
        err (TypeContradiction(ty1, ty2))

  | (FuncType(optrow1, tydom1, tycod1), FuncType(optrow2, tydom2, tycod2)) ->
      let* () = unify_row optrow1 optrow2 in
      let* () = unify_type tydom1 tydom2 in
      let* () = unify_type tycod1 tycod2 in
      return ()

  | (InlineCommandType(cmdargtys1), InlineCommandType(cmdargtys2))
  | (BlockCommandType(cmdargtys1), BlockCommandType(cmdargtys2))
  | (MathCommandType(cmdargtys1), MathCommandType(cmdargtys2)) ->
      let* zipped =
        try
          return @@ List.combine cmdargtys1 cmdargtys2
        with
        | Invalid_argument(_) ->
            let len1 = List.length cmdargtys1 in
            let len2 = List.length cmdargtys2 in
            err (CommandArityMismatch(len1, len2))
      in
      zipped |> foldM (fun () (cmdargty1, cmdargty2) ->
        let CommandArgType(ty_labmap1, ty1) = cmdargty1 in
        let CommandArgType(ty_labmap2, ty2) = cmdargty2 in
        let resmap =
          LabelMap.merge (fun label tyopt1 tyopt2 ->
            match (tyopt1, tyopt2) with
            | (Some(ty1), Some(ty2)) -> Some(unify_type ty1 ty2)
            | (_, None) | (None, _)  -> Some(err (CommandOptionalLabelMismatch(label)))
          ) ty_labmap1 ty_labmap2
        in
        let* () =
          LabelMap.fold (fun _label res_elem res ->
            res >>= fun () ->
            res_elem
          ) resmap (return ())
        in
        unify_type ty1 ty2
      ) ()

  | (ProductType(tys1), ProductType(tys2)) ->
      let ue = TypeContradiction(ty1, ty2) in
      unify_list ~error:ue (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

  | (RecordType(row1), RecordType(row2)) ->
      unify_row row1 row2

  | (DataType(tyargs1, tyid1), DataType(tyargs2, tyid2)) ->
      let ue = TypeContradiction(ty1, ty2) in
      if TypeID.equal tyid1 tyid2 then
        unify_list ~error:ue tyargs1 tyargs2
      else
        err ue

  | (ListType(tysub1), ListType(tysub2)) -> unify_type tysub1 tysub2
  | (RefType(tysub1), RefType(tysub2))   -> unify_type tysub1 tysub2
  | (CodeType(tysub1), CodeType(tysub2)) -> unify_type tysub1 tysub2

  | (TypeVariable(Updatable{contents = MonoLink(tylinked1)}), _) -> unify_type tylinked1 ty2
  | (_, TypeVariable(Updatable{contents = MonoLink(tylinked2)})) -> unify_type ty1 tylinked2

  | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)),
      TypeVariable(Updatable({contents = MonoFree(fid2)} as tvref2))) ->
    if FreeID.equal fid1 fid2 then
      return ()
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
      tvref_old := MonoLink(ty_new);
      return ()
    end

  | (TypeVariable(Updatable({contents = MonoFree(fid1)} as tvref1)), _) ->
      let chk = occurs fid1 ty2 in
      if chk then
        err (TypeVariableInclusion(fid1, ty2))
      else begin
        let ty2new = if Range.is_dummy rng1 then (rng2, tymain2) else (rng1, tymain2) in
        tvref1 := MonoLink(ty2new);
        return ()
      end

  | (_, TypeVariable(Updatable({contents = MonoFree(fid2)} as tvref2))) ->
      let chk = occurs fid2 ty1 in
      if chk then
        err (TypeVariableInclusion(fid2, ty1))
      else begin
        let ty1new = if Range.is_dummy rng2 then (rng1, tymain1) else (rng2, tymain1) in
        tvref2 := MonoLink(ty1new);
        return ()
      end

  | (TypeVariable(MustBeBound(mbbid1)), TypeVariable(MustBeBound(mbbid2))) ->
      if MustBeBoundID.equal mbbid1 mbbid2 then
        return ()
      else
        err (TypeContradiction(ty1, ty2))

  | _ ->
      err (TypeContradiction(ty1, ty2))


and unify_list ~error:(ue : unification_error) (tys1 : mono_type list) (tys2 : mono_type list) : (unit, unification_error) result =
  let open ResultMonad in
  let* zipped =
    try
      return @@ List.combine tys1 tys2
    with
    | Invalid_argument(_) ->
        err ue
  in
  zipped |> foldM (fun () (t1, t2) ->
    unify_type t1 t2
  ) ()


and solve_row_disjointness (row : mono_row) (labset : LabelSet.t) : (unit, unification_error) result =
  let open ResultMonad in
  match row with
  | RowCons((_rng, label), _ty, rowsub) ->
      if labset |> LabelSet.mem label then
        err (BreaksRowDisjointness(label))
      else
        solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoRowLink(rowsub)}) ->
      solve_row_disjointness rowsub labset

  | RowVar(UpdatableRow{contents = MonoRowFree(frid0)}) ->
      let labset0 = FreeRowID.get_label_set frid0 in
      FreeRowID.set_label_set frid0 (LabelSet.union labset0 labset);
      return ()

  | RowVar(MustBeBoundRow(mbbrid0)) ->
      let labset0 = BoundRowID.get_label_set (MustBeBoundRowID.to_bound_id mbbrid0) in
      if LabelSet.subset labset labset0 then
        return ()
      else
        err (InsufficientRowVariableConstraint(mbbrid0, labset, labset0))

  | RowEmpty ->
      return ()


and solve_row_membership (rng : Range.t) (label : label) (ty : mono_type) (row : mono_row) : (mono_row, unification_error) result =
  let open ResultMonad in
  match row with
  | RowCons((rng0, label0), ty0, row0) ->
      if String.equal label0 label then
        let* () = unify_type ty0 ty in
        return row0
      else
        let* row0rest = solve_row_membership rng label ty row0 in
        return @@ RowCons((rng0, label0), ty0, row0rest)

  | RowVar(UpdatableRow{contents = MonoRowLink(row0)}) ->
      solve_row_membership rng label ty row0

  | RowVar(UpdatableRow({contents = MonoRowFree(frid0)} as orvuref0)) ->
      let labset0 = FreeRowID.get_label_set frid0 in
      if labset0 |> LabelSet.mem label then
        err (BreaksLabelMembershipByFreeRowVariable(frid0, label, labset0))
      else begin
        let lev0 = FreeRowID.get_level frid0 in
        let frid1 = FreeRowID.fresh lev0 LabelSet.empty in
        let rvref1 = ref (MonoRowFree(frid1)) in
        let row_rest = RowVar(UpdatableRow(rvref1)) in
        let row_new = RowCons((rng, label), ty, row_rest) in
        orvuref0 := MonoRowLink(row_new);
        return row_rest
      end

  | RowVar(MustBeBoundRow(mbbrid0)) ->
      err (BreaksLabelMembershipByBoundRowVariable(mbbrid0, label))

  | RowEmpty ->
      err (BreaksLabelMembershipByEmptyRow(label))


and unify_row (row1 : mono_row) (row2 : mono_row) : (unit, unification_error) result =
  let open ResultMonad in
  match (row1, row2) with
  | (RowVar(UpdatableRow{contents = MonoRowLink(row1sub)}), _) ->
      unify_row row1sub row2

  | (_, RowVar(UpdatableRow{contents = MonoRowLink(row2sub)})) ->
      unify_row row1 row2sub

  | (RowVar(UpdatableRow({contents = MonoRowFree(frid1)} as rvref1)), RowVar(UpdatableRow{contents = MonoRowFree(frid2)})) ->
      if FreeRowID.equal frid1 frid2 then
        return ()
      else begin
        rvref1 := MonoRowLink(row2);
        return ()
      end

  | (RowVar(UpdatableRow({contents = MonoRowFree(frid1)} as rvref1)), _) ->
      if occurs_row frid1 row2 then
        err (RowVariableInclusion(frid1, row2))
      else begin
        let labset1 = FreeRowID.get_label_set frid1 in
        let* () = solve_row_disjointness row2 labset1 in
        rvref1 := MonoRowLink(row2);
        return ()
      end

  | (_, RowVar(UpdatableRow({contents = MonoRowFree(frid2)} as rvref2))) ->
      if occurs_row frid2 row1 then
        err (RowVariableInclusion(frid2, row1))
      else begin
        let labset2 = FreeRowID.get_label_set frid2 in
        let* () = solve_row_disjointness row1 labset2 in
        rvref2 := MonoRowLink(row1);
        return ()
      end

  | (RowVar(MustBeBoundRow(mbbrid1)), RowVar(MustBeBoundRow(mbbrid2))) ->
      if MustBeBoundRowID.equal mbbrid1 mbbrid2 then
        return ()
      else
        err (RowContradiction(row1, row2))

  | (RowVar(MustBeBoundRow(_)), _) | (_, RowVar(MustBeBoundRow(_))) ->
        err (RowContradiction(row1, row2))

  | (RowCons((rng, label), ty1, row1sub), _) ->
      let* row2rest = solve_row_membership rng label ty1 row2 in
      unify_row row1sub row2rest

  | (RowEmpty, RowEmpty) ->
      return ()

  | (RowEmpty, RowCons(_, _, _)) ->
      err (RowContradiction(row1, row2))
