
open SyntaxBase
open Types
open StaticEnv
open TypeError
open TypecheckUtil


let decode_manual_type (pre : pre) (tyenv : Typeenv.t) (mty : manual_type) : mono_type ok =
  let open ResultMonad in
  let invalid rng tynm ~expect:len_expected ~actual:len_actual =
    err (IllegalNumberOfTypeArguments(rng, tynm, len_expected, len_actual))
  in
  let rec aux ((rng, mtymain) : manual_type) : mono_type ok =
    let* tymain =
      match mtymain with
      | MTypeName(modidents, tyident, mtyargs) ->
          let* tyargs = mtyargs |> mapM aux in
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
                                    | [ ty ] -> return @@ CodeType(ty)
                                    | _      -> invalid rng "code" ~expect:1 ~actual:len_actual
                                  end

                              | "list" ->
                                  begin
                                    match tyargs with
                                    | [ ty ] -> return @@ ListType(ty)
                                    | _      -> invalid rng "list" ~expect:1 ~actual:len_actual
                                  end

                              | "ref" ->
                                  begin
                                    match tyargs with
                                    | [ ty ] -> return @@ RefType(ty)
                                    | _      -> invalid rng "ref" ~expect:1 ~actual:len_actual
                                  end

                              | _ ->
                                  err (UndefinedTypeName(rng_tynm, tynm))
                            end

                        | Some(bt) ->
                            return @@ BaseType(bt)
                      end

                  | Some(tentry) ->
                      begin
                        match TypeConv.apply_type_scheme_mono tentry.type_scheme tyargs with
                        | Some((_, tymain)) ->
                            return tymain

                        | None ->
                            let (bids, _) = tentry.type_scheme in
                            let len_expected = List.length bids in
                            invalid rng tynm ~expect:len_expected ~actual:len_actual
                      end
                end

            | modident0 :: proj ->
                let modchain = (modident0, proj) in
                let* mentry = find_module_chain tyenv modchain in
                begin
                  match mentry.mod_signature with
                  | ConcFunctor(fsig) ->
                      err (NotAStructureSignature(rng, fsig))
                        (* TODO (error): give a better code range to this error *)

                  | ConcStructure(ssig) ->
                      begin
                        match ssig |> StructSig.find_type tynm with
                        | None ->
                            err (UndefinedTypeName(rng_tynm, tynm))

                        | Some(tentry) ->
                            begin
                              match TypeConv.apply_type_scheme_mono tentry.type_scheme tyargs with
                              | Some((_, tymain)) ->
                                  return tymain

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
                err (UndefinedTypeVariable(rng, typaram))

            | Some(mbbid) ->
                return @@ TypeVariable(MustBeBound(mbbid))
          end

      | MFuncType(mnfields, rowvar_opt, mtydom, mtycod) ->
          let* row = aux_row mnfields rowvar_opt in
          let* ty_dom = aux mtydom in
          let* ty_cod = aux mtycod in
          return @@ FuncType(row, ty_dom, ty_cod)

      | MProductType(mntys) ->
          let* tys = TupleList.mapM aux mntys in
          return @@ ProductType(tys)

      | MRecordType(mnfields, rowvar) ->
          let* row = aux_row mnfields rowvar in
          return @@ RecordType(row)

      | MInlineCommandType(mncmdargtys) ->
          let* cmdargtys = aux_cmd_list mncmdargtys in
          return @@ InlineCommandType(cmdargtys)

      | MBlockCommandType(mncmdargtys) ->
          let* cmdargtys = aux_cmd_list mncmdargtys in
          return @@ BlockCommandType(cmdargtys)

      | MMathCommandType(mncmdargtys) ->
          let* cmdargtys = aux_cmd_list mncmdargtys in
          return @@ MathCommandType(cmdargtys)
    in
    return (rng, tymain)

  and aux_cmd_list (mncmdargtys : manual_command_argument_type list) : (mono_command_argument_type list) ok =
    mncmdargtys |> mapM (fun mncmdargty ->
      let MArgType(mnfields, mnty) = mncmdargty in
      let* tylabmap =
        mnfields |> foldM (fun tylabmap (rlabel, mnty) ->
          let (rng, label) = rlabel in
          if tylabmap |> LabelMap.mem label then
            err (LabelUsedMoreThanOnce(rng, label))
          else
            let* ty = aux mnty in
            return (tylabmap |> LabelMap.add label ty)
        ) LabelMap.empty
      in
      let* ty = aux mnty in
      return @@ CommandArgType(tylabmap, ty)
    )

  and aux_row (mnfields : (label ranged * manual_type) list) (rowvar_opt : (row_variable_name ranged) option) : mono_row ok =
    let* row_end =
      match rowvar_opt with
      | None ->
          return RowEmpty

      | Some((rng, rowparam)) ->
          begin
            match pre.row_parameters |> RowParameterMap.find_opt rowparam with
            | None ->
                err (UndefinedRowVariable(rng, rowparam))

            | Some(mbbrid) ->
                return @@ RowVar(MustBeBoundRow(mbbrid))
          end
    in
    let* (_, row) =
      mnfields |> foldM (fun (labset, row) (rlabel, mnty) ->
        let (rng, label) = rlabel in
        if labset |> LabelSet.mem label then
          err (LabelUsedMoreThanOnce(rng, label))
        else
          let* ty = aux mnty in
          let row = RowCons(rlabel, ty, row) in
          return (labset |> LabelSet.add label, row)
      ) (LabelSet.empty, row_end)
    in
    return row
  in
  aux mty


let decode_manual_base_kind (mnbkd : manual_base_kind) : base_kind ok =
  let open ResultMonad in
  let MKindName((rng, kdnm)) = mnbkd in
  match kdnm with
  | "o" -> return TypeKind
  | _   -> err (UndefinedKindName(rng, kdnm))


let decode_manual_kind (_pre : pre) (_tyenv : Typeenv.t) (mnkd : manual_kind) : kind ok =
  let open ResultMonad in
  let MKind(mnbkds_dom, mnbkd_cod) = mnkd in
  let* kds_dom = mnbkds_dom |> mapM decode_manual_base_kind in
  let* TypeKind = decode_manual_base_kind mnbkd_cod in
  return @@ Kind(kds_dom)


let decode_manual_macro_parameter_type (pre : pre) (tyenv : Typeenv.t) (mmacparamty : manual_macro_parameter_type) : mono_macro_parameter_type ok =
  let open ResultMonad in
  match mmacparamty with
  | MLateMacroParameter(mty) ->
      let* ty = decode_manual_type pre tyenv mty in
      return @@ LateMacroParameter(ty)

  | MEarlyMacroParameter(mty) ->
      let* ty = decode_manual_type pre tyenv mty in
      return @@ EarlyMacroParameter(ty)


let decode_manual_macro_type (pre : pre) (tyenv : Typeenv.t) (mmacty : manual_macro_type) : mono_macro_type ok =
  let open ResultMonad in
  match mmacty with
  | MInlineMacroType(mmacparamtys) ->
      let* macparamtys = mmacparamtys |> mapM (decode_manual_macro_parameter_type pre tyenv) in
      return @@ InlineMacroType(macparamtys)

  | MBlockMacroType(mmacparamtys) ->
      let* macparamtys = mmacparamtys |> mapM (decode_manual_macro_parameter_type pre tyenv) in
      return @@ BlockMacroType(macparamtys)
