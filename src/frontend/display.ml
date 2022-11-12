
open SyntaxBase
open Types


let collect_ids_scheme (fid_ht : unit FreeIDHashTable.t) (frid_ht : LabelSet.t FreeRowIDHashTable.t) (bid_ht : unit BoundIDHashTable.t) (brid_ht : LabelSet.t BoundRowIDHashTable.t) =
  let aux_free_id (fid : FreeID.t) =
    if FreeIDHashTable.mem fid_ht fid then () else
      FreeIDHashTable.add fid_ht fid ()
  in
  let aux_free_row_id (frid : FreeRowID.t) =
    if FreeRowIDHashTable.mem frid_ht frid then () else
      let labset = FreeRowID.get_label_set frid in
      FreeRowIDHashTable.add frid_ht frid labset
  in
  let aux_bound_id (bid : BoundID.t) =
    if BoundIDHashTable.mem bid_ht bid then
      ()
    else
      BoundIDHashTable.add bid_ht bid ()
  in
  let aux_bound_row_id (brid : BoundRowID.t) =
    if BoundRowIDHashTable.mem brid_ht brid then
      ()
    else
      let labset = BoundRowID.get_label_set brid in
      BoundRowIDHashTable.add brid_ht brid labset
  in
  let rec aux_mono ((_, tymain) : mono_type) : unit =
    match tymain with
    | BaseType(_) ->
        ()

    | FuncType(optrow, tydom, tycod) ->
        aux_mono_row optrow;
        aux_mono tydom;
        aux_mono tycod

    | ListType(ty) -> aux_mono ty
    | RefType(ty)  -> aux_mono ty

    | ProductType(tys) ->
        tys |> TupleList.to_list |> List.iter aux_mono

    | TypeVariable(tv) ->
        begin
          match tv with
          | Updatable({contents = MonoLink(ty)})  -> aux_mono ty
          | Updatable({contents = MonoFree(fid)}) -> aux_free_id fid
          | MustBeBound(_mbbid)                   -> ()
        end

    | DataType(tys, _tyid) ->
        tys |> List.iter aux_mono

    | RecordType(row) ->
        aux_mono_row row

    | InlineCommandType(cmdargtys)
    | BlockCommandType(cmdargtys)
    | MathCommandType(cmdargtys) ->
        cmdargtys |> List.iter aux_mono_cmd_arg

    | CodeType(ty) ->
        aux_mono ty

  and aux_poly ((_, ptymain) : poly_type_body) : unit =
    match ptymain with
    | BaseType(_) ->
        ()

    | TypeVariable(ptv) ->
        begin
          match ptv with
          | PolyFree({contents = MonoLink(ty)})  -> aux_mono ty
          | PolyFree({contents = MonoFree(fid)}) -> aux_free_id fid
          | PolyBound(bid)                       -> aux_bound_id bid
        end

    | FuncType(poptrow, ptydom, ptycod) ->
        aux_poly_row poptrow;
        aux_poly ptydom;
        aux_poly ptycod

    | ListType(pty) -> aux_poly pty
    | RefType(pty)  -> aux_poly pty

    | ProductType(ptys) ->
        ptys |> TupleList.to_list |> List.iter aux_poly

    | RecordType(prow) ->
        aux_poly_row prow

    | DataType(ptys, _tyid) ->
        ptys |> List.iter aux_poly

    | InlineCommandType(pcmdargtys)
    | BlockCommandType(pcmdargtys)
    | MathCommandType(pcmdargtys) ->
        pcmdargtys |> List.iter aux_poly_cmd_arg

    | CodeType(pty) ->
        aux_poly pty

  and aux_mono_row : mono_row -> unit = function
    | RowCons(_rlabel, ty, row)                          -> aux_mono ty; aux_mono_row row
    | RowVar(UpdatableRow{contents = MonoRowLink(row)})  -> aux_mono_row row
    | RowVar(UpdatableRow{contents = MonoRowFree(frid)}) -> aux_free_row_id frid
    | RowVar(MustBeBoundRow(_mbbrid))                    -> ()
    | RowEmpty                                           -> ()

  and aux_poly_row : poly_row -> unit = function
    | RowCons(_rlabel, pty, prow) ->
        aux_poly pty;
        aux_poly_row prow

    | RowVar(PolyRowFree(prv)) ->
        begin
          match prv with
          | UpdatableRow{contents = MonoRowLink(row)}  -> aux_mono_row row
          | UpdatableRow{contents = MonoRowFree(frid)} -> aux_free_row_id frid
          | MustBeBoundRow(_)                          -> ()
        end

    | RowVar(PolyRowBound(brid)) ->
        aux_bound_row_id brid

    | RowEmpty ->
        ()

  and aux_mono_cmd_arg (CommandArgType(labmap, ty) : mono_command_argument_type) : unit =
    aux_mono_label_map labmap;
    aux_mono ty

  and aux_poly_cmd_arg (CommandArgType(plabmap, pty) : poly_command_argument_type) : unit =
    aux_poly_label_map plabmap;
    aux_poly pty

  and aux_mono_label_map (labmap : mono_type LabelMap.t) : unit =
    LabelMap.iter (fun _ ty -> aux_mono ty) labmap

  and aux_poly_label_map (plabmap : poly_type_body LabelMap.t) : unit =
    LabelMap.iter (fun _ pty -> aux_poly pty) plabmap

  in
  (aux_mono, aux_mono_row, aux_poly, aux_poly_row)


let collect_from_hash_tables (fid_ht : unit FreeIDHashTable.t) (frid_ht : LabelSet.t FreeRowIDHashTable.t) (bid_ht : unit BoundIDHashTable.t) (brid_ht : LabelSet.t BoundRowIDHashTable.t) (dispmap : DisplayMap.t) : DisplayMap.t =
  let dispmap =
    FreeIDHashTable.fold (fun fid () dispmap ->
      let (dispmap, _) = dispmap |> DisplayMap.add_free_id fid in
      dispmap
    ) fid_ht dispmap
  in
  let dispmap =
    FreeRowIDHashTable.fold (fun frid labset dispmap ->
      let (dispmap, _) = dispmap |> DisplayMap.add_free_row_id frid labset in
      dispmap
    ) frid_ht dispmap
  in
  let dispmap =
    BoundIDHashTable.fold (fun bid () dispmap ->
      dispmap |> DisplayMap.add_bound_id bid
    ) bid_ht dispmap
  in
  let dispmap =
    BoundRowIDHashTable.fold (fun brid labset dispmap ->
      dispmap |> DisplayMap.add_bound_row_id brid labset
    ) brid_ht dispmap
  in
  dispmap


let collect_ids_mono (ty : mono_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fid_ht = DisplayMap.make_free_id_hash_set dispmap in
  let frid_ht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bid_ht = DisplayMap.make_bound_id_hash_set dispmap in
  let brid_ht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (aux_mono, _, _, _) = collect_ids_scheme fid_ht frid_ht bid_ht brid_ht in
  aux_mono ty;
  dispmap |> collect_from_hash_tables fid_ht frid_ht bid_ht brid_ht


let collect_ids_mono_row (row : mono_row) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fid_ht = DisplayMap.make_free_id_hash_set dispmap in
  let frid_ht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bid_ht = DisplayMap.make_bound_id_hash_set dispmap in
  let brid_ht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (_, aux_mono_row, _, _) = collect_ids_scheme fid_ht frid_ht bid_ht brid_ht in
  aux_mono_row row;
  dispmap |> collect_from_hash_tables fid_ht frid_ht bid_ht brid_ht


let collect_ids_poly (Poly(pty) : poly_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fid_ht = DisplayMap.make_free_id_hash_set dispmap in
  let frid_ht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bid_ht = DisplayMap.make_bound_id_hash_set dispmap in
  let brid_ht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (_, _, aux_poly, _) = collect_ids_scheme fid_ht frid_ht bid_ht brid_ht in
  aux_poly pty;
  dispmap |> collect_from_hash_tables fid_ht frid_ht bid_ht brid_ht


let show_base_type = function
  | UnitType        -> "unit"
  | BoolType        -> "bool"
  | IntType         -> "int"
  | FloatType       -> "float"
  | LengthType      -> "length"
  | StringType      -> "string"
  | InlineTextType  -> "inline-text"
  | BlockTextType   -> "block-text"
  | MathTextType    -> "math-text"
  | InlineBoxesType -> "inline-boxes"
  | BlockBoxesType  -> "block-boxes"
  | MathBoxesType   -> "math-boxes"
  | ContextType     -> "context"
  | PrePathType     -> "pre-path"
  | PathType        -> "path"
  | GraphicsType    -> "graphics"
  | ImageType       -> "image"
  | FontType        -> "font"
  | DocumentType    -> "document"
  | RegExpType      -> "regexp"
  | TextInfoType    -> "text-info"
  | InputPosType    -> "input-position"


type paren_level =
  | Outmost
  | DomainSide
  | ProductElement
  | Single


let paren_level_number = function
  | Outmost        -> 4
  | DomainSide     -> 3
  | ProductElement -> 2
  | Single         -> 1


let ( <=@ ) plev1 plev2 =
  paren_level_number plev1 <= paren_level_number plev2


let show_type : 'a 'b. (('a, 'b) row -> string option) -> (paren_level -> 'a -> string) -> paren_level -> ('a, 'b) typ -> string =
fun show_row tvf plev ty ->
  let rec aux plev (_, tymain) =
    let (plev_required, s) =
      match tymain with
      | FuncType(optrow, tydom, tycod) ->
          let s_opts =
            match show_row optrow with
            | None    -> ""
            | Some(s) -> Printf.sprintf "?(%s) " s
          in
          let s_dom = aux DomainSide tydom in
          let s_cod = aux Outmost tycod in
          let s = Printf.sprintf "%s%s -> %s" s_opts s_dom s_cod in
          (Outmost, s)

      | ProductType(tys) ->
          let s = tys |> TupleList.to_list |> List.map (aux ProductElement) |> String.concat " * " in
          (DomainSide, s)

      | ListType(ty) ->
          (ProductElement, Printf.sprintf "list %s" (aux Single ty))

      | RefType(ty) ->
          (ProductElement, Printf.sprintf "ref %s" (aux Single ty))

      | CodeType(ty) ->
          (ProductElement, Printf.sprintf "code %s" (aux Single ty))

      | DataType((_ :: _) as tys, tyid) ->
          let name = TypeID.extract_name tyid in
          let s = Printf.sprintf "%s %s" name (tys |> List.map (aux Single) |> String.concat " ") in
          (ProductElement, s)

      | DataType([], tyid) ->
          (Single, TypeID.extract_name tyid)

      | InlineCommandType(cmdargtys) ->
          let ss = cmdargtys |> List.map aux_cmd_arg in
          (ProductElement, Printf.sprintf "inline [%s]" (String.concat ", " ss))

      | BlockCommandType(cmdargtys) ->
          let ss = cmdargtys |> List.map aux_cmd_arg in
          (ProductElement, Printf.sprintf "block [%s]" (String.concat ", " ss))

      | MathCommandType(cmdargtys) ->
          let ss = cmdargtys |> List.map aux_cmd_arg in
          (ProductElement, Printf.sprintf "math [%s]" (String.concat ", " ss))

      | TypeVariable(tv) ->
          (Single, tvf plev tv)

      | BaseType(bty) ->
          (Single, show_base_type bty)

      | RecordType(row) ->
          let s = (show_row row) |> Option.value ~default:"" in
          (Single, Printf.sprintf "(| %s |)" s)
    in
    if plev_required <=@ plev then
      s
    else
      Printf.sprintf "(%s)" s

  and aux_cmd_arg (CommandArgType(tylabmap, ty0)) =
    let s_opts =
      if LabelMap.cardinal tylabmap = 0 then
        ""
      else
        let ss =
          tylabmap |> LabelMap.bindings |> List.map (fun (label, ty) ->
            Printf.sprintf "%s : %s" label (aux Outmost ty)
          )
        in
        Printf.sprintf "?(%s) " (String.concat ", " ss)
    in
    Printf.sprintf "%s%s" s_opts (aux ProductElement ty0)

  in
  aux plev ty


let rec tvf_mono (dispmap : DisplayMap.t) (plev : paren_level) (tv : mono_type_variable) : string =
  match tv with
  | Updatable(tvuref)  -> tvf_mono_updatable dispmap plev !tvuref
  | MustBeBound(mbbid) -> dispmap |> DisplayMap.find_bound_id (MustBeBoundID.to_bound_id mbbid)


and tvf_mono_updatable (dispmap : DisplayMap.t) (plev : paren_level) (tvu : mono_type_variable_updatable) : string =
  match tvu with
  | MonoFree(fid) -> dispmap |> DisplayMap.find_free_id fid
  | MonoLink(ty)  -> show_type (show_mono_row_by_map dispmap) (tvf_mono dispmap) plev ty


and rvf_mono (dispmap : DisplayMap.t) (rv : mono_row_variable) : string =
  match rv with
  | UpdatableRow(rvref) ->
      begin
        match !rvref with
        | MonoRowFree(frid) ->
            dispmap |> DisplayMap.find_free_row_id frid

        | MonoRowLink(row) ->
            begin
              match show_mono_row_by_map dispmap row with
              | None    -> ""
              | Some(s) -> Printf.sprintf "?(%s)" s
            end
      end

  | MustBeBoundRow(mbbrid) ->
      dispmap |> DisplayMap.find_bound_row_id (MustBeBoundRowID.to_bound_id mbbrid)


and show_mono_row_by_map (dispmap : DisplayMap.t) (row : mono_row) : string option =
  let NormalizedRow(ty_labmap, nrv_opt) = TypeConv.normalize_mono_row row in
  if LabelMap.cardinal ty_labmap = 0 then
    match nrv_opt with
    | None ->
        None

    | Some(NormFreeRow(frid)) ->
        Some(dispmap |> DisplayMap.find_free_row_id frid)

    | Some(NormMustBeBoundRow(mbbrid)) ->
        Some(dispmap |> DisplayMap.find_bound_row_id (MustBeBoundRowID.to_bound_id mbbrid))
  else
    let ss =
      ty_labmap |> LabelMap.bindings |> List.map (fun (label, ty) ->
        let s_ty = show_type (show_mono_row_by_map dispmap) (tvf_mono dispmap) Outmost ty in
        Printf.sprintf "%s : %s" label s_ty
      )
    in
    match nrv_opt with
    | None ->
        Some(Printf.sprintf "%s" (String.concat ", " ss))

    | Some(NormFreeRow(frid)) ->
        let s_frid = dispmap |> DisplayMap.find_free_row_id frid in
        Some(Printf.sprintf "%s | %s" (String.concat ", " ss) s_frid)

    | Some(NormMustBeBoundRow(mbbrid)) ->
        let s_mbbrid = dispmap |> DisplayMap.find_bound_row_id (MustBeBoundRowID.to_bound_id mbbrid) in
        Some(Printf.sprintf "%s | %s" (String.concat ", " ss) s_mbbrid)


let tvf_poly (dispmap : DisplayMap.t) (plev : paren_level) (ptv : poly_type_variable) : string =
  match ptv with
  | PolyFree(tvuref) -> tvf_mono_updatable dispmap plev !tvuref
  | PolyBound(bid)   -> dispmap |> DisplayMap.find_bound_id bid


let rvf_poly (dispmap : DisplayMap.t) (prv : poly_row_variable) : string =
  match prv with
  | PolyRowFree(rvref) -> rvf_mono dispmap rvref
  | PolyRowBound(brid) -> dispmap |> DisplayMap.find_bound_row_id brid


let rec show_poly_row_by_map (dispmap : DisplayMap.t) (prow : poly_row) : string option =
  let NormalizedRow(pty_labmap, prv_opt) = TypeConv.normalize_poly_row prow in
  if LabelMap.cardinal pty_labmap = 0 then
    match prv_opt with
    | None      -> None
    | Some(prv) -> Some(rvf_poly dispmap prv)
  else
    let ss =
      pty_labmap |> LabelMap.bindings |> List.map (fun (label, pty) ->
        let s_ty = show_type (show_poly_row_by_map dispmap) (tvf_poly dispmap) Outmost pty in
        Printf.sprintf "%s : %s" label s_ty
      )
    in
    match prv_opt with
    | None ->
        Some(Printf.sprintf "%s" (String.concat ", " ss))

    | Some(prv) ->
        let s_prv = rvf_poly dispmap prv in
        Some(Printf.sprintf "%s | %s" (String.concat ", " ss) s_prv)


let show_mono_type_by_map (dispmap : DisplayMap.t) (ty : mono_type) =
  show_type (show_mono_row_by_map dispmap) (tvf_mono dispmap) Outmost ty


let show_mono_type (ty : mono_type) =
  let dispmap = DisplayMap.empty |> collect_ids_mono ty in
  show_mono_type_by_map dispmap ty


let show_mono_type_double (ty1 : mono_type) (ty2 : mono_type) =
  let dispmap = DisplayMap.empty |> collect_ids_mono ty1 |> collect_ids_mono ty2 in
  let show_row = show_mono_row_by_map dispmap in
  let tvf = tvf_mono dispmap in
  let s1 = show_type show_row tvf Outmost ty1 in
  let s2 = show_type show_row tvf Outmost ty2 in
  (s1, s2)


let show_poly_type (Poly(pty) : poly_type) =
  let dispmap = DisplayMap.empty |> collect_ids_poly (Poly(pty)) in
  show_type (show_poly_row_by_map dispmap) (tvf_poly dispmap) Outmost pty


let show_poly_macro_parameter_type (macparamty : poly_macro_parameter_type) =
  match macparamty with
  | LateMacroParameter(pty) ->
      show_poly_type (Poly(pty))

  | EarlyMacroParameter(pty) ->
      let s = show_poly_type (Poly(pty)) in
      Printf.sprintf "~(%s)" s


let show_poly_macro_type (macty : poly_macro_type) =
  match macty with
  | InlineMacroType(macparamtys) ->
      let ss = macparamtys |> List.map show_poly_macro_parameter_type in
      Printf.sprintf "inline [%s]" (String.concat ", " ss)

  | BlockMacroType(macparamtys) ->
      let ss = macparamtys |> List.map show_poly_macro_parameter_type in
      Printf.sprintf "block [%s]" (String.concat ", " ss)
