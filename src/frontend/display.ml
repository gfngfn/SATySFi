
open SyntaxBase
open Types
open StaticEnv


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
          | MustBeBound(mbbid)                    -> ()
        end

    | DataType(tys, tyid) ->
        tys |> List.iter aux_mono

    | RecordType(row) ->
        aux_mono_row row

    | HorzCommandType(cmdargtys) -> cmdargtys |> List.iter aux_mono_cmd_arg
    | VertCommandType(cmdargtys) -> cmdargtys |> List.iter aux_mono_cmd_arg
    | MathCommandType(cmdargtys) -> cmdargtys |> List.iter aux_mono_cmd_arg

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

    | DataType(ptys, tyid) ->
        ptys |> List.iter aux_poly

    | HorzCommandType(pcmdargtys) -> pcmdargtys |> List.iter aux_poly_cmd_arg
    | VertCommandType(pcmdargtys) -> pcmdargtys |> List.iter aux_poly_cmd_arg
    | MathCommandType(pcmdargtys) -> pcmdargtys |> List.iter aux_poly_cmd_arg

    | CodeType(pty) ->
        aux_poly pty

  and aux_mono_row : mono_row -> unit = function
    | RowCons(_rlabel, ty, row)                         -> aux_mono ty; aux_mono_row row
    | RowVar(UpdatableRow{contents = MonoORLink(row)})  -> aux_mono_row row
    | RowVar(UpdatableRow{contents = MonoORFree(frid)}) -> aux_free_row_id frid
    | RowVar(MustBeBoundRow(mbbrid))                    -> ()
    | RowEmpty                                          -> ()

  and aux_poly_row : poly_row -> unit = function
    | RowCons(_rlabel, pty, prow) ->
        aux_poly pty;
        aux_poly_row prow

    | RowVar(PolyORFree(prv)) ->
        begin
          match prv with
          | UpdatableRow{contents = MonoORLink(row)}  -> aux_mono_row row
          | UpdatableRow{contents = MonoORFree(frid)} -> aux_free_row_id frid
          | MustBeBoundRow(_)                         -> ()
        end

    | RowVar(PolyORBound(brid)) ->
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
  (aux_mono, aux_poly)


let collect_ids_mono (ty : mono_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fid_ht = DisplayMap.make_free_id_hash_set dispmap in
  let frid_ht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bid_ht = DisplayMap.make_bound_id_hash_set dispmap in
  let brid_ht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (aux_mono, _) = collect_ids_scheme fid_ht frid_ht bid_ht brid_ht in
  aux_mono ty;
  let dispmap =
    FreeIDHashTable.fold (fun fid () dispmap ->
      dispmap |> DisplayMap.add_free_id fid
    ) fid_ht dispmap
  in
  let dispmap =
    FreeRowIDHashTable.fold (fun frid labset dispmap ->
      dispmap |> DisplayMap.add_free_row_id frid labset
    ) frid_ht dispmap
  in
  dispmap


let collect_ids_poly (Poly(pty) : poly_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fid_ht = DisplayMap.make_free_id_hash_set dispmap in
  let frid_ht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bid_ht = DisplayMap.make_bound_id_hash_set dispmap in
  let brid_ht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (_, aux_poly) = collect_ids_scheme fid_ht frid_ht bid_ht brid_ht in
  aux_poly pty;
  let dispmap =
    FreeIDHashTable.fold (fun fid () dispmap ->
      dispmap |> DisplayMap.add_free_id fid
    ) fid_ht dispmap
  in
  let dispmap =
    FreeRowIDHashTable.fold (fun frid labset dispmap ->
      dispmap |> DisplayMap.add_free_row_id frid labset
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


let show_base_type = function
  | EnvType     -> "env"  (* -- unused -- *)
  | UnitType    -> "unit"
  | BoolType    -> "bool"
  | IntType     -> "int"
  | FloatType   -> "float"
  | StringType  -> "string"
  | TextRowType -> "inline-text"
  | TextColType -> "block-text"
  | BoxRowType  -> "inline-boxes"
  | BoxColType  -> "block-boxes"
  | ContextType -> "context"
  | PrePathType -> "pre-path"
  | PathType    -> "path"
  | LengthType  -> "length"
  | GraphicsType -> "graphics"
  | ImageType    -> "image"
  | DocumentType -> "document"
  | MathType     -> "math"
  | RegExpType   -> "regexp"
  | TextInfoType -> "text-info"
  | InputPosType -> "input-position"


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


let rec show_type : 'a 'b. (paren_level -> 'a -> string) -> ('b -> string) -> paren_level -> ('a, 'b) typ -> string =
fun tvf rvf plev ty ->
  let rec aux plev (_, tymain) =
    let (plev_required, s) =
      match tymain with
      | FuncType(optrow, tydom, tycod) ->
          let s_opts =
            match show_row tvf rvf optrow with
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

      | HorzCommandType(cmdargtys) ->
          let ss = cmdargtys |> List.map aux_cmd_arg in
          (ProductElement, Printf.sprintf "inline [%s]" (String.concat ", " ss))

      | VertCommandType(cmdargtys) ->
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
          let s = (show_row tvf rvf row) |> Option.value ~default:"" in
          (Single, Printf.sprintf "(|%s|)" s)
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


and show_row : 'a 'b. (paren_level -> 'a -> string) -> ('b -> string) -> ('a, 'b) row -> string option =
fun tvf rvf row ->
  let NormalizedRow(ty_labmap, prv_opt) = TypeConv.normalize_row_general row in
  if LabelMap.cardinal ty_labmap = 0 then
    match prv_opt with
    | None      -> None
    | Some(prv) -> Some(rvf prv)
  else
    let ss =
      ty_labmap |> LabelMap.bindings |> List.map (fun (label, ty) ->
        Printf.sprintf "%s : %s" label (show_type tvf rvf Outmost ty)
      )
    in
    match prv_opt with
    | None      -> Some(Printf.sprintf "%s" (String.concat ", " ss))
    | Some(prv) -> Some(Printf.sprintf "%s | %s" (String.concat ", " ss) (rvf prv))


let rec tvf_mono (dispmap : DisplayMap.t) (plev : paren_level) (tv : mono_type_variable) : string =
  match tv with
  | Updatable(tvuref)  -> tvf_mono_updatable dispmap plev !tvuref
  | MustBeBound(mbbid) -> dispmap |> DisplayMap.find_bound_id (MustBeBoundID.to_bound_id mbbid)


and tvf_mono_updatable (dispmap : DisplayMap.t) (plev : paren_level) (tvu : mono_type_variable_updatable) : string =
  match tvu with
  | MonoFree(fid) -> dispmap |> DisplayMap.find_free_id fid
  | MonoLink(ty)  -> show_type (tvf_mono dispmap) (rvf_mono dispmap) plev ty


and rvf_mono (dispmap : DisplayMap.t) (rv : mono_row_variable) : string =
  match rv with
  | UpdatableRow(rvref) ->
      begin
        match !rvref with
        | MonoORFree(_) ->
            ""

        | MonoORLink(row) ->
            begin
              match show_row (tvf_mono dispmap) (rvf_mono dispmap) row with
              | None    -> ""
              | Some(s) -> Printf.sprintf "?(%s)" s
            end
      end

  | MustBeBoundRow(mbbrid) ->
      dispmap |> DisplayMap.find_bound_row_id (MustBeBoundRowID.to_bound_id mbbrid)


let rec tvf_poly (dispmap : DisplayMap.t) (plev : paren_level) (ptv : poly_type_variable) : string =
  match ptv with
  | PolyFree(tvuref) -> tvf_mono_updatable dispmap plev !tvuref
  | PolyBound(bid)   -> dispmap |> DisplayMap.find_bound_id bid


and rvf_poly (dispmap : DisplayMap.t) (prv : poly_row_variable) : string =
  match prv with
  | PolyORFree(rvref) -> rvf_mono dispmap rvref
  | PolyORBound(brid) -> dispmap |> DisplayMap.find_bound_row_id brid


let show_mono_type (ty : mono_type) =
  let dispmap = DisplayMap.empty |> collect_ids_mono ty in
  show_type (tvf_mono dispmap) (rvf_mono dispmap) Outmost ty


let show_mono_type_double (ty1 : mono_type) (ty2 : mono_type) =
  let dispmap = DisplayMap.empty |> collect_ids_mono ty1 |> collect_ids_mono ty2 in
  let tvf = tvf_mono dispmap in
  let rvf = rvf_mono dispmap in
  let s1 = show_type tvf rvf Outmost ty1 in
  let s2 = show_type tvf rvf Outmost ty2 in
  (s1, s2)


let show_poly_type (Poly(pty) : poly_type) =
  let dispmap = DisplayMap.empty |> collect_ids_poly (Poly(pty)) in
  show_type (tvf_poly dispmap) (rvf_poly dispmap) Outmost pty


let show_macro_parameter_type (macparamty : macro_parameter_type) =
  match macparamty with
  | LateMacroParameter(pty) ->
      show_poly_type pty

  | EarlyMacroParameter(pty) ->
      let s = show_poly_type pty in
      Printf.sprintf "~(%s)" s


let show_macro_type (macty : macro_type) =
  match macty with
  | HorzMacroType(macparamtys) ->
      let ss = macparamtys |> List.map show_macro_parameter_type in
      Printf.sprintf "inline [%s]" (String.concat ", " ss)

  | VertMacroType(macparamtys) ->
      let ss = macparamtys |> List.map show_macro_parameter_type in
      Printf.sprintf "block [%s]" (String.concat ", " ss)
