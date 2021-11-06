
open SyntaxBase
open Types
open StaticEnv


let show_row (type a) (type b) (s_left : string) (s_right : string) (tyf : (a, b) typ -> string) (rvf : b -> string) (row : (a, b) row) : string =
  let NormalizedRow(ty_labmap, prv_opt) = TypeConv.normalize_row_general row in
  let ss =
    ty_labmap |> LabelMap.bindings |> List.map (fun (label, ty) ->
      Printf.sprintf "%s : %s" label (tyf ty)
    )
  in
  match prv_opt with
  | None      -> Printf.sprintf "%s %s %s" s_left s_right (String.concat ", " ss)
  | Some(prv) -> Printf.sprintf "%s %s | %s %s" s_left s_right (String.concat ", " ss) (rvf prv)


let rec variable_name_of_number (n : int) : string =
  begin
    if n >= 26 then
      variable_name_of_number ((n - n mod 26) / 26 - 1)
    else
      ""
  end ^ (String.make 1 (Char.chr ((Char.code 'a') + n mod 26)))


let show_type_variable (type a) (type b) (f : (a, b) typ -> string) (name : string) =
  name


type general_id =
  | FreeID  of FreeID.t
  | BoundID of BoundID.t


module GeneralIDHashTable_ = Hashtbl.Make(
  struct
    type t = general_id

    let equal gid1 gid2 =
      match (gid1, gid2) with
      | (FreeID(tvid1), FreeID(tvid2)) -> FreeID.equal tvid1 tvid2
      | (BoundID(bid1), BoundID(bid2)) -> BoundID.equal bid1 bid2
      | (_, _)                         -> false

    let hash = Hashtbl.hash
  end)


module GeneralIDHashTable
: sig
    include Hashtbl.S
    val initialize : unit -> unit
    val intern_number : int t -> general_id -> int
  end
= struct
    include GeneralIDHashTable_

    let current_number = ref 0

    let initialize () = ( current_number := 0 )

    (* --
      'new_number' is required to be 0-origin by 'variable_name_of_number'
    -- *)
    let new_number () =
      let res = !current_number in
      begin
        incr current_number;
        res
      end

    let intern_number (current_ht : 'a t) (gid : general_id) =
      match find_opt current_ht gid with
      | Some(num) ->
          num

      | None ->
          let num = new_number () in
          begin
            add current_ht gid num;
            num
          end

  end


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


let rec string_of_mono_type_sub (tvf : paren_level -> 'a -> string) (ortvf : 'b -> string) (current_ht : int GeneralIDHashTable.t) (plev : paren_level) ((_, tymain) : ('a, 'b) typ) =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  let iter_cmd  = string_of_command_argument_type tvf ortvf current_ht in
  let iter_args = string_of_type_argument_list tvf ortvf current_ht in
  let iter_prod = string_of_product tvf ortvf current_ht in
    match tymain with

    | TypeVariable(tvi) -> tvf plev tvi

    | BaseType(bty) -> show_base_type bty

    | DataType(tyargs, tyid) ->
        let name = TypeID.extract_name tyid in
        let s = (iter_args tyargs) ^ name in
        begin
          match (tyargs, plev) with
          | (_ :: _, Single) -> "(" ^ s ^ ")"
          | _                 -> s
        end

    | FuncType(optrow, tydom, tycod) ->
        let stropts = show_row "?(" ")" (iter Outmost) ortvf optrow in
        let strdom = iter DomainSide tydom in
        let strcod = iter Outmost tycod in
        let s = stropts ^ strdom ^ " -> " ^ strcod in
        begin
          match plev with
          | Single | ProductElement | DomainSide -> "(" ^ s ^ ")"
          | _                                    -> s
        end

    | ListType(tycont) ->
        let strcont = iter Single tycont in
        let s = strcont ^ " list" in
        begin
          match plev with
          | Single -> "(" ^ s ^ ")"
          | _      -> s
        end

    | RefType(tycont) ->
        let strcont = iter Single tycont in
        let s = strcont ^ " ref" in
          begin
            match plev with
            | Single -> "(" ^ s ^ ")"
            | _      -> s
          end

    | CodeType(tysub) ->
        let strsub = iter Single tysub in
        "&" ^ strsub

    | ProductType(tys) ->
        let s = iter_prod (TupleList.to_list tys) in
        begin
          match plev with
          | Single | ProductElement -> "(" ^ s ^ ")"
          | _                       -> s
        end

    | RecordType(row) ->
        show_row "(|" "|)" (iter Outmost) ortvf row

    | HorzCommandType(cmdargtys) ->
        let ss = List.map iter_cmd cmdargtys in
        Printf.sprintf "inline [%s]" (String.concat ", " ss)

    | VertCommandType(cmdargtys) ->
        let ss = List.map iter_cmd cmdargtys in
        Printf.sprintf "block [%s]" (String.concat ", " ss)

    | MathCommandType(cmdargtys) ->
        let ss = List.map iter_cmd cmdargtys in
        Printf.sprintf "math [%s]" (String.concat ", " ss)


and string_of_command_argument_type tvf ortvf current_ht cmdargty =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  match cmdargty with
  | CommandArgType(tylabmap, ty) ->
      let ss =
        tylabmap |> LabelMap.bindings |> List.map (fun (label, ty) ->
          Printf.sprintf "?%s %s " label (iter Outmost ty)
        )
      in
      Printf.sprintf "%s%s" (String.concat "" ss) (iter Outmost ty)


and string_of_type_argument_list tvf ortvf current_ht tyarglist =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  let iter_args = string_of_type_argument_list tvf ortvf current_ht in
    match tyarglist with
    | [] ->
        ""

    | head :: tail ->
        let strhd = iter Single head in
        let strtl = iter_args tail in
        strhd ^ " " ^ strtl


and string_of_product tvf ortvf current_ht tys =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  let iter_list = string_of_product tvf ortvf current_ht in
    match tys with
    | []           -> ""
    | head :: tail ->
        let strhead = iter ProductElement head in
        let strtail = iter_list tail in
        strhead ^
        begin
          match tail with
          | [] -> ""
          | _  -> " * " ^ strtail
        end


let rec tvf_mono current_ht plev tv =
  let iter = string_of_mono_type_sub (tvf_mono current_ht) (ortvf_mono current_ht) current_ht in
  match tv with
  | Updatable(tvuref) ->
      begin
        match !tvuref with
        | MonoFree(fid) ->
            let num = GeneralIDHashTable.intern_number current_ht (FreeID(fid)) in
            let s =
              let prefix = if FreeID.get_quantifiability fid then "'" else "'_" in
              prefix ^ (variable_name_of_number num)
            in
            show_type_variable (iter Outmost) s

        | MonoLink(ty) ->
            iter plev ty
      end

  | MustBeBound(mbbid) ->
      MustBeBoundID.show mbbid


and ortvf_mono current_ht rv =
  let ortvf = ortvf_mono current_ht in
  let iter = string_of_mono_type_sub (tvf_mono current_ht) ortvf current_ht in
  match rv with
  | UpdatableRow(rvref) ->
      begin
        match !rvref with
        | MonoORFree(_)   -> ""
        | MonoORLink(row) -> show_row "?(" ")" (iter Outmost) (ortvf_mono current_ht) row
      end

  | MustBeBoundRow(mbbrid) ->
      failwith "TODO: ortvf_mono, MustBeBoundRow"


let rec tvf_poly current_ht plev ptvi =
  let iter_poly = string_of_mono_type_sub (tvf_poly current_ht) (ortvf_poly current_ht) current_ht in
  match ptvi with
  | PolyFree(tvref) ->
      tvf_mono current_ht plev tvref

  | PolyBound(bid) ->
      let num = GeneralIDHashTable.intern_number current_ht (BoundID(bid)) in
      let s = "'#" ^ (variable_name_of_number num) in
      show_type_variable (iter_poly Outmost) s


and ortvf_poly current_ht rv =
  match rv with
  | PolyORFree(rvref) ->
      ortvf_mono current_ht rvref

  | PolyORBound(brid) ->
      failwith "TODO: ortvf_poly"


let string_of_mono_type (ty : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
      string_of_mono_type_sub (tvf_mono current_ht) (ortvf_mono current_ht) current_ht Outmost ty
  end


let string_of_mono_type_double (ty1 : mono_type) (ty2 : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    let strf = string_of_mono_type_sub (tvf_mono current_ht) (ortvf_mono current_ht) current_ht Outmost in
    let strty1 = strf ty1 in
    let strty2 = strf ty2 in
      (strty1, strty2)
  end


let string_of_poly_type (Poly(pty) : poly_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    string_of_mono_type_sub (tvf_poly current_ht) (ortvf_poly current_ht) current_ht Outmost pty
  end
