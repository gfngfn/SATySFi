open Types
open StaticEnv


let string_of_record_type (type a) (type b) (f : (a, b) typ -> string) (asc : ((a, b) typ) Assoc.t) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let string_of_kind (type a) (type b) (f : (a, b) typ -> string) (kdstr : (a, b) kind) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    match kdstr with
    | UniversalKind   -> "U"
    | RecordKind(asc) -> "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let rec variable_name_of_number (n : int) =
  ( if n >= 26 then
      variable_name_of_number ((n - n mod 26) / 26 - 1)
    else
      ""
  ) ^ (String.make 1 (Char.chr ((Char.code 'a') + n mod 26)))


let show_type_variable (type a) (type b) (f : (a, b) typ -> string) (name : string) (kd : (a, b) kind) =
  match kd with
  | UniversalKind   -> name
  | RecordKind(asc) -> "(" ^ name ^ " <: " ^ (string_of_kind f kd) ^ ")"


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


type paren_level =
  | Outmost
  | DomainSide
  | ProductElement
  | Single


let rec string_of_mono_type_sub (tvf : paren_level -> 'a -> string) ortvf (current_ht : int GeneralIDHashTable.t) (plev : paren_level) ((_, tymain) : ('a, 'b) typ) =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  let iter_cmd  = string_of_command_argument_type tvf ortvf current_ht in
  let iter_args = string_of_type_argument_list tvf ortvf current_ht in
  let iter_prod = string_of_product tvf ortvf current_ht in
  let iter_or = string_of_option_row tvf ortvf current_ht in
    match tymain with

    | TypeVariable(tvi) -> tvf plev tvi

    | BaseType(EnvType)     -> "env"  (* -- unused -- *)
    | BaseType(UnitType)    -> "unit"
    | BaseType(BoolType)    -> "bool"
    | BaseType(IntType)     -> "int"
    | BaseType(FloatType)   -> "float"
    | BaseType(StringType)  -> "string"

    | BaseType(TextRowType) -> "inline-text"
    | BaseType(TextColType) -> "block-text"
    | BaseType(BoxRowType)  -> "inline-boxes"
    | BaseType(BoxColType)  -> "block-boxes"
    | BaseType(ContextType) -> "context"
    | BaseType(PrePathType) -> "pre-path"
    | BaseType(PathType)    -> "path"
    | BaseType(LengthType)  -> "length"
    | BaseType(GraphicsType) -> "graphics"
    | BaseType(ImageType)    -> "image"
    | BaseType(DocumentType) -> "document"
    | BaseType(MathType)     -> "math"
    | BaseType(RegExpType)   -> "regexp"
    | BaseType(TextInfoType) -> "text-info"

    | DataType(tyargs, tyid) ->
        let name =
          match tyid with
          | TypeID.Variant(vid) -> TypeID.Variant.extract_name vid
          | TypeID.Synonym(sid) -> TypeID.Synonym.extract_name sid
          | TypeID.Opaque(oid)  -> TypeID.Opaque.extract_name oid
        in
        let s = (iter_args tyargs) ^ name in
        begin
          match (tyargs, plev) with
          | (_ :: _, Single) -> "(" ^ s ^ ")"
          | _                 -> s
        end

    | FuncType(optrow, ((_, tydommain) as tydom), tycod) ->
        let stropts = iter_or optrow in
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

    | RecordType(asc) ->
        string_of_record_type (iter Outmost) asc

    | HorzCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] inline-cmd"

    | VertCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] block-cmd"

    | MathCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] math-cmd"


and string_of_option_row tvf ortvf current_ht = function
  | OptionRowEmpty -> ""

  | OptionRowVariable(orvi) -> ortvf orvi

  | OptionRowCons(ty, tail) ->
      let s = string_of_mono_type_sub tvf ortvf current_ht DomainSide ty in
      s ^ "?-> " ^ (string_of_option_row tvf ortvf current_ht tail)


and string_of_command_argument_type tvf ortvf current_ht cmdargty =
  let iter = string_of_mono_type_sub tvf ortvf current_ht in
  match cmdargty with
  | MandatoryArgumentType(ty) ->
      iter Outmost ty

  | OptionalArgumentType(ty)  ->
      let strty = iter Outmost ty in
      strty ^ "?"


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
            let fentry = KindStore.get_free_id fid in
            let num = GeneralIDHashTable.intern_number current_ht (FreeID(fid)) in
            let s =
              let prefix =
                match fentry.KindStore.quantifiability with
                | Quantifiable   -> "'"
                | Unquantifiable -> "'_"
              in
              prefix ^ (variable_name_of_number num)
            in
            show_type_variable (iter Outmost) s (fentry.KindStore.mono_kind)

        | MonoLink(ty) ->
            iter plev ty
      end

  | MustBeBound(mbbid) ->
      MustBeBoundID.show mbbid


and ortvf_mono current_ht (UpdatableRow(orvref)) =
  match !orvref with
  | MonoORFree(_)      -> "...?-> "
  | MonoORLink(optrow) -> string_of_option_row (tvf_mono current_ht) (ortvf_mono current_ht) current_ht optrow


let rec tvf_poly current_ht plev ptvi =
  let iter_poly = string_of_mono_type_sub (tvf_poly current_ht) (ortvf_poly current_ht) current_ht in
  match ptvi with
  | PolyFree(tvref) ->
      tvf_mono current_ht plev tvref

  | PolyBound(bid) ->
      let bentry = KindStore.get_bound_id bid in
      let num = GeneralIDHashTable.intern_number current_ht (BoundID(bid)) in
      let s = "'#" ^ (variable_name_of_number num) in
        show_type_variable (iter_poly Outmost) s bentry.KindStore.poly_kind


and ortvf_poly current_ht porvi =
  match porvi with
  | PolyORFree(orviref) ->
      ortvf_mono current_ht orviref


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


(* -- following are all for debug -- *)

let string_of_utast utast = show_untyped_abstract_tree utast


let escape_letters str =
  let rec aux str index =
    if index <= 0 then "" else
      let head =
        match str.[0] with
        | '\\'  -> "\\\\"
        | '"'   -> "\\\""
        | other -> String.make 1 other
      in
        head ^ (aux (String.sub str 1 (index - 1)) (index - 1))
  in
    aux str (String.length str)


let string_of_ast (ast : abstract_tree) = show_abstract_tree ast
