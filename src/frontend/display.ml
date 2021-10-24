open Types


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
  | FreeID  of mono_kind FreeID_.t_
  | BoundID of BoundID.t


module GeneralIDHashTable_ = Hashtbl.Make(
  struct
    type t = general_id

    let equal gid1 gid2 =
      match (gid1, gid2) with
      | (FreeID(tvid1), FreeID(tvid2)) -> FreeID.equal tvid1 tvid2
      | (BoundID(bid1), BoundID(bid2)) -> BoundID.eq bid1 bid2
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


let rec string_of_mono_type_sub (tvf : paren_level -> 'a -> string) ortvf (tyenv : Typeenv.t) (current_ht : int GeneralIDHashTable.t) (plev : paren_level) ((_, tymain) : ('a, 'b) typ) =
  let iter = string_of_mono_type_sub tvf ortvf tyenv current_ht in
  let iter_cmd  = string_of_command_argument_type tvf ortvf tyenv current_ht in
  let iter_args = string_of_type_argument_list tvf ortvf tyenv current_ht in
  let iter_prod = string_of_product tvf ortvf tyenv current_ht in
  let iter_or = string_of_option_row tvf ortvf tyenv current_ht in
    match tymain with

    | TypeVariable(tvi) -> tvf plev tvi

    | BaseType(EnvType)     -> "env"  (* -- unused -- *)
    | BaseType(UnitType)    -> "unit"
    | BaseType(BoolType)    -> "bool"
    | BaseType(IntType)     -> "int"
    | BaseType(FloatType)   -> "float"
    | BaseType(StringType)  -> "string"
    | BaseType(CharType)    -> "char"

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
    | BaseType(InputPosType) -> "input-position"

    | VariantType(tyarglist, tyid) ->
        let s = (iter_args tyarglist) ^ (Typeenv.find_type_name tyenv tyid) in
        begin
          match (tyarglist, plev) with
          | (_ :: _, Single) -> "(" ^ s ^ ")"
          | _                 -> s
        end

    | SynonymType(tyarglist, tyid, tyreal) ->
        let s =
          (iter_args tyarglist) ^ (Typeenv.find_type_name tyenv tyid)
          ^ " (= " ^ (iter Single tyreal) ^ ")"
        in
        begin
          match (tyarglist, plev) with
          | (_ :: _, Single) -> "(" ^ s ^ ")"
          | _                -> s
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

    | ProductType(tylist) ->
        let s = iter_prod tylist in
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


and string_of_option_row tvf ortvf tyenv current_ht = function
  | OptionRowEmpty -> ""

  | OptionRowVariable(orvi) -> ortvf orvi

  | OptionRowCons(ty, tail) ->
      let s = string_of_mono_type_sub tvf ortvf tyenv current_ht DomainSide ty in
      s ^ "?-> " ^ (string_of_option_row tvf ortvf tyenv current_ht tail)


and string_of_command_argument_type tvf ortvf tyenv current_ht cmdargty =
  let iter = string_of_mono_type_sub tvf ortvf tyenv current_ht in
  match cmdargty with
  | MandatoryArgumentType(ty) ->
      iter Outmost ty

  | OptionalArgumentType(ty)  ->
      let strty = iter Outmost ty in
      strty ^ "?"


and string_of_type_argument_list tvf ortvf tyenv current_ht tyarglist =
  let iter = string_of_mono_type_sub tvf ortvf tyenv current_ht in
  let iter_args = string_of_type_argument_list tvf ortvf tyenv current_ht in
    match tyarglist with
    | [] ->
        ""

    | head :: tail ->
        let strhd = iter Single head in
        let strtl = iter_args tail in
        strhd ^ " " ^ strtl


and string_of_product tvf ortvf tyenv current_ht tylist =
  let iter = string_of_mono_type_sub tvf ortvf tyenv current_ht in
  let iter_list = string_of_product tvf ortvf tyenv current_ht in
    match tylist with
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


let rec tvf_mono current_ht tyenv plev tvi =
  let iter = string_of_mono_type_sub (tvf_mono current_ht tyenv) (ortvf_mono current_ht tyenv) tyenv current_ht in
  match !tvi with
  | MonoFree(tvid) ->
      let num = GeneralIDHashTable.intern_number current_ht (FreeID(tvid)) in
      let s = (if FreeID.is_quantifiable tvid then "'" else "'_") ^ (variable_name_of_number num) in
        show_type_variable (iter Outmost) s (FreeID.get_kind tvid)

  | MonoLink(ty) ->
      iter plev ty


and ortvf_mono current_ht tyenv orvi =
  match !orvi with
  | MonoORFree(_)      -> "...?-> "
  | MonoORLink(optrow) -> string_of_option_row (tvf_mono current_ht tyenv) (ortvf_mono current_ht tyenv) tyenv current_ht optrow


let rec tvf_poly current_ht tyenv plev ptvi =
  let iter_poly = string_of_mono_type_sub (tvf_poly current_ht tyenv) (ortvf_poly current_ht tyenv) tyenv current_ht in
  match ptvi with
  | PolyFree(tvref) ->
      tvf_mono current_ht tyenv plev tvref

  | PolyBound(bid) ->
      let num = GeneralIDHashTable.intern_number current_ht (BoundID(bid)) in
      let s = "'#" ^ (variable_name_of_number num) in
        show_type_variable (iter_poly Outmost) s (BoundID.get_kind bid)


and ortvf_poly current_ht tyenv porvi =
  match porvi with
  | PolyORFree(orviref) ->
      ortvf_mono current_ht tyenv orviref


let string_of_mono_type (tyenv : Typeenv.t) (ty : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
      string_of_mono_type_sub (tvf_mono current_ht tyenv) (ortvf_mono current_ht tyenv) tyenv current_ht Outmost ty
  end


let string_of_mono_type_double (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    let strf = string_of_mono_type_sub (tvf_mono current_ht tyenv) (ortvf_mono current_ht tyenv) tyenv current_ht Outmost in
    let strty1 = strf ty1 in
    let strty2 = strf ty2 in
      (strty1, strty2)
  end


let string_of_poly_type (tyenv : Typeenv.t) (Poly(pty) : poly_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    string_of_mono_type_sub (tvf_poly current_ht tyenv) (ortvf_poly current_ht tyenv) tyenv current_ht Outmost pty
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
