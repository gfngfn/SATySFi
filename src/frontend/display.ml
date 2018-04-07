module Types = Types_
open Types


let string_of_record_type (f : mono_type -> string) (asc : mono_type Assoc.t) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let string_of_kind (f : mono_type -> string) (kdstr : kind) =
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


let show_type_variable (f : mono_type -> string) (name : string) (kd : kind) =
  match kd with
  | UniversalKind   -> name
  | RecordKind(asc) -> "(" ^ name ^ " <: " ^ (string_of_kind f (normalize_kind kd)) ^ ")"


type general_id = FreeID of FreeID.t | BoundID of BoundID.t


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


let rec string_of_mono_type_sub (tyenv : Typeenv.t) (current_ht : int GeneralIDHashTable.t) ((_, tymain) : mono_type) =
  let iter = string_of_mono_type_sub tyenv current_ht in
  let iter_cmd  = string_of_command_argument_type tyenv current_ht in
  let iter_args = string_of_type_argument_list tyenv current_ht in
  let iter_list = string_of_mono_type_list tyenv current_ht in
    match tymain with

    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | Link(tyl)  ->
            (* -- 'Link(_)' must be eliminated by 'normalize_mono_type' and 'normalize_kind' -- *)
              assert false
(*
              "${" ^ iter tyl ^ "}"  (* TEMPORARY *)
*)

          | Bound(bid) ->
              let num = GeneralIDHashTable.intern_number current_ht (BoundID(bid)) in
              let s = "'#" ^ (variable_name_of_number num) in
                show_type_variable iter s (BoundID.get_kind bid)

          | Free(tvid) ->
              let num = GeneralIDHashTable.intern_number current_ht (FreeID(tvid)) in
              let s = (if FreeID.is_quantifiable tvid then "'" else "'_") ^ (variable_name_of_number num) in
                show_type_variable iter s (FreeID.get_kind tvid)
        end

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
(*
    | BaseType(FontType)    -> "font"
*)
    | BaseType(ContextType) -> "context"
    | BaseType(PrePathType) -> "pre-path"
    | BaseType(PathType)    -> "path"
    | BaseType(LengthType)  -> "length"
    | BaseType(GraphicsType) -> "graphics"
    | BaseType(ImageType)    -> "image"
    | BaseType(DocumentType) -> "document"
    | BaseType(MathType)     -> "math"
    | BaseType(RegExpType)   -> "regexp"

    | VariantType(tyarglist, tyid) -> (iter_args tyarglist) ^ (Typeenv.find_type_name tyenv tyid)

    | SynonymType(tyarglist, tyid, tyreal) -> (iter_args tyarglist) ^ (Typeenv.find_type_name tyenv tyid)
                                             ^ " (= " ^ (iter tyreal) ^ ")"

    | FuncType(tydom, tycod) ->
        let strdom = iter tydom in
        let strcod = iter tycod in
          begin
            match tydom with
            | (_, FuncType(_, _))
            | (_, OptFuncType(_, _))
                -> "(" ^ strdom ^ ")"
            | _ -> strdom
          end ^ " -> " ^ strcod

    | OptFuncType(tydom, tycod) ->
        let strdom = iter tydom in
        let strcod = iter tycod in
          begin
            match tydom with
            | (_, FuncType(_, _))
            | (_, OptFuncType(_, _))
                -> "(" ^ strdom ^ ")"
            | _ -> strdom
          end ^ "? -> " ^ strcod

    | ListType(tycont) ->
        let strcont = iter tycont in
          begin
            match tycont with
            | (_, FuncType(_, _))
            | (_, OptFuncType(_, _))
            | (_, ProductType(_))
                -> "(" ^ strcont ^ ")"
            | _ -> strcont
          end ^ " list"

    | RefType(tycont) ->
        let strcont = iter tycont in
          begin
            match tycont with
            | (_, FuncType(_, _))
            | (_, OptFuncType(_, _))
            | (_, ProductType(_))
                -> "(" ^ strcont ^ ")"
            | _ -> strcont
          end ^ " ref"

    | ProductType(tylist) -> iter_list tylist

    | RecordType(asc) -> string_of_record_type iter asc

    | HorzCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] inline-cmd"

    | VertCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] block-cmd"

    | MathCommandType(cmdargtylist) ->
        let slist = List.map iter_cmd cmdargtylist in
        "[" ^ (String.concat "; " slist) ^ "] math-cmd"


and string_of_command_argument_type tyenv current_ht = function
  | MandatoryArgumentType(ty) -> string_of_mono_type_sub tyenv current_ht ty
  | OptionalArgumentType(ty)  ->
      let strty = string_of_mono_type_sub tyenv current_ht ty in
      begin
        match ty with
        | (_, ProductType(_))
        | (_, FuncType(_))
        | (_, OptFuncType(_))
          -> "(" ^ strty ^  ")?"

        | _ -> strty ^ "?"
      end


and string_of_type_argument_list tyenv current_ht tyarglist =
  let iter = string_of_mono_type_sub tyenv current_ht in
  let iter_args = string_of_type_argument_list tyenv current_ht in
    match tyarglist with
    | []           -> ""
    | head :: tail ->
        let strhd = iter head in
        let strtl = iter_args tail in
        let (_, headmain) = head in
          begin
            match headmain with
            | FuncType(_, _)
            | OptFuncType(_, _)
            | ProductType(_)
           (* | TypeSynonym(_ :: _, _, _) *) (* temporary *)
            | ListType(_)
            | RefType(_)
            | VariantType(_ :: _, _)
                -> "(" ^ strhd ^ ")"
            | _ -> strhd
          end ^ " " ^ strtl


and string_of_mono_type_list tyenv current_ht tylist =
  let iter = string_of_mono_type_sub tyenv current_ht in
  let iter_list = string_of_mono_type_list tyenv current_ht in
    match tylist with
    | []           -> ""
    | head :: tail ->
        let strhead = iter head in
        let strtail = iter_list tail in
        let (_, headmain) = head in
        begin
          match headmain with
          | ProductType(_)
          | FuncType(_, _)
          | OptFuncType(_, _)
              -> "(" ^ strhead ^ ")"
          | _ -> strhead
        end ^
        begin
          match tail with
          | [] -> ""
          | _  -> " * " ^ strtail
        end


let string_of_mono_type (tyenv : Typeenv.t) (ty : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    let tyn = normalize_mono_type ty in
      string_of_mono_type_sub tyenv current_ht tyn
  end


let string_of_mono_type_double (tyenv : Typeenv.t) (ty1 : mono_type) (ty2 : mono_type) =
  begin
    GeneralIDHashTable.initialize ();
    let current_ht = GeneralIDHashTable.create 32 in
    let tyn1 = normalize_mono_type ty1 in
    let tyn2 = normalize_mono_type ty2 in
    let strty1 = string_of_mono_type_sub tyenv current_ht tyn1 in
    let strty2 = string_of_mono_type_sub tyenv current_ht tyn2 in
      (strty1, strty2)
  end


let string_of_poly_type (tyenv : Typeenv.t) (Poly(ty) : poly_type) =
  let tyn = normalize_mono_type ty in
  string_of_mono_type tyenv tyn (* temporary *)


(* -- following are all for debug -- *)
let string_of_utast utast = show_untyped_abstract_tree utast

(*
let rec string_of_utast ((_, utastmain) : untyped_abstract_tree) =
  match utastmain with
  | UTStringEmpty                  -> "{}"
  | UTIntegerConstant(nc)          -> string_of_int nc
  | UTBooleanConstant(bc)          -> string_of_bool bc
  | UTStringConstant(sc)           -> "{" ^ sc ^ "}"
  | UTUnitConstant                 -> "()"
  | UTContentOf(lst, varnm)        -> (List.fold_left (fun mdlnm s -> s ^ mdlnm ^ ".") "" lst) ^ varnm
  | UTConcat(ut1, (_, UTStringEmpty)) -> string_of_utast ut1
  | UTConcat(ut1, ut2)             -> "(" ^ (string_of_utast ut1) ^ " ^ " ^ (string_of_utast ut2) ^ ")"
  | UTApply(ut1, ut2)              -> "(" ^ (string_of_utast ut1) ^ " " ^ (string_of_utast ut2) ^ ")"
  | UTListCons(hd, tl)             -> "(" ^ (string_of_utast hd) ^ " :: " ^ (string_of_utast tl) ^ ")"
  | UTEndOfList                    -> "[]"
  | UTTupleCons(hd, tl)            -> "(" ^ (string_of_utast hd) ^ ", " ^ (string_of_utast tl) ^ ")"
  | UTEndOfTuple                   -> "$"
(*
  | UTBreakAndIndent               -> "break"
*)
  | UTLetRecIn(_, ut)              -> "(let ... in " ^ (string_of_utast ut) ^ ")"
  | UTIfThenElse(ut1, ut2, ut3)    -> "(if " ^ (string_of_utast ut1) ^ " then "
                                        ^ (string_of_utast ut2) ^ " else " ^ (string_of_utast ut3) ^ ")"
  | UTFunction(_)                  -> "(function ...)"
  | UTFinishHeaderFile             -> "finish"
  | UTPatternMatch(ut, pmcons)     -> "(match " ^ (string_of_utast ut) ^ " with" ^ (string_of_pmcons pmcons) ^ ")"
  | UTItemize(itmz)                -> "(itemize " ^ string_of_itemize 0 itmz ^ ")"
(*  | UTDeclareVariantIn() *)
  | UTInputVert(utivlst)           -> "(textV " ^ (String.concat " " (List.map string_of_utiv utivlst)) ^ ")"
  | UTInputHorz(utihlst)           -> "(textH " ^ (String.concat " " (List.map string_of_utih utihlst)) ^ ")"
  | _                              -> "OTHER"
*)

(*
let rec string_of_utiv (_, utivmain) =
  match utivmain with
  | UTInputVertEmbedded(utastcmd, utastlst) ->
      "(embV " ^ (string_of_utast utastcmd) ^ " " ^ (String.concat " " (List.map string_of_utast utastlst)) ^ ")"
  | UTInputVertContent(utast0) ->
      "(embVC " ^ (string_of_utast utast0) ^ ")"

and string_of_utih (_, utihmain) =
  match utihmain with
  | UTInputHorzEmbedded(utastcmd, utastlst) ->
      "(embH " ^ (string_of_utast utastcmd) ^ " " ^ (String.concat " " (List.map string_of_utast utastlst)) ^ ")"
  | UTInputHorzText(s) -> "\"" ^ s ^ "\""
  | UTInputHorzContent(utast0) ->
      "(embHC " ^ (string_of_utast utast0) ^ ")"
  | UTInputHorzEmbeddedMath(utastmath) ->
      "(embHM " ^ (string_of_utast utastmath) ^ ")"

and string_of_itemize dp (UTItem(utast, itmzlst)) =
  "(" ^ (String.make dp '*') ^ " " ^ (string_of_utast utast)
    ^ (List.fold_left (fun x y -> x ^ " " ^ y) "" (List.map (string_of_itemize (dp + 1)) itmzlst)) ^ ")"

and string_of_pmcons pmcons =
  match pmcons with
  | [] -> ""
  | UTPatternBranch(pat, ut) :: tail
      -> " | " ^ (string_of_utpat pat) ^ " -> " ^ (string_of_utast ut) ^ (string_of_pmcons tail)
  | UTPatternBranchWhen(pat, utb, ut) :: tail
      -> " | " ^ (string_of_utpat pat) ^ " when " ^ (string_of_utast utb)
          ^ " -> " ^ (string_of_utast ut) ^ (string_of_pmcons tail)

and string_of_utpat (_, pat) =
  match pat with
  | UTPIntegerConstant(nc)  -> string_of_int nc
  | UTPBooleanConstant(bc)  -> string_of_bool bc
  | UTPStringConstant(ut)   -> string_of_utast ut
  | UTPUnitConstant         -> "()"
  | UTPListCons(hd, tl)     -> (string_of_utpat hd) ^ " :: " ^ (string_of_utpat tl)
  | UTPEndOfList            ->  "[]"
  | UTPTupleCons(hd, tl)    -> "(" ^ (string_of_utpat hd) ^ ", " ^ (string_of_utpat tl) ^ ")"
  | UTPEndOfTuple           -> "$"
  | UTPWildCard             -> "_"
  | UTPVariable(varnm)      -> varnm
  | UTPAsVariable(varnm, p) -> "(" ^ (string_of_utpat p) ^ " as " ^ varnm ^ ")"
  | UTPConstructor(cnm,p)   -> "(" ^ cnm ^ " " ^ (string_of_utpat p) ^ ")"
*)

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

(*
let rec string_of_ast (ast : abstract_tree) =
  match ast with
  | LambdaAbstract(x, m)         -> "(" ^ (EvalVarID.show_direct x) ^ " -> " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(" ^ (EvalVarID.show_direct x) ^ " *-> " ^ (string_of_ast m) ^ ")"
  | ContentOf(rng, x)            -> EvalVarID.show_direct x
  | Apply(m, n)                  -> "(" ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t)                 -> "(" ^ (string_of_ast s) ^ " ^ " ^ (string_of_ast t) ^ ")"
  | StringEmpty                  -> "\"\""
  | StringConstant(sc)           -> "\"" ^ (escape_letters sc) ^ "\""
  | IntegerConstant(nc)          -> string_of_int nc
  | FloatConstant(nc)            -> string_of_float nc
  | BooleanConstant(bc)          -> string_of_bool bc
  | IfThenElse(b, t, f)          ->
      "(if " ^ (string_of_ast b) ^ " then " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
(*
  | ApplyClassAndID(c, i, m)     ->
      "(apply-class-and-id " ^ (string_of_ast c) ^ " " ^ (string_of_ast i) ^ " " ^ (string_of_ast m) ^ ")"
*)
  | Dereference(a)               -> "(!" ^ (string_of_ast a) ^ ")"
(*
  | ReferenceFinal(a)            -> "(!!" ^ (string_of_ast a) ^ ")"
*)
  | Overwrite(x, n)              -> "(" ^ (EvalVarID.show_direct x) ^ " <- " ^ (string_of_ast n) ^ ")"
  | Location(loc)                -> "<mutable>"
  | UnitConstant                 -> "()"
  | LetMutableIn(x, d, f)        -> "(let-mutable " ^ (EvalVarID.show_direct x) ^ " <- " ^ (string_of_ast d) ^ " in " ^ (string_of_ast f) ^ ")"
  | ListCons(a, cons)            -> "(" ^ (string_of_ast a) ^ " :: " ^ (string_of_ast cons) ^ ")"
  | EndOfList                    -> "[]"
  | TupleCons(a, cons)           -> "(" ^ (string_of_ast a) ^ ", " ^ (string_of_ast cons) ^ ")"
  | EndOfTuple                   -> "end-of-tuple"
(*
  | BreakAndIndent               -> "break"
*)
  | FinishHeaderFile             -> "finish-header-file"
  | EvaluatedEnvironment(_)      -> "evaluated-environment"
(*
  | DeeperIndent(m)              -> "(deeper " ^ (string_of_ast m) ^ ")"
*)
  | Constructor(c, m)            -> "(constructor " ^ c ^ " " ^ (string_of_ast m) ^ ")"
  | PatternMatch(_, _)           -> "(match ...)"
  | LetIn(_, m)                  -> "(let ... in " ^ (string_of_ast m) ^ ")"
  | WhileDo(m, n)                -> "(while " ^ (string_of_ast m) ^ " do " ^ (string_of_ast n) ^ ")"
(*
  | DeclareGlobalHash(m, n)      -> "(declare-global-hash " ^ (string_of_ast m) ^ " <<- " ^ (string_of_ast n) ^ ")"
  | OverwriteGlobalHash(m, n)    -> "(overwrite-global-hash " ^ (string_of_ast m) ^ " <<- " ^ (string_of_ast n) ^ ")"
*)
  | Module(m, n)                 -> "(module " ^ (string_of_ast m) ^ " end in " ^ (string_of_ast n) ^ ")"
  | Sequential(m, n)             -> "(sequential " ^ (string_of_ast m) ^ " ; " ^ (string_of_ast n) ^ ")"
  | PrimitiveSame(m, n)          -> "(same " ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | PrimitiveStringSub(m, n, o)  ->
      "(string-sub " ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ " " ^ (string_of_ast o) ^ ")"
  | PrimitiveStringLength(m)     -> "(string-length " ^ (string_of_ast m) ^ ")"
  | PrimitiveArabic(m)           -> "(arabic " ^ (string_of_ast m) ^ ")"
  | Record(asc)                  -> "(| ... |)"
  | AccessField(r, f)            -> (string_of_ast r) ^ "#" ^ f
  | InputHorz(_)                 -> "(input-horz ...)"
  | InputVert(_)                 -> "(input-vert ...)"
  | Horz(_)                      -> "(horz ...)"
  | Vert(_)                      -> "(vert ...)"
  | HorzConcat(ast1, ast2)       -> "(horz-concat " ^ (string_of_ast ast1) ^ " " ^ (string_of_ast ast2) ^ ")"
  | VertConcat(ast1, ast2)       -> "(vert-concat " ^ (string_of_ast ast1) ^ " " ^ (string_of_ast ast2) ^ ")"
  | HorzLex(ast1, ast2)          -> "(horz-lex " ^ (string_of_ast ast1) ^ " " ^ (string_of_ast ast2) ^ ")"
  | VertLex(ast1, ast2)          -> "(vert-lex " ^ (string_of_ast ast1) ^ " " ^ (string_of_ast ast2) ^ ")"
  | LambdaHorz(_, ast1)          -> "(lambda-horz _. " ^ (string_of_ast ast1) ^ ")"
  | LambdaVert(_, ast1)          -> "(lambda-vert _. " ^ (string_of_ast ast1) ^ ")"
  | LambdaHorzWithEnvironment(_, ast1, _) -> "(lambda-horz! _. " ^ (string_of_ast ast1) ^ ")"
  | LambdaVertWithEnvironment(_, ast1, _) -> "(lambda-vert! _. " ^ (string_of_ast ast1) ^ ")"
  | Context(_)                   -> "(context)"
  | FontDesignation(_)           -> "(font-designation)"
  | _                            -> "OTHER"
*)
