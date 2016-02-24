open Types

(* code_range -> string *)
let describe_position (sttln, sttpos, endln, endpos) =
  if sttln = endln then
    "line " ^ (string_of_int sttln) ^ ", characters " ^ (string_of_int sttpos)
      ^ "-" ^ (string_of_int endpos)
  else
    "line " ^ (string_of_int sttln) ^ ", character " ^ (string_of_int sttpos)
      ^ " to line " ^ (string_of_int endln) ^ ", character " ^ (string_of_int endpos)


(* code_range -> string list -> 'a *)
let report_error_with_range rng msglst =
  let rec lines_of_list msglst =
    match msglst with
    | []             -> ""
    | msghd :: []    -> "    " ^ msghd
    | msghd :: msgtl -> "    " ^ msghd ^ "\n" ^ (lines_of_list msgtl)
  in
    raise (TypeCheckError( "at " ^ (describe_position rng) ^ ":\n" ^ (lines_of_list msglst)))


let meta_max    : type_variable_id ref = ref 0
let unbound_max : type_variable_id ref = ref 0
let unbound_type_valiable_name_list : (type_variable_id * string ) list ref = ref []

(* int -> string *)
let rec variable_name_of_int n =
  ( if n >= 26 then
      variable_name_of_int ((n - n mod 26) / 26 - 1)
    else
      ""
  ) ^ (String.make 1 (Char.chr ((Char.code 'a') + n mod 26)))

(* unit -> string *)
let new_meta_type_variable_name () =
  let res = variable_name_of_int (!meta_max) in
    begin meta_max := !meta_max + 1 ; res end

(* (type_variable_id * string) list -> type_variable_id -> string *)
let rec find_meta_type_variable lst tvid =
  match lst with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k = tvid then v else find_meta_type_variable tail tvid

(* type_variable_id -> string *)
let new_unbound_type_variable_name tvid =
  let res = variable_name_of_int (!unbound_max) in
    begin
      unbound_max := !unbound_max + 1 ;
      unbound_type_valiable_name_list := (tvid, res) :: (!unbound_type_valiable_name_list) ;
      res
    end

(* type_variable_id -> string *)
let find_unbound_type_variable tvid =
  find_meta_type_variable (!unbound_type_valiable_name_list) tvid

(* type_struct -> string *)
let rec string_of_type_struct tystr =
  begin
    meta_max := 0 ;
    unbound_max := 0 ;
    unbound_type_valiable_name_list := [] ;
    string_of_type_struct_sub tystr []
  end

and string_of_type_struct_double tystr1 tystr2 =
  begin
    meta_max := 0 ;
    unbound_max := 0 ;
    unbound_type_valiable_name_list := [] ;
    let strty1 = string_of_type_struct_sub tystr1 [] in
    let strty2 = string_of_type_struct_sub tystr2 [] in
      (strty1, strty2)
  end

(* type_struct -> (type_variable_id * string) list -> string *)
and string_of_type_struct_sub tystr lst =
  match tystr with
  | StringType(_)                      -> "string"
  | IntType(_)                         -> "int"
  | BoolType(_)                        -> "bool"
  | UnitType(_)                        -> "unit"
  | VariantType(_, tyarglist, varntnm) -> (string_of_type_argument_list tyarglist) ^ varntnm
  | TypeSynonym(_, tyarglist, tynm, tycont) -> tynm ^ " (= " ^ (string_of_type_struct_sub tycont lst) ^ ")"

  | FuncType(_, tydom, tycod) ->
      let strdom = string_of_type_struct_sub tydom lst in
      let strcod = string_of_type_struct_sub tycod lst in
        begin
          match tydom with
          | FuncType(_, _, _) -> "(" ^ strdom ^ ")"
          | _                 -> strdom
        end ^ " -> " ^ strcod

  | ListType(_, tycont) ->
      let strcont = string_of_type_struct_sub tycont lst in
        begin
          match tycont with
          | FuncType(_, _, _) -> "(" ^ strcont ^ ")"
          | ProductType(_, _) -> "(" ^ strcont ^ ")"
          | _                 -> strcont
        end ^ " list"

  | RefType(_, tycont) ->
      let strcont = string_of_type_struct_sub tycont lst in
        begin
          match tycont with
          | FuncType(_, _, _) -> "(" ^ strcont ^ ")"
          | ProductType(_, _) -> "(" ^ strcont ^ ")"
          | _                 -> strcont
        end ^ " ref"

  | ProductType(_, tylist) -> string_of_type_struct_list tylist lst

  | TypeVariable(_, tvid) ->
      begin
        try "'" ^ (find_meta_type_variable lst tvid) with
        | Not_found ->
            "'" ^
              begin
                try find_unbound_type_variable tvid with
                | Not_found -> new_unbound_type_variable_name tvid
              end
      end

  | ForallType(tvid, tycont) ->
      let meta = new_meta_type_variable_name () in
        (string_of_type_struct_sub tycont ((tvid, meta) :: lst))

  | TypeArgument(_, tyvarnm) -> "['" ^ tyvarnm ^ "]"

and string_of_type_argument_list tyarglist =
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = string_of_type_struct head in
      let strtl = string_of_type_argument_list tail in
        begin
          match head with
          | FuncType(_, _, _)            -> "(" ^ strhd ^ ")"
          | ListType(_, _)               -> "(" ^ strhd ^ ")"
          | RefType(_, _)                -> "(" ^ strhd ^ ")"
          | ProductType(_, _)            -> "(" ^ strhd ^ ")"
          | VariantType(_, [], _)        -> strhd
          | VariantType(_, _ :: _, _)    -> "(" ^ strhd ^ ")"
          | TypeSynonym(_, [], _, _)     -> strhd
          | TypeSynonym(_, _ :: _, _, _) -> "(" ^ strhd ^ ")"
          | _                            -> strhd
        end ^ " " ^ strtl

and string_of_type_struct_list tylist lst =
  match tylist with
  | []           -> ""
  | head :: tail ->
      let strhead = string_of_type_struct_sub head lst in
      let strtail = string_of_type_struct_list tail lst in
        begin match head with
        | ProductType(_, _) -> "(" ^ strhead ^ ")"
        | FuncType(_, _, _) -> "(" ^ strhead ^ ")"
        | _                 -> strhead
        end ^
        begin match tail with
        | [] -> ""
        | _  -> " * " ^ strtail
        end


(* -- following are all for debug -- *)


(* untyped_abstract_tree -> string *)
let rec string_of_utast (_, utastmain) =
  match utastmain with
  | UTStringEmpty                  -> "{}"
  | UTNumericConstant(nc)          -> string_of_int nc
  | UTBooleanConstant(bc)          -> string_of_bool bc
  | UTStringConstant(sc)           -> "{" ^ sc ^ "}"
  | UTUnitConstant                 -> "()"
  | UTContentOf(varnm)             -> varnm
  | UTConcat(ut1, (_, UTStringEmpty)) -> string_of_utast ut1
  | UTConcat(ut1, ut2)             -> "(" ^ (string_of_utast ut1) ^ " ^ " ^ (string_of_utast ut2) ^ ")"
  | UTApply(ut1, ut2)              -> "(" ^ (string_of_utast ut1) ^ " " ^ (string_of_utast ut2) ^ ")"
  | UTListCons(hd, tl)             -> "(" ^ (string_of_utast hd) ^ " :: " ^ (string_of_utast tl) ^ ")" 
  | UTEndOfList                    -> "[]"
  | UTTupleCons(hd, tl)            -> "(" ^ (string_of_utast hd) ^ ", " ^ (string_of_utast tl) ^ ")"
  | UTEndOfTuple                   -> "$"
  | UTBreakAndIndent               -> "break"
  | UTLetIn(umlc, ut)              -> "(let ... in " ^ (string_of_utast ut) ^ ")"
  | UTIfThenElse(ut1, ut2, ut3)    -> "(if " ^ (string_of_utast ut1) ^ " then "
                                        ^ (string_of_utast ut2) ^ " else " ^ (string_of_utast ut3) ^ ")"
  | UTLambdaAbstract(_, varnm, ut) -> "(" ^ varnm ^ " -> " ^ (string_of_utast ut) ^ ")"
  | UTFinishHeaderFile             -> "finish"
  | UTPatternMatch(ut, pmcons)     -> "(match " ^ (string_of_utast ut) ^ " with" ^ (string_of_pmcons pmcons) ^ ")"
  | UTItemize(itmz)                -> "(itemize " ^ string_of_itemize 0 itmz ^ ")"
(*  | UTDeclareVariantIn() *)
  | _ -> "?"

and string_of_itemize dp (UTItem(utast, itmzlst)) =
  "(" ^ (String.make dp '*') ^ " " ^ (string_of_utast utast)
    ^ (List.fold_left (fun x y -> x ^ " " ^ y) "" (List.map (string_of_itemize (dp + 1)) itmzlst)) ^ ")"

and string_of_pmcons pmcons =
  match pmcons with
  | UTEndOfPatternMatch -> ""
  | UTPatternMatchCons(pat, ut, tail)
      -> " | " ^ (string_of_utpat pat) ^ " -> " ^ (string_of_utast ut) ^ (string_of_pmcons tail)
  | UTPatternMatchConsWhen(pat, utb, ut, tail)
      -> " | " ^ (string_of_utpat pat) ^ " when " ^ (string_of_utast utb)
          ^ " -> " ^ (string_of_utast ut) ^ (string_of_pmcons tail)

and string_of_utpat (_, pat) =
  match pat with
  | UTPNumericConstant(nc)  -> string_of_int nc
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


(* abstract_tree -> string *)
let rec string_of_ast ast =
  match ast with
  | LambdaAbstract(x, m)         -> "(" ^ x ^ " -> " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(" ^ x ^ " *-> " ^ (string_of_ast m) ^ ")"
  | ContentOf(v)                 -> v
  | Apply(m, n)                  -> "(" ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t)                 -> (string_of_ast s) ^ (string_of_ast t)
  | StringEmpty                  -> ""
  | StringConstant(sc)           -> "{" ^ sc ^ "}"
  | NumericConstant(nc)          -> string_of_int nc
  | BooleanConstant(bc)          -> string_of_bool bc
  | IfThenElse(b, t, f)          ->
      "(if " ^ (string_of_ast b) ^ " then " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | IfClassIsValid(t, f)         -> "(if-class-is-valid " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | IfIDIsValid(t, f)            -> "(if-id-is-valid " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | ApplyClassAndID(c, i, m)     ->
      "(apply-class-and-id " ^ (string_of_ast c) ^ " " ^ (string_of_ast i) ^ " " ^ (string_of_ast m) ^ ")"
  | Reference(a)                 -> "(!" ^ (string_of_ast a) ^ ")"
  | ReferenceFinal(a)            -> "(!!" ^ (string_of_ast a) ^ ")"
  | Overwrite(vn, n)             -> "(" ^ vn ^ " <- " ^ (string_of_ast n) ^ ")"
  | MutableValue(mv)             -> "(mutable " ^ (string_of_ast mv) ^ ")"
  | UnitConstant                 -> "()"
  | LetMutableIn(vn, d, f)       -> "(let-mutable " ^ vn ^ " <- " ^ (string_of_ast d) ^ " in " ^ (string_of_ast f) ^ ")"
  | ListCons(a, cons)            -> "(" ^ (string_of_ast a) ^ " :: " ^ (string_of_ast cons) ^ ")"
  | EndOfList                    -> "[]"
  | TupleCons(a, cons)           -> "(" ^ (string_of_ast a) ^ ", " ^ (string_of_ast cons) ^ ")"
  | EndOfTuple                   -> "$"
  | BreakAndIndent               -> "break"
  | FinishHeaderFile             -> "finish-header-file"
  | EvaluatedEnvironment(_)      -> "evaluated-environment"
  | DeeperIndent(m)              -> "(deeper " ^ (string_of_ast m) ^ ")"
  | Constructor(c, m)            -> "(constructor " ^ c ^ " " ^ (string_of_ast m) ^ ")"
  | NoContent                    -> "no-content"
  | PatternMatch(_, _)           -> "(match ...)"
  | LetIn(_, m)                  -> "(let ... in " ^ (string_of_ast m) ^ ")"
  | WhileDo(m, n)                -> "(while " ^ (string_of_ast m) ^ " do " ^ (string_of_ast n) ^ ")"
  | DeclareGlobalHash(m, n)      -> "(declare-global-hash " ^ (string_of_ast m) ^ " <<- " ^ (string_of_ast n) ^ ")"
  | OverwriteGlobalHash(m, n)    -> "(overwrite-global-hash " ^ (string_of_ast m) ^ " <<- " ^ (string_of_ast n) ^ ")"
  | Module(mn, _, _)             -> "(module " ^ mn ^ " = struct ... end-struct)"
  | Sequential(m, n)             -> "(sequential " ^ (string_of_ast m) ^ " ; " ^ (string_of_ast n) ^ ")"
  | PrimitiveSame(m, n)          -> "(same " ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | PrimitiveStringSub(m, n, o)  ->
      "(string-sub " ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ " " ^ (string_of_ast o) ^ ")"
  | PrimitiveStringLength(m)     -> "(string-length " ^ (string_of_ast m) ^ ")"
  | PrimitiveArabic(m)           -> "(arabic " ^ (string_of_ast m) ^ ")"
  | _                            -> "?"


let rec string_of_type_struct_basic tystr =
  let (sttln, _, _, _) = Typeenv.get_range_from_type tystr in
    match tystr with
    | StringType(_)                      -> if sttln <= 0 then "string?" else "string"
    | IntType(_)                         -> if sttln <= 0 then "int?"    else "int"
    | BoolType(_)                        -> if sttln <= 0 then "bool?"   else "bool"
    | UnitType(_)                        -> if sttln <= 0 then "unit?"   else "unit"

    | VariantType(_, tyarglist, varntnm) ->
        (string_of_type_argument_list_basic tyarglist) ^ varntnm ^ (if sttln <= 0 then "@?" else "@")

    | TypeSynonym(_, tyarglist, tynm, tycont) ->
        (string_of_type_argument_list_basic tyarglist) ^ tynm ^ "(= " ^ (string_of_type_struct_basic tycont) ^ ")"

    | FuncType(_, tydom, tycod)    ->
        let strdom = string_of_type_struct_basic tydom in
        let strcod = string_of_type_struct_basic tycod in
          begin match tydom with
          | FuncType(_, _, _)     -> "(" ^ strdom ^ ")"
          | _                     -> strdom
          end ^ " ->" ^ (if sttln <= 0 then "? " else " ") ^ strcod

    | ListType(_, tycont)          ->
        let strcont = string_of_type_struct_basic tycont in
          begin match tycont with
          | FuncType(_, _, _)            -> "(" ^ strcont ^ ")"
          | ProductType(_, _)            -> "(" ^ strcont ^ ")"
          | VariantType(_, [], _)        -> strcont
          | VariantType(_, _, _)         -> "(" ^ strcont ^ ")"
          | TypeSynonym(_, [], _, _)     -> strcont
          | TypeSynonym(_, _ :: _, _, _) -> "(" ^ strcont ^ ")"
          | _                            -> strcont
          end ^ " list" ^ (if sttln <= 0 then "?" else "")

    | RefType(_, tycont)           ->
        let strcont = string_of_type_struct_basic tycont in
          begin match tycont with
          | FuncType(_, _, _)            -> "(" ^ strcont ^ ")"
          | ProductType(_, _)            -> "(" ^ strcont ^ ")"
          | VariantType(_, [], _)        -> strcont
          | VariantType(_, _, _)         -> "(" ^ strcont ^ ")"
          | TypeSynonym(_, [], _, _)     -> strcont
          | TypeSynonym(_, _ :: _, _, _) -> "(" ^ strcont ^ ")"
          | _                            -> strcont
          end ^ " ref" ^ (if sttln <= 0 then "?" else "")

    | ProductType(_, tylist)       -> string_of_type_struct_list_basic tylist
    | TypeVariable(_, tvid)        -> "'" ^ (string_of_int tvid) ^ (if sttln <= 0 then "?" else "")
    | ForallType(tvid, tycont)     -> "('" ^ (string_of_int tvid) ^ ". " ^ (string_of_type_struct_basic tycont) ^ ")"
    | TypeArgument(_, tyargnm)     -> tyargnm

and string_of_type_argument_list_basic tyarglist =
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = string_of_type_struct_basic head in
      let strtl = string_of_type_argument_list_basic tail in
        begin
          match head with
          | FuncType(_, _, _)            -> "(" ^ strhd ^ ")"
          | ListType(_, _)               -> "(" ^ strhd ^ ")"
          | RefType(_, _)                -> "(" ^ strhd ^ ")"
          | ProductType(_, _)            -> "(" ^ strhd ^ ")"
          | TypeSynonym(_, [], _, _)     -> strhd
          | TypeSynonym(_, _ :: _, _, _) -> "(" ^ strhd ^ ")"
          | VariantType(_, [], _)        -> strhd
          | VariantType(_, _ :: _, _)    -> "(" ^ strhd ^ ")"
          | _                            -> strhd
        end ^ " " ^ strtl

and string_of_type_struct_list_basic tylist =
  match tylist with
  | []           -> ""
  | head :: []   ->
      let strhd = string_of_type_struct_basic head in
        begin
          match head with
          | ProductType(_, _) -> "(" ^ strhd ^ ")"
          | FuncType(_, _, _) -> "(" ^ strhd ^ ")"
          | _                 -> strhd
        end
  | head :: tail ->
      let strhd = string_of_type_struct_basic head in
      let strtl = string_of_type_struct_list_basic tail in
        begin
          match head with
          | ProductType(_, _) -> "(" ^ strhd ^ ")"
          | FuncType(_, _, _) -> "(" ^ strhd ^ ")"
          | _                 -> strhd
        end ^ " * " ^ strtl
