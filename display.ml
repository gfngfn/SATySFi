open Types

let type_variable_name_max : int ref = ref 0
let type_valiable_name_list : (Tyvarid.t * string * bool * kind_struct) list ref = ref []


let string_of_record_type (f : type_struct -> string) (asc : (field_name, type_struct) Assoc.t) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let string_of_kind_struct (f : type_struct -> string) (kdstr : kind_struct) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    match kdstr with
    | UniversalKind   -> "U"
    | RecordKind(asc) -> "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let rec variable_name_of_int (n : int) =
  ( if n >= 26 then
      variable_name_of_int ((n - n mod 26) / 26 - 1)
    else
      ""
  ) ^ (String.make 1 (Char.chr ((Char.code 'a') + n mod 26)))


let show_type_variable (f : type_struct -> string) (name : string) (bound : bool) (kdstr : kind_struct) =
  match kdstr with
  | UniversalKind   -> if bound then name else "#" ^ name
  | RecordKind(asc) -> if bound then name else "(#" ^ name ^ " <: " ^ (string_of_kind_struct f kdstr) ^ ")"


let new_type_variable_name (bound : bool) (f : type_struct -> string) (tvid : Tyvarid.t) (kdstr : kind_struct) =
  let res = variable_name_of_int (!type_variable_name_max) in
    begin
      type_variable_name_max := !type_variable_name_max + 1 ;
      type_valiable_name_list := (tvid, res, bound, kdstr) :: (!type_valiable_name_list) ;
      show_type_variable f res bound kdstr
    end

let new_bound_type_variable_name   = new_type_variable_name true
let new_unbound_type_variable_name = new_type_variable_name false


let find_type_variable (f : type_struct -> string) (tvid : Tyvarid.t) =
  let rec aux (lst : (Tyvarid.t * string * bool * kind_struct) list) =
    match lst with
    | []                           -> raise Not_found
    | (k, v, bound, kdstr) :: tail -> if Tyvarid.same k tvid then show_type_variable f v bound kdstr else aux tail
  in
    aux (!type_valiable_name_list)


let rec string_of_type_struct (kdenv : Kindenv.t) (tystr : type_struct) =
  begin
    type_variable_name_max := 0 ;
    type_valiable_name_list := [] ;
    string_of_type_struct_sub kdenv tystr
  end


and string_of_type_struct_double (kdenv : Kindenv.t) (tystr1 : type_struct) (tystr2 : type_struct) =
  begin
    type_variable_name_max := 0 ;
    type_valiable_name_list := [] ;
    let strty1 = string_of_type_struct_sub kdenv tystr1 in
    let strty2 = string_of_type_struct_sub kdenv tystr2 in
      (strty1, strty2)
  end


and string_of_type_struct_sub (kdenv : Kindenv.t) (tystr : type_struct) =
  let iter = string_of_type_struct_sub kdenv in
  let iter_args = string_of_type_argument_list kdenv in
  let iter_list = string_of_type_struct_list kdenv in
  let (_, tymain) = tystr in
  match tymain with
  | TypeVariable(tvid) ->
      ( if Tyvarid.is_quantifiable tvid then "'" else "'_") ^
        begin
          try find_type_variable (string_of_type_struct_sub kdenv) tvid with
          | Not_found ->
              begin
                try new_unbound_type_variable_name iter tvid (Kindenv.find kdenv tvid) with
                | Not_found -> failwith ("type variable id '" ^ (Tyvarid.show_direct tvid) ^ " not found in kind environment")
              end
        end

  | ForallType(tvid, kdstr, tycont) ->
      let meta = new_bound_type_variable_name iter tvid kdstr in
        "(forall " ^ meta ^ " <: " ^ (string_of_kind_struct (string_of_type_struct_sub kdenv) kdstr) ^ ". " ^ (iter tycont) ^ ")"

  | StringType                      -> "string"
  | IntType                         -> "int"
  | BoolType                        -> "bool"
  | UnitType                        -> "unit"

  | VariantType(tyarglist, varntnm) -> (iter_args tyarglist) ^ varntnm

  | TypeSynonym(tyarglist, tynm, tycont) -> (iter_args tyarglist) ^ tynm ^ " (= " ^ (iter tycont) ^ ")"

  | FuncType(tydom, tycod) ->
      let strdom = iter tydom in
      let strcod = iter tycod in
        begin
          match tydom with
          | (_, FuncType(_, _)) -> "(" ^ strdom ^ ")"
          | _                   -> strdom
        end ^ " -> " ^ strcod

  | ListType(tycont) ->
      let strcont = iter tycont in
        begin
          match tycont with
          | ( (_, FuncType(_, _)) | (_, ProductType(_)) ) -> "(" ^ strcont ^ ")"
          | _                                             -> strcont
        end ^ " list"

  | RefType(tycont) ->
      let strcont = iter tycont in
        begin
          match tycont with
          | ( (_, FuncType(_, _)) | (_, ProductType(_)) ) -> "(" ^ strcont ^ ")"
          | _                                             -> strcont
        end ^ " ref"

  | ProductType(tylist) -> iter_list tylist

  | TypeArgument(tyvarnm) -> "['" ^ tyvarnm ^ "]"

  | RecordType(asc) -> string_of_record_type iter asc


and string_of_type_argument_list kdenv tyarglist =
  let iter = string_of_type_struct_sub kdenv in
  let iter_args = string_of_type_argument_list kdenv in
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = iter head in
      let strtl = iter_args tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( FuncType(_, _) | ProductType(_) | TypeSynonym(_ :: _, _, _)
            | ListType(_) | RefType(_) | VariantType(_ :: _, _) )         -> "(" ^ strhd ^ ")"
          | _                                                             -> strhd
        end ^ " " ^ strtl

and string_of_type_struct_list kdenv tylist =
  let iter = string_of_type_struct_sub kdenv in
  let iter_list = string_of_type_struct_list kdenv in
  match tylist with
  | []           -> ""
  | head :: tail ->
      let strhead = iter head in
      let strtail = iter_list tail in
      let (_, headmain) = head in
      begin
        match headmain with
        | ( ProductType(_) | FuncType(_, _) ) -> "(" ^ strhead ^ ")"
        | _                                   -> strhead
      end ^
      begin
        match tail with
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
  | _ -> "OTHER"

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

(* abstract_tree -> string *)
let rec string_of_ast ast =
  match ast with
  | LambdaAbstract(x, m)         -> "(" ^ x ^ " -> " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(" ^ x ^ " *-> " ^ (string_of_ast m) ^ ")"
  | ContentOf(v)                 -> "_" ^ v ^ "_"
  | Apply(m, n)                  -> "(" ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t)                 -> "(" ^ (string_of_ast s) ^ " ^ " ^ (string_of_ast t) ^ ")"
  | StringEmpty                  -> "\"\""
  | StringConstant(sc)           -> "\"" ^ (escape_letters sc) ^ "\""
  | NumericConstant(nc)          -> string_of_int nc
  | BooleanConstant(bc)          -> string_of_bool bc
  | IfThenElse(b, t, f)          ->
      "(if " ^ (string_of_ast b) ^ " then " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | ApplyClassAndID(c, i, m)     ->
      "(apply-class-and-id " ^ (string_of_ast c) ^ " " ^ (string_of_ast i) ^ " " ^ (string_of_ast m) ^ ")"
  | Reference(a)                 -> "(!" ^ (string_of_ast a) ^ ")"
  | ReferenceFinal(a)            -> "(!!" ^ (string_of_ast a) ^ ")"
  | Overwrite(vn, n)             -> "(" ^ vn ^ " <- " ^ (string_of_ast n) ^ ")"
  | Location(loc)                -> "<mutable>"
  | UnitConstant                 -> "()"
  | LetMutableIn(vn, d, f)       -> "(let-mutable " ^ vn ^ " <- " ^ (string_of_ast d) ^ " in " ^ (string_of_ast f) ^ ")"
  | ListCons(a, cons)            -> "(" ^ (string_of_ast a) ^ " :: " ^ (string_of_ast cons) ^ ")"
  | EndOfList                    -> "[]"
  | TupleCons(a, cons)           -> "(" ^ (string_of_ast a) ^ ", " ^ (string_of_ast cons) ^ ")"
  | EndOfTuple                   -> "end-of-tuple"
  | BreakAndIndent               -> "break"
  | FinishHeaderFile             -> "finish-header-file"
  | EvaluatedEnvironment(_)      -> "evaluated-environment"
  | DeeperIndent(m)              -> "(deeper " ^ (string_of_ast m) ^ ")"
  | Constructor(c, m)            -> "(constructor " ^ c ^ " " ^ (string_of_ast m) ^ ")"
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
  | Record(asc)                  -> "(| ... |)"
  | AccessField(r, f)            -> (string_of_ast r) ^ "#" ^ f
  | _                            -> "OTHER"


let rec string_of_type_struct_basic tystr =
  let (rng, tymain) = tystr in
  let qstn = if Range.is_dummy rng then "?" else "" in
    match tymain with
    | StringType                      -> "string" ^ qstn
    | IntType                         -> "int" ^ qstn
    | BoolType                        -> "bool" ^ qstn
    | UnitType                        -> "unit" ^ qstn

    | VariantType(tyarglist, varntnm) ->
        (string_of_type_argument_list_basic tyarglist) ^ varntnm ^ "@" ^ qstn

    | TypeSynonym(tyarglist, tynm, tycont) ->
        (string_of_type_argument_list_basic tyarglist) ^ tynm ^ "(= " ^ (string_of_type_struct_basic tycont) ^ ")"

    | FuncType(tydom, tycod)    ->
        let strdom = string_of_type_struct_basic tydom in
        let strcod = string_of_type_struct_basic tycod in
          begin match tydom with
          | (_, FuncType(_, _)) -> "(" ^ strdom ^ ")"
          | _                   -> strdom
          end ^ " ->" ^ qstn ^ strcod

    | ListType(tycont)          ->
        let strcont = string_of_type_struct_basic tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | ( FuncType(_, _)
            | ProductType(_)
            | VariantType(_ :: _, _)
            | TypeSynonym(_ :: _, _, _) ) -> "(" ^ strcont ^ ")"
          | _                             -> strcont
          end ^ " list" ^ qstn

    | RefType(tycont)           ->
        let strcont = string_of_type_struct_basic tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | ( FuncType(_, _)
            | ProductType(_)
            | VariantType(_ :: _, _)
            | TypeSynonym(_ :: _, _, _) ) -> "(" ^ strcont ^ ")"
          | _                                -> strcont
          end ^ " ref" ^ qstn

    | ProductType(tylist)       -> string_of_type_struct_list_basic tylist
    | TypeVariable(tvid)        -> "'" ^ (Tyvarid.show_direct tvid) ^ qstn
    | ForallType(tvid, UniversalKind, tycont) -> "('" ^ (Tyvarid.show_direct tvid) ^ ". " ^ (string_of_type_struct_basic tycont) ^ ")"
    | ForallType(tvid, kdstr, tycont)         -> "('" ^ (Tyvarid.show_direct tvid) ^ " <: " ^ (string_of_kind_struct string_of_type_struct_basic kdstr) ^ ". " ^ (string_of_type_struct_basic tycont) ^ ")"
    | TypeArgument(tyargnm)     -> tyargnm
    | RecordType(asc)           -> string_of_record_type string_of_type_struct_basic asc


and string_of_type_argument_list_basic tyarglist =
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = string_of_type_struct_basic head in
      let strtl = string_of_type_argument_list_basic tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( FuncType(_, _) | ProductType(_) | TypeSynonym(_ :: _, _, _)
            | ListType(_) | RefType(_) | VariantType(_ :: _, _) )          -> "(" ^ strhd ^ ")"
          | _                                                              -> strhd
        end ^ " " ^ strtl


and string_of_type_struct_list_basic tylist =
  match tylist with
  | []           -> ""
  | head :: []   ->
      let strhd = string_of_type_struct_basic head in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( ProductType(_) | FuncType(_, _) ) -> "(" ^ strhd ^ ")"
          | _                                   -> strhd
        end
  | head :: tail ->
      let strhd = string_of_type_struct_basic head in
      let strtl = string_of_type_struct_list_basic tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( ProductType(_) | FuncType(_, _) ) -> "(" ^ strhd ^ ")"
          | _                                   -> strhd
        end ^ " * " ^ strtl


let string_of_kind_struct_basic kdstr = string_of_kind_struct string_of_type_struct_basic kdstr


let string_of_kind_environment kdenv = Kindenv.to_string string_of_kind_struct_basic kdenv
