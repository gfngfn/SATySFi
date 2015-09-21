%{
  open Types

  type literal_reading_state = Normal | ReadingSpace
  type range_kind =
    | Tok       of token_position
    | TokArg    of (token_position * string)
    | Untyped   of untyped_abstract_tree
    | Pat       of untyped_pattern_tree
    | PatCons   of untyped_pattern_match_cons
    | TypeStr   of type_struct
    | VarntCons of untyped_variant_cons

  (* range_kind -> range_kind -> code_range *)
  let make_range sttx endx =
    let (sttln, sttpos) =
      match sttx with
      | Tok(l, s, _)                 -> (l, s)
      | TokArg((l, s, _), _)         -> (l, s)
      | Untyped((sl, sp, _, _), _)   -> (sl, sp)
      | Pat((sl, sp, _, _), _)       -> (sl, sp)
      | PatCons((sl, sp, _, _), _)   -> (sl, sp)
      | VarntCons((sl, sp, _, _), _) -> (sl, sp)
      | TypeStr(tystr)               ->
          let (sl, sp, _, _) = Typeenv.get_range_from_type tystr in (sl, sp)
    in
    let (endln, endpos) =
      match endx with
      | Tok(l, _, e)                 -> (l, e)
      | TokArg((l, _, e), _)         -> (l, e)
      | Untyped((_, _, el, ep), _)   -> (el, ep)
      | Pat((_, _, el, ep), _)       -> (el, ep)
      | PatCons((_, _, el, ep), _)   -> (el, ep)
      | VarntCons((_, _, el, ep), _) -> (el, ep)
      | TypeStr(tystr)               ->
          let (_, _, el, ep) = Typeenv.get_range_from_type tystr in (el, ep)
    in
      (sttln, sttpos, endln, endpos)

  (* token_position -> code_range *)
  let extract_range (ln, sttpos, endpos) = (ln, sttpos, ln, endpos)

  (* token_position * string -> code_range * string *)
  let extract_range_and_name ((ln, sttpos, endpos), name) = ((ln, sttpos, ln, endpos), name)

  (* untyped_argument_cons -> untyped_argument_cons -> untyped_argument_cons *)
  let rec append_argument_list arglsta arglstb =
    match arglsta with
    | UTEndOfArgument              -> arglstb
    | UTArgumentCons(arg, arglstl) ->
        UTArgumentCons(arg, (append_argument_list arglstl arglstb))

  (* ctrlseq_name -> untyped_abstract_tree -> untyped_abstract_tree -> untyped_argument_cons -> untyped_abstract_tree *)
  let rec convert_into_apply csutast clsnmutast idnmutast argcons =
    convert_into_apply_sub argcons ((-12, 0, 0, 0), UTApplyClassAndID(clsnmutast, idnmutast, csutast))

  (* argument_cons -> untyped_abstract_tree -> untyped_abstract_tree *)
  and convert_into_apply_sub argcons utastconstr =
    match argcons with
    | UTEndOfArgument             -> utastconstr
    | UTArgumentCons(arg, actail) ->
        let (rng, _) = arg in
          convert_into_apply_sub actail (rng, UTApply(utastconstr, arg))

  let class_name_to_abstract_tree clsnm =
    UTStringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  let id_name_to_abstract_tree idnm =
    UTStringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  let rec curry_lambda_abstract rng argvarcons utastdef =
    match argvarcons with
    | UTEndOfArgumentVariable -> utastdef
    | UTArgumentVariableCons(varrng, argvar, avtail) ->
        (rng, UTLambdaAbstract(varrng, argvar, curry_lambda_abstract (-11, 0, 0, 0) avtail utastdef))

  let error_reporting msg disp pos =
    let (pos_ln, pos_start, pos_end) = pos in
      "Syntax error: " ^ msg ^ ".\n\n    " ^ disp
        ^ "\n\n  (at line " ^ (string_of_int pos_ln) ^ ", "
        ^ (string_of_int pos_start) ^ "-" ^ (string_of_int pos_end) ^ ")"

  let rec string_of_avc argvarcons =
    match argvarcons with
    | UTEndOfArgumentVariable                   -> ""
    | UTArgumentVariableCons(_, argvar, avtail) -> argvar ^ " " ^ (string_of_avc avtail)

  let rec stringify_literal ltrl =
    let (_, ltrlmain) = ltrl in
      match ltrlmain with
      | UTConcat(utastf, utastl) -> (stringify_literal utastf) ^ (stringify_literal utastl)
      | UTStringConstant(s)      -> s
      | UTStringEmpty            -> ""
      | _  -> raise (ParseErrorDetail("illegal token in literal area; this cannot happen"))

  let rec omit_pre_spaces str =
    if String.sub str 0 1 = " "  then
      omit_pre_spaces (String.sub str 1 ((String.length str) - 1)) 
(*    else if String.sub str 0 1 = "\n" then
      (String.sub str 1 ((String.length str) - 1)) *)
    else
      str

  let rec omit_post_spaces str =
    if String.sub str ((String.length str) - 1) 1 = " " then
      omit_post_spaces (String.sub str 0 ((String.length str) - 1))
    else if String.sub str ((String.length str) - 1) 1 = "\n" then
      (String.sub str 0 ((String.length str) - 1))
    else
      str

  (* untyped_abstract_tree -> untyped_abstract_tree_main *)
  let rec omit_spaces ltrl =
    let str_ltrl = omit_post_spaces (omit_pre_spaces (stringify_literal ltrl)) in
      let min_indent = min_indent_space str_ltrl in
        let str_shaved = shave_indent str_ltrl min_indent in
          if str_shaved.[(String.length str_shaved) - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 ((String.length str_shaved) - 1) in
              UTConcat(
                ((-13, 0, 0, 0), UTStringConstant(str_no_last_break)),
                ((-14, 0, 0, 0), UTBreakAndIndent)
              )
          else
            UTStringConstant(str_shaved)

  (* string -> int *)
  and min_indent_space str_ltrl =
    min_indent_space_sub str_ltrl 0 ReadingSpace 0 (String.length str_ltrl)

  (* string -> int -> literal_reading_state -> int -> int -> int *)
  and min_indent_space_sub str_ltrl index lrstate spnum minspnum =
    if index >= (String.length str_ltrl) then
      (* ( print_string ("min_indent: " ^ (string_of_int minspnum) ^ "\n") ; *)
        minspnum
      (* ) *)
    else
      match lrstate with
      | Normal ->
          ( match str_ltrl.[index] with
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 minspnum
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' '  -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace (spnum + 1) minspnum
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
                (* does not take space-only line into account *)
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 (if spnum < minspnum then spnum else minspnum)
          )

  and shave_indent str_ltrl minspnum =
    shave_indent_sub str_ltrl minspnum 0 "" Normal 0

  and shave_indent_sub str_ltrl minspnum index str_constr lrstate spnum =
    if index >= (String.length str_ltrl) then
      str_constr
    else
      match lrstate with
      | Normal ->
          ( match str_ltrl.[index] with
            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' ' ->
                if spnum < minspnum then
                  shave_indent_sub str_ltrl minspnum (index + 1) str_constr ReadingSpace (spnum + 1)
                else
                  shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ " ") ReadingSpace (spnum + 1)

            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )

  let binary_operator opname lft op rgt =
    let (opln, opstt, opend) = op in
    let oprng = (opln, opstt, opln, opend) in
    let rng = make_range (Untyped lft) (Untyped rgt) in
      (rng, UTApply(((-15, 0, 0, 0), UTApply((oprng, UTContentOf(opname)), lft)), rgt))

  let make_let_expression lettk vartk utargcons utastdef decs utastaft =
    let (varrng, varnm) = extract_range_and_name vartk in
    let rng = make_range (Tok lettk) (Untyped utastaft) in
    let curried = curry_lambda_abstract varrng utargcons utastdef in
      (rng, UTLetIn(UTMutualLetCons(varnm, curried, decs), utastaft))

  let make_let_mutable_expression letmuttk vartk utastdef utastaft =
    let (varrng, varnm) = extract_range_and_name vartk in
    let rng = make_range (Tok letmuttk) (Untyped utastaft) in
      (rng, UTLetMutableIn(varrng, varnm, utastdef, utastaft))

%}

%token <Types.token_position * Types.var_name> VAR
%token <Types.token_position * Types.var_name> VARINSTR
%token <Types.token_position * string> NUMCONST
%token <Types.token_position * string> CHAR
%token <Types.token_position> SPACE
%token <Types.token_position> BREAK
%token <Types.token_position * Types.ctrlseq_name> CTRLSEQ
%token <Types.token_position * Types.id_name> IDNAME
%token <Types.token_position * Types.class_name> CLASSNAME
%token <Types.token_position> END
%token <Types.token_position> LAMBDA ARROW
%token <Types.token_position> LET DEFEQ LETAND IN
%token <Types.token_position> LETMUTABLE OVERWRITEEQ
%token <Types.token_position> REFNOW REFFINAL
%token <Types.token_position> IF THEN ELSE IFCLASSISVALID IFIDISVALID
%token <Types.token_position> LPAREN
%token <Types.token_position> RPAREN
%token <Types.token_position> TIMES DIVIDES
%token <Types.token_position> MOD
%token <Types.token_position> PLUS MINUS
%token <Types.token_position> EQ NEQ GEQ LEQ GT LT
%token <Types.token_position> LNOT
%token <Types.token_position> LAND
%token <Types.token_position> LOR
%token <Types.token_position> CONCAT
%token <Types.token_position> OPENQT CLOSEQT
%token <Types.token_position> OPENSTR CLOSESTR
%token <Types.token_position> OPENNUM CLOSENUM
%token <Types.token_position> BGRP EGRP
%token <Types.token_position> TRUE FALSE
%token <Types.token_position> FINISH
%token <Types.token_position> SEP
%token <Types.token_position> BLIST LISTPUNCT ELIST CONS
%token <Types.token_position> BEFORE
%token <Types.token_position> UNITVALUE
%token <Types.token_position> WHILE DO
%token <Types.token_position> NEWGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
%token <Types.token_position> MUTUAL ENDMUTUAL
%token <Types.token_position> COMMA
%token <Types.token_position> MATCH WITH BAR WILDCARD WHEN AS
%token <Types.token_position> VARIANT OF
%token <Types.token_position * Types.constructor_name> CONSTRUCTOR
%token EOI
%token IGNORED

%nonassoc LET DEFEQ IN LETAND LETMUTABLE OVERWRITEEQ
%nonassoc MATCH WITH
%nonassoc IF THEN ELSE
%left OVERWRITEGLOBALHASH
%left BEFORE
%nonassoc WHILE
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%left PLUS
%right MINUS
%left TIMES
%right MOD DIVIDES
%nonassoc VAR
%nonassoc LPAREN RPAREN

%start main
%type <Types.untyped_abstract_tree> main
%type <Types.untyped_abstract_tree> nxlet
%type <Types.untyped_abstract_tree> nxletsub
%type <Types.untyped_mutual_let_cons> nxdec
%type <Types.untyped_abstract_tree> nxbfr
%type <Types.untyped_abstract_tree> nxwhl
%type <Types.untyped_abstract_tree> nxif
%type <Types.untyped_abstract_tree> nxlor
%type <Types.untyped_abstract_tree> nxland
%type <Types.untyped_abstract_tree> nxcomp
%type <Types.untyped_abstract_tree> nxconcat
%type <Types.untyped_abstract_tree> nxlplus
%type <Types.untyped_abstract_tree> nxltimes
%type <Types.untyped_abstract_tree> nxrplus
%type <Types.untyped_abstract_tree> nxrtimes
%type <Types.untyped_abstract_tree> nxun
%type <Types.untyped_abstract_tree> nxapp
%type <Types.untyped_abstract_tree> nxbot
%type <Types.untyped_abstract_tree> tuple
%type <Types.untyped_pattern_match_cons> pats
%type <Types.untyped_pattern_tree> pattr
%type <Types.untyped_pattern_tree> patbot
%type <Types.untyped_abstract_tree> nxlist
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxsepsub
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> sxbot
%type <Types.untyped_abstract_tree> sxclsnm
%type <Types.untyped_abstract_tree> sxidnm
%type <Types.untyped_argument_cons> narg
%type <Types.untyped_argument_cons> sarg
%type <Types.untyped_argument_cons> sargsub
%type <Types.untyped_argument_variable_cons> argvar
%type <string> binop

%%


main:
  | nxtoplevel EOI { $1 }
  | sxblock EOI { $1 }
;
nxtoplevel:
/* ---- toplevel style ---- */
  | LET VAR     argvar DEFEQ nxlet nxdec nxtoplevel { make_let_expression $1 $2 $3 $5 $6 $7 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec nxtoplevel { make_let_expression $1 $2 $3 $5 $6 $7 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet nxtoplevel     { make_let_mutable_expression $1 $2 $4 $5 }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual nxtoplevel { make_let_expression $2 $3 $4 $6 $7 $8 }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual nxtoplevel { make_let_expression $2 $3 $4 $6 $7 $8 }
  | VARIANT VAR DEFEQ variants nxvariantdec nxtoplevel {
        let (_, typenm) = extract_range_and_name $2 in
        let rng = make_range (Tok $1) (Untyped $6) in
          (rng, UTDeclareVariantIn(UTMutualVariantCons(typenm, $4, $5), $6))
      }
  | VARIANT VAR DEFEQ BAR variants nxvariantdec nxtoplevel {
        let (_, typenm) = extract_range_and_name $2 in
        let rng = make_range (Tok $1) (Untyped $7) in
          (rng, UTDeclareVariantIn(UTMutualVariantCons(typenm, $5, $6), $7))
      }
/* ---- toplevel terminal ---- */
  | LET VAR argvar DEFEQ nxlet nxdec {
        make_let_expression $1 $2 $3 $5 $6 ((-256, 0, 0, 0), UTFinishHeaderFile)
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec {
        make_let_expression $1 $2 $3 $5 $6 ((-257, 0, 0, 0), UTFinishHeaderFile)
      }
  | LETMUTABLE VAR OVERWRITEEQ nxlet {
        make_let_mutable_expression $1 $2 $4 ((-258, 0, 0, 0), UTFinishHeaderFile)
      }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual {
        make_let_expression $2 $3 $4 $6 $7 ((-259, 0, 0, 0), UTFinishHeaderFile)
      }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual {
        make_let_expression $2 $3 $4 $6 $7 ((-260, 0, 0, 0), UTFinishHeaderFile)
      }
  | VARIANT VAR DEFEQ variants nxvariantdec {
        let (_, typenm) = extract_range_and_name $2 in
        let rng = make_range (Tok $1) (Tok (-1, 0, 0)) in
          (rng, UTDeclareVariantIn(UTMutualVariantCons(typenm, $4, $5), ((-261, 0, 0, 0), UTFinishHeaderFile)))
      }
  | VARIANT VAR DEFEQ BAR variants nxvariantdec {
        let (_, typenm) = extract_range_and_name $2 in
        let rng = make_range (Tok $1) (Tok (-1, 0, 0)) in
          (rng, UTDeclareVariantIn(UTMutualVariantCons(typenm, $5, $6), ((-262, 0, 0, 0), UTFinishHeaderFile)))
      }
/* ---- transition to expression style ---- */
  | LET VAR     argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet     { make_let_mutable_expression $1 $2 $4 $6 }
  | MUTUAL LET VAR     argvar DEFEQ nxlet nxmutual IN nxlet { make_let_expression $2 $3 $4 $6 $7 $9 }
  | MUTUAL LET CTRLSEQ argvar DEFEQ nxlet nxmutual IN nxlet { make_let_expression $2 $3 $4 $6 $7 $9 }
/* ---- for syntax error log ---- */
  | LET error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'let'" "let ..<!>.." $1))
      }
  | LET VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'let'"
            ("let" ^ vn ^ " ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ..<!>..") $1))
      }
  | LET CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ csname ^ " ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ " = ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") ln))
      }
  | LETMUTABLE error {
        raise (ParseErrorDetail(error_reporting "missing identifier after 'let-mutable'"
          "let-mutable ..<!>.." $1))
      }
  | LETMUTABLE VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting ("missing '<-' after '" ^ vn ^ "'")
            ("let-mutable " ^ vn ^ " ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '<-'"
            ("let-mutable " ^ vn ^ " <- ..<!>..") $1))
      }
;
nxmutual: /* -> Types.untyped_mutual_let_cons */
  | LET VAR argvar DEFEQ nxlet nxmutual {
        let ((varln, varstt, varend), vn) = $2 in
        let varrng = (varln, varstt, varln, varend) in
        let curried = curry_lambda_abstract varrng $3 $5 in
          UTMutualLetCons(vn, curried, $6)
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxmutual {
        let ((csln, csstt, csend), cn) = $2 in
        let csrng = (csln, csstt, csln, csend) in
        let curried = curry_lambda_abstract csrng $3 $5 in
          UTMutualLetCons(cn, curried, $6)
      }
  | ENDMUTUAL { UTEndOfMutualLet }
;
nxlet:
  | MATCH nxlet WITH pats {
        let rng = make_range (Tok $1) (PatCons $4) in
          (rng, UTPatternMatch($2, $4))
      }
  | MATCH nxlet WITH BAR pats {
        let rng = make_range (Tok $1) (PatCons $5) in
          (rng, UTPatternMatch($2, $5))
      }
  | nxletsub { $1 }
nxletsub:
  | LET VAR argvar DEFEQ nxlet nxdec IN nxlet     { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN nxlet { make_let_expression $1 $2 $3 $5 $6 $8 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet     { make_let_mutable_expression $1 $2 $4 $6 }
  | nxwhl { $1 }
/* -- for syntax error log -- */
  | LET error {
        raise (ParseErrorDetail(error_reporting
          "missing identifier after 'let'" "let ..<!>.." $1))
      }
  | LET VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ vn ^ " ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ..<!>..") $1))
      }
  | LET VAR argvar DEFEQ nxlet nxdec IN error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ vn ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") $1))
      }
  | LET CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument"
            ("let " ^ csname ^ " ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '='"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ " = ..<!>..") ln))
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec IN error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") ln))
      }
  | LETMUTABLE error {
        raise (ParseErrorDetail(error_reporting "missing identifier after 'let-mutable'"
          "let-mutable ..<!>.." $1))
      }
  | LETMUTABLE VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting ("missing '<-' after '" ^ vn ^ "'")
            ("let-mutable " ^ vn ^ " ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after '<-'"
            ("let-mutable " ^ vn ^ " <- ..<!>..") $1))
      }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
          "in ..<!>.." $5))
      }
;
nxdec: /* -> mutual_let_cons */
  | LETAND VAR argvar DEFEQ nxlet nxdec {
        let (varrng, varnm) = extract_range_and_name $2 in
        let curried = curry_lambda_abstract varrng $3 $5 in
          UTMutualLetCons(varnm, curried, $6)
      }
  | LETAND CTRLSEQ argvar DEFEQ nxlet nxdec {
        let (csrng, csnm) = extract_range_and_name $2 in
        let curried = curry_lambda_abstract csrng $3 $5 in
          UTMutualLetCons(csnm, curried, $6)
      }
  | { UTEndOfMutualLet }
/* -- for syntax error log -- */
  | LETAND VAR error {
        let (_, varnm) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ varnm ^ " ..<!>..") $1))
      }
  | LETAND CTRLSEQ error {
        let (ln, csnm) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ csnm ^ " ..<!>..") ln))
      }
;
nxvariantdec: /* -> mutual_variant_cons */
  | LETAND VAR DEFEQ variants nxvariantdec {
        let (_, typenm) = extract_range_and_name $2 in
          UTMutualVariantCons(typenm, $4, $5)
      }
  | { UTEndOfMutualVariant }
;
nxwhl:
  | WHILE nxlet DO nxwhl {
        let rng = make_range (Tok $1) (Untyped $4) in
          (rng, UTWhileDo($2, $4))
      }
  | nxif { $1 }
/* -- for syntax error log --*/
  | WHILE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'while'" "while ..<!>.." $1))
      }
  | WHILE nxlet DO error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'do'" "do ..<!>.." $3))
      }
nxif:
  | IF nxlet THEN nxlet ELSE nxlet {
        let rng = make_range (Tok $1) (Untyped $6) in
          (rng, UTIfThenElse($2, $4, $6))
      }
  | IFCLASSISVALID nxlet ELSE nxlet {
        let rng = make_range (Tok $1) (Untyped $4) in
          (rng, UTIfClassIsValid($2, $4))
      }
  | IFCLASSISVALID THEN nxlet ELSE nxlet {
        let rng = make_range (Tok $1) (Untyped $5) in
          (rng, UTIfClassIsValid($3, $5))
      }
  | IFIDISVALID nxlet ELSE nxlet {
        let rng = make_range (Tok $1) (Untyped $4) in
          (rng, UTIfIDIsValid($2, $4))
      }
  | IFIDISVALID THEN nxlet ELSE nxlet {
        let rng = make_range (Tok $1) (Untyped $5) in
          (rng, UTIfIDIsValid($3, $5))
      }
  | nxbfr { $1 }
/* -- for syntax error log -- */
  | IF error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if'" "if ..<!>.." $1))
      }
  | IF nxlet THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $3))
      }
  | IF nxlet THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $5))
      }
  | IFCLASSISVALID error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if-class-is-valid'" "if-class-is-valid ..<!>.." $1))
      }
  | IFCLASSISVALID nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $3))
      }
  | IFCLASSISVALID THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $2))
      }
  | IFCLASSISVALID THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $4))
      }
  | IFIDISVALID error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'if-id-is-valid'" "if-id-is-valid ..<!>.." $1))
      }
  | IFIDISVALID nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $3))
      }
  | IFIDISVALID THEN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'then'" "then ..<!>.." $2))
      }
  | IFIDISVALID THEN nxlet ELSE error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'else'" "else ..<!>.." $4))
      }
;
nxbfr:
  | nxlambda BEFORE nxbfr {
        let rng = make_range (Untyped $1) (Untyped $3) in
          (rng, UTSequential($1, $3))
      }
  | nxlambda { $1 }
/* -- for syntax error log -- */
  | nxlambda BEFORE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'before'" "before ..<!>.." $2))
      }
;
nxlambda:
  | VAR OVERWRITEEQ nxlor {
        let (varrng, varnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (Untyped $3) in
          (rng, UTOverwrite(varrng, varnm, $3))
      }
  | NEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        let rng = make_range (Tok $1) (Untyped $4) in
          (rng, UTDeclareGlobalHash($2, $4))
      }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        let rng = make_range (Tok $1) (Untyped $4) in
          (rng, UTOverwriteGlobalHash($2, $4))
      }
  | LAMBDA argvar ARROW nxlor {
        let rng = make_range (Tok $1) (Untyped $4) in
          curry_lambda_abstract rng $2 $4
      }
  | nxlor { $1 }
/* -- for syntax error log -- */
  | LAMBDA error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after 'function'" "function ..<!>.." $1))
      }
  | LAMBDA argvar ARROW error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '->'" "-> ..<!>.." $3))
      }
  | NEWGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after 'declare-global-hash'" "declare-global-hash ..<!>.." $1))
      }
  | NEWGLOBALHASH nxlet OVERWRITEGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after '<<-'" "<<- ..<!>.." $3))
      }
  | RENEWGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after 'renew'" "renew ..<!>.." $1))
      }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH error {
        raise (ParseErrorDetail(error_reporting
          "illegal token after '<<-'" "<<- ..<!>.." $3))
      }
;
argvar: /* -> Types.argument_variable_cons */
  | VAR argvar {
        let (varrng, vn) = extract_range_and_name $1 in
          UTArgumentVariableCons(varrng, vn, $2)
      }
  | { UTEndOfArgumentVariable }
;
nxlor:
  | nxland LOR nxlor { binary_operator "||" $1 $2 $3 }
  | nxland { $1 }
/* -- for syntax error log -- */
  | nxland LOR error {
        raise (ParseErrorDetail(error_reporting "illegal token after '||'" "|| ..<!>.." $2))
      }
;
nxland:
  | nxcomp LAND nxland { binary_operator "&&" $1 $2 $3 }
  | nxcomp { $1 }
/* -- for syntax error log -- */
  | nxcomp LAND error {
        raise (ParseErrorDetail(error_reporting "illegal token after '&&'" "&& ..<!>.." $2))
      }
;
nxcomp:
  | nxconcat EQ nxcomp  { binary_operator "==" $1 $2 $3 }
  | nxconcat NEQ nxcomp { binary_operator "<>" $1 $2 $3 }
  | nxconcat GEQ nxcomp { binary_operator ">=" $1 $2 $3 }
  | nxconcat LEQ nxcomp { binary_operator "<=" $1 $2 $3 }
  | nxconcat GT nxcomp  { binary_operator ">" $1 $2 $3 }
  | nxconcat LT nxcomp  { binary_operator "<" $1 $2 $3 }
  | nxconcat { $1 }
/* -- for syntax error log -- */
  | nxconcat EQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '=='" "== ..<!>.." $2))
      }
  | nxconcat NEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<>'" "<> ..<!>.." $2))
      }
  | nxconcat GEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '>='" ">= ..<!>.." $2))
      }
  | nxconcat LEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<='" "<= ..<!>.." $2))
      }
  | nxconcat GT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '>'" "> ..<!>.." $2))
      }
  | nxconcat LT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '<'" "< ..<!>.." $2))
      }
;
nxconcat:
  | nxlplus CONCAT nxconcat { binary_operator "^" $1 $2 $3 }
  | nxlplus CONS nxconcat   { binary_operator "::" $1 $2 $3 }
  | nxlplus { $1 }
/* -- for syntax error log -- */
  | nxlplus CONCAT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '^'" "^ ..<!>.." $2))
      }
;
nxlplus:
  | nxlminus PLUS nxrplus { binary_operator "+" $1 $2 $3 }
  | nxlminus { $1 }
/* -- for syntax error log -- */
  | nxlminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxlminus:
  | nxlplus MINUS nxrtimes { binary_operator "-" $1 $2 $3 }
  | nxltimes { $1 }
/* -- for syntax error log -- */
  | nxlplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxrplus:
  | nxrminus PLUS nxrplus { binary_operator "+" $1 $2 $3 }
  | nxrminus { $1 }
/* -- for syntax error log -- */
  | nxrminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxrminus:
  | nxrplus MINUS nxrtimes { binary_operator "+" $1 $2 $3 }
  | nxrtimes { $1 }
/* -- for syntax error log -- */
  | nxrplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxltimes:
  | nxun TIMES nxrtimes    { binary_operator "*" $1 $2 $3 }
  | nxltimes DIVIDES nxapp { binary_operator "/" $1 $2 $3 }
  | nxltimes MOD nxapp     { binary_operator "mod" $1 $2 $3 }
  | nxun { $1 }
/* -- for syntax error log -- */
  | nxun TIMES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '*'" "* ..<!>.." $2))
      }
  | nxltimes DIVIDES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '/'" "/ ..<!>.." $2))
      }
  | nxltimes MOD error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'mod'" "mod ..<!>.." $2))
      }
;
nxrtimes:
  | nxapp TIMES nxrtimes   { binary_operator "*" $1 $2 $3 }
  | nxrtimes DIVIDES nxapp { binary_operator "/" $1 $2 $3 }
  | nxrtimes MOD nxapp     { binary_operator "mod" $1 $2 $3 }
  | nxapp { $1 }
/* -- for syntax error log -- */
  | nxapp TIMES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '*'" "* ..<!>.." $2))
      }
  | nxrtimes DIVIDES error {
        raise (ParseErrorDetail(error_reporting "illegal token after '/'" "/ ..<!>.." $2))
      }
  | nxrtimes MOD error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'mod'" "mod ..<!>.." $2))
      }
;
nxun:
  | MINUS nxapp { binary_operator "-" ((-16, 0, 0, 0), UTNumericConstant(0)) $1 $2 }
  | LNOT nxapp  {
        let lnotrng = extract_range $1 in
        let rng = make_range (Tok $1) (Untyped $2) in
          (rng, UTApply((lnotrng, UTContentOf("not")), $2))
      }
  | REFNOW nxapp {
        let refnowrng = extract_range $1 in
        let rng = make_range (Tok $1) (Untyped $2) in
          (rng, UTApply((refnowrng, UTContentOf("!")), $2))
      }
  | REFFINAL nxapp {
        let rng = make_range (Tok $1) (Untyped $2) in
          (rng, UTReferenceFinal($2))
      }
  | nxapp { $1 }
/* -- for syntax error log -- */
  | MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after unary '-'" "- ..<!>.." $1))
      }
  | LNOT error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'not'" "not ..<!>.." $1))
      }
  | REFNOW error {
        raise (ParseErrorDetail(error_reporting "illegal token after '!'" "! ..<!>.." $1))
      }
  | REFFINAL error {
        raise (ParseErrorDetail(error_reporting "illegal token after '!!'" "!! ..<!>.." $1))
      }
;
nxapp:
  | nxapp nxbot {
        let rng = make_range (Untyped $1) (Untyped $2) in
          (rng, UTApply($1, $2))
      }
  | nxbot { $1 }
;
nxbot:
  | VAR {
        let (rng, vn) = extract_range_and_name $1 in
          (rng, UTContentOf(vn))
      }
  | CONSTRUCTOR nxbot {
        let (_, constrnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (Untyped $2) in
          (rng, UTConstructor(constrnm, $2))
      }
  | NUMCONST {
        let (rng, ncs) = extract_range_and_name $1 in
          (rng, UTNumericConstant(int_of_string ncs))
      }
  | TRUE  {
        let rng = extract_range $1 in
          (rng, UTBooleanConstant(true))
      }
  | FALSE {
        let rng = extract_range $1 in
          (rng, UTBooleanConstant(false))
      }
  | LPAREN nxlet RPAREN {
        let (_, utast) = $2 in
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, utast)
      }
  | LPAREN nxlet COMMA tuple RPAREN {
        let rng = make_range (Tok $1) (Tok $5) in
          (rng, UTTupleCons($2, $4))
      }
  | OPENSTR sxsep CLOSESTR {
        let (_, utast) = $2 in
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, utast)
      }
  | OPENQT sxblock CLOSEQT {
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, omit_spaces $2)
      }
  | BLIST ELIST {
        let rng = make_range (Tok $1) (Tok $2) in
          (rng, UTEndOfList)
      }
  | BLIST nxlet nxlist ELIST {
        let rng = make_range (Tok $1) (Tok $4) in
          (rng, UTListCons($2, $3))
      }
  | UNITVALUE {
        let rng = extract_range $1 in
          (rng, UTUnitConstant)
      }
  | FINISH {
        let rng = extract_range $1 in
          (rng, UTFinishHeaderFile)
      }
  | LPAREN binop RPAREN {
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, UTContentOf($2))
  }
/* -- for syntax error log -- */
  | BLIST error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '['" "[ ..<!>.." $1))
      }
  | OPENSTR error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after beginning of string area '{'" "{ ..<!>.." $1))
      }
  | LPAREN error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '('" "( ..<!>.." $1))
      }
;
nxlist:
  | LISTPUNCT nxlet nxlist {
        let rng = make_range (Tok $1) (Untyped $3) in
          (rng, UTListCons($2, $3))
      }
  | { ((-17, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | LISTPUNCT error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after ';'" "; ..<!>.." $1))
      }
;
variants: /* -> untyped_variant_cons */
  | CONSTRUCTOR OF txfunc {
        let (_, constrnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (TypeStr $3) in
          (rng, UTVariantCons(constrnm, $3, ((-400, 0, 0, 0), UTEndOfVariant)))
      }
  | CONSTRUCTOR OF txfunc BAR variants {
        let (_, constrnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (VarntCons $5) in
          (rng, UTVariantCons(constrnm, $3, $5))
      }
;
txfunc: /* -> type_struct */
  | txprod ARROW txfunc {
        let rng = make_range (TypeStr $1) (TypeStr $3) in
          FuncType(rng, $1, $3)
      }
  | txprod { $1 }
;
txprod:
  | txbot TIMES txprod {
        let rng = make_range (TypeStr $1) (TypeStr $3) in
          match $3 with
          | ProductType(_, tylist) -> ProductType(rng, $1 :: tylist)
          | other                  -> ProductType(rng, [$1; $3])
      }
  | txbot VAR {
        let (_, tyschnm) = extract_range_and_name $2 in
        let rng = make_range (TypeStr $1) (TokArg $2) in
          match tyschnm with
          | "list" -> ListType(rng, $1)
          | "ref"  -> RefType(rng, $1)
          | other  -> raise (ParseErrorDetail("undefined type scheme '" ^ other ^ "'"))
      }
  | txbot { $1 }
;
txbot:
  | VAR {
        let ((ln, sttpos, endpos), tynm) = $1 in
        let rng = (ln, sttpos, ln, endpos) in
          match tynm with
          | "int"    -> IntType(rng)
          | "bool"   -> BoolType(rng)
          | "string" -> StringType(rng)
          | "unit"   -> UnitType(rng)
          | other    -> VariantType(rng, other)
      }
  | LPAREN txfunc RPAREN { $2 }
;
tuple: /* -> untyped_tuple_cons */
  | nxlet {
        let (rng, _) = $1 in
          (rng, UTTupleCons($1, ((-5000, 0, 0, 0), UTEndOfTuple)))
      }
  | nxlet COMMA tuple {
        let rng = make_range (Untyped $1) (Untyped $3) in
          (rng, UTTupleCons($1, $3))
      }
/* -- for syntax error log -- */
  | nxlet COMMA error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after ','" ", ..<!>.." $2))
      }
;
pats: /* -> untyped_patter_match_cons */
  | pattr ARROW nxletsub {
        let rng = make_range (Pat $1) (Untyped $3) in
          (rng, UTPatternMatchCons($1, $3, ((-5001, 0, 0, 0), UTEndOfPatternMatch)))
      }
  | pattr ARROW nxletsub BAR pats {
        let rng = make_range (Pat $1) (PatCons $5) in
          (rng, UTPatternMatchCons($1, $3, $5))
      }
  | pattr WHEN nxletsub ARROW nxletsub {
        let rng = make_range (Pat $1) (Untyped $5) in
          (rng, UTPatternMatchConsWhen($1, $3, $5, ((-5001, 0, 0, 0), UTEndOfPatternMatch)))
      }
  | pattr WHEN nxletsub ARROW nxletsub BAR pats {
        let rng = make_range (Pat $1) (PatCons $7) in
          (rng, UTPatternMatchConsWhen($1, $3, $5, $7))
      }
;
pattr: /* -> Types.untyped_pattern_tree */
  | patbot CONS pattr {
        let rng = make_range (Pat $1) (Pat $3) in
          (rng, UTPListCons($1, $3))
      }
  | pattr AS VAR {
        let (_, varnm) = extract_range_and_name $3 in
        let rng = make_range (Pat $1) (TokArg $3) in
          (rng, UTPAsVariable(varnm, $1))
      }
  | patbot { $1 }
;
patbot: /* -> Types.untyped_pattern_tree */
  | NUMCONST {
        let (rng, ncs) = extract_range_and_name $1 in
          (rng, UTPNumericConstant(int_of_string ncs))
      }
  | TRUE {
        let rng = extract_range $1 in
          (rng, UTPBooleanConstant(true))
      }
  | FALSE {
        let rng = extract_range $1 in
          (rng, UTPBooleanConstant(false))
      }
  | UNITVALUE {
        let rng = extract_range $1 in
          (rng, UTPUnitConstant)
      }
  | WILDCARD {
        let rng = extract_range $1 in
          (rng, UTPWildCard)
      }
  | VAR {
        let (rng, varnm) = extract_range_and_name $1 in
          (rng, UTPVariable(varnm))
      }
  | CONSTRUCTOR patbot {
         let (_, constrnm) = extract_range_and_name $1 in
         let rng = make_range (TokArg $1) (Pat $2) in
           (rng, UTPConstructor(constrnm, $2))
      }
  | LPAREN pattr RPAREN {
        let rng = make_range (Tok $1) (Tok $3) in
        let (_, pat) = $2 in
          (rng, pat)
      }
  | LPAREN pattr COMMA pattuple RPAREN {
        let (sttln, sttpos, _) = $1 in
        let (endln, _, endpos) = $5 in
        let rng = (sttln, sttpos, endln, endpos) in
          (rng, UTPTupleCons($2, $4))
      }
  | BLIST ELIST {
        let rng = make_range (Tok $1) (Tok $2) in
          (rng, UTPEndOfList)
      }
  | OPENQT sxblock CLOSEQT {
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, UTPStringConstant(rng, omit_spaces $2))
      }
;
pattuple: /* -> untyped_pattern_tree */
  | pattr {
        let (rng, _) = $1 in
          (rng, UTPTupleCons($1, ((-5002, 0, 0, 0), UTPEndOfTuple)))
      }
  | pattr COMMA pattuple {
        let rng = make_range (Pat $1) (Pat $3) in
          (rng, UTPTupleCons($1, $3))
      }
;
binop:
  | PLUS    { "+" }
  | MINUS   { "-" }
  | MOD     { "mod" }
  | TIMES   { "*" }
  | DIVIDES { "/" }
  | CONCAT  { "^" }
  | EQ      { "==" }
  | NEQ     { "<>" }
  | GEQ     { ">=" }
  | LEQ     { "<=" }
  | GT      { ">" }
  | LT      { "<" }
  | LAND    { "&&" }
  | LOR     { "||" }
  | LNOT    { "not" }
  | BEFORE  { "before" }
;
sxsep:
  | SEP sxsepsub { $2 }
  | sxblock { $1 }
/* -- for syntax error log -- */
  | SEP error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '|'" "| ..<!>.." $1))
      }
;
sxsepsub:
  | sxblock SEP sxsepsub {
        let rng = make_range (Untyped $1) (Untyped $3) in
          (rng, UTListCons($1, $3))
      }
  | { ((-18, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | sxblock SEP error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '|'" "| ..<!>.." $2))
      }
;
sxblock:
  | sxbot sxblock {
        let rng = make_range (Untyped $1) (Untyped $2) in
          (rng, UTConcat($1, $2))
      }
  | { ((-19, 0, 0, 0), UTStringEmpty) }
  ;
sxbot:
  | CHAR {
        let (rng, ch) = extract_range_and_name $1 in
          (rng, UTStringConstant(ch))
      }
  | SPACE {
        let rng = extract_range $1 in
          (rng, UTStringConstant(" "))
      }
  | BREAK {
        let rng = extract_range $1 in
          (rng, UTBreakAndIndent)
      }
  | VARINSTR END {
        let (_, varnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (Tok $2) in
          (rng, UTContentOf(varnm))
      }
  | CTRLSEQ sxclsnm sxidnm narg sarg {
        let (csrng, csnm) = extract_range_and_name $1 in
          convert_into_apply (csrng, UTContentOf(csnm)) $2 $3 (append_argument_list $4 $5)
      }
/* -- for syntax error log -- */
  | CTRLSEQ error {
        let (ln, csnm) = $1 in
          raise (ParseErrorDetail(error_reporting ("illegal token after '" ^ csnm ^ "'") (csnm ^ " ..<!>..") ln))
      }
sxclsnm:
  | CLASSNAME {
        let (rng, clsnm) = extract_range_and_name $1 in
          (rng, class_name_to_abstract_tree clsnm)
      }
  | { ((-20, 0, 0, 0), UTNoContent) }
sxidnm:
  | IDNAME {
        let (rng, idnm) = extract_range_and_name $1 in
          (rng, id_name_to_abstract_tree idnm)
      }
  | { ((-21, 0, 0, 0), UTNoContent) }
;
narg: /* -> Types.untyped_argument_cons */
  | OPENNUM nxlet CLOSENUM narg {
        let (_, utastmain) = $2 in
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, utastmain), $4)
      }
  | { UTEndOfArgument }
/* -- for syntax error log -- */
  | OPENNUM error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after beginning of program '('" "( ..<!>.." $1))
      }
  | OPENNUM nxlet CLOSENUM error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after end of program ')'" ") ..<!>.." $3))
      }
;
sarg: /* -> Types.untyped_argument_cons */
  | BGRP sxsep EGRP sargsub { UTArgumentCons($2, $4) }
  | OPENQT sxblock CLOSEQT sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | END { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
        let (_, utastmain) = $2 in
          UTArgumentCons((rng, utastmain), $4)
      }
  | OPENQT sxblock CLOSEQT sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
