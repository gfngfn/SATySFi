%{
  open Types

  type literal_reading_state = Normal | ReadingSpace
  exception MyParseError of string

  let rec append_argument_list arglsta arglstb =
    match arglsta with
    | EndOfArgument -> arglstb
    | ArgumentCons(arg, arglstl) ->
        ArgumentCons(arg, (append_argument_list arglstl arglstb))

  (* ctrlseq_name -> abstract_tree -> abstract_tree -> argument_cons -> abstract_tree *)
  let rec convert_into_numeric_apply csnm clsnmast idnmast argcons =
    convert_into_numeric_apply_sub argcons (ApplyClassAndID(clsnmast, idnmast, ContentOf(csnm)))
  
  (* argument_cons -> abstract_tree -> abstract_tree *)
  and convert_into_numeric_apply_sub argcons astconstr =
    match argcons with
    | EndOfArgument -> astconstr
    | ArgumentCons(arg, actail) ->
        convert_into_numeric_apply_sub actail (Apply(astconstr, arg))

  let class_name_to_abstract_tree clsnm =
    StringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  let id_name_to_abstract_tree idnm =
    StringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  let rec curry_lambda_abstract argvarcons astdef =
    match argvarcons with
    | EndOfArgumentVariable -> astdef
    | ArgumentVariableCons(argvar, avtail) -> 
        LambdaAbstract(argvar, curry_lambda_abstract avtail astdef)

  let error_reporting msg disp pos =
    let (pos_ln, pos_start, pos_end) = pos in
      "Syntax error: " ^ msg ^ ".\n\n    " ^ disp
        ^ "\n\n  (at line " ^ (string_of_int pos_ln) ^ ", "
        ^ (string_of_int pos_start) ^ "-" ^ (string_of_int pos_end) ^ ")"

  let rec string_of_avc argvarcons =
    match argvarcons with
    | EndOfArgumentVariable -> ""
    | ArgumentVariableCons(argvar, avtail) -> argvar ^ " " ^ (string_of_avc avtail)


  let rec stringify_literal chofltrl =
    match chofltrl with
    | Concat(astf, astl) -> (stringify_literal astf) ^ (stringify_literal astl)
    | StringConstant(s)  -> s
    | StringEmpty        -> ""
    | _  -> raise (ParseErrorDetail("illegal token in literal area; this cannot happen"))

  (* abstract_tree -> abstract_tree *)
  and omit_spaces chofltrl =
    let str_ltrl = stringify_literal chofltrl in
      let min_indent = min_indent_space str_ltrl in
        let str_shaved = shave_indent str_ltrl min_indent in
          if str_shaved.[(String.length str_shaved) - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 ((String.length str_shaved) - 1) in
              Concat(StringConstant(str_no_last_break), BreakAndIndent)
          else
            StringConstant(str_shaved)

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

%}
%token <Types.token_position * Types.var_name> VAR
%token <Types.token_position * Types.var_name> VARINSTR
%token <Types.token_position * string> NUMCONST
%token <Types.token_position * string> CHAR
%token SPACE
%token BREAK
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
%token <Types.token_position> BLIST LISTPUNCT ELIST
%token <Types.token_position> BEFORE
%token <Types.token_position> UNITVALUE
%token <Types.token_position> WHILE DO
%token EOI
%token IGNORED

%nonassoc LET DEFEQ IN LETAND LETMUTABLE OVERWRITEEQ
%nonassoc IF THEN ELSE
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
%type <Types.abstract_tree> main
%type <Types.abstract_tree> nxlet
%type <Types.mutual_let_cons> nxdec
%type <Types.abstract_tree> nxbfr
%type <Types.abstract_tree> nxwhl
%type <Types.abstract_tree> nxif
%type <Types.abstract_tree> nxlor
%type <Types.abstract_tree> nxland
%type <Types.abstract_tree> nxcomp
%type <Types.abstract_tree> nxconcat
%type <Types.abstract_tree> nxlplus
%type <Types.abstract_tree> nxltimes
%type <Types.abstract_tree> nxrplus
%type <Types.abstract_tree> nxrtimes
%type <Types.abstract_tree> nxun
%type <Types.abstract_tree> nxapp
%type <Types.abstract_tree> nxbot
%type <Types.abstract_tree> nxlist
%type <Types.abstract_tree> sxsep
%type <Types.abstract_tree> sxsepsub
%type <Types.abstract_tree> sxblock
%type <Types.abstract_tree> sxbot
%type <Types.abstract_tree> sxclsnm
%type <Types.abstract_tree> sxidnm
%type <Types.argument_cons> narg
%type <Types.argument_cons> sarg
%type <Types.argument_cons> sargsub
%type <Types.argument_variable_cons> argvar

%%

main:
  | nxlet EOI { $1 }
  | sxblock EOI { $1 }
;
nxlet:
  | LET VAR argvar DEFEQ nxlet nxdec nxlet {
        let (_, vn) = $2 in
        let curried = curry_lambda_abstract $3 $5 in
          Types.LetIn(Types.MutualLetCons(vn, curried, $6), $7)
      }
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec nxlet {
        let (_, csname) = $2 in
        let curried = curry_lambda_abstract $3 $5 in
          Types.LetIn(Types.MutualLetCons(csname, curried, $6), $7)
      }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet {
        let (_, vn) = $2 in
          Types.LetMutableIn(vn, $4, $6)
      }
  | nxwhl { $1 }
/* -- for syntax error log -- */
  | LET error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'let'" "let ..<!>.." $1))
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
  | LET VAR argvar DEFEQ nxlet nxdec error {
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
  | LET CTRLSEQ argvar DEFEQ nxlet nxdec error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
            ("let " ^ csname ^ " " ^ (string_of_avc $3) ^ "= ... in ..<!>..") ln))
      }
;
nxdec:
  | LETAND VAR argvar DEFEQ nxlet nxdec {
        let (_, vn) = $2 in
        let curried = curry_lambda_abstract $3 $5 in
          Types.MutualLetCons(vn, curried, $6)
      }
  | LETAND CTRLSEQ argvar DEFEQ nxlet nxdec {
        let (_, csname) = $2 in
        let curried = curry_lambda_abstract $3 $5 in
          Types.MutualLetCons(csname, curried, $6)
      }
  | IN { Types.EndOfMutualLet }
/* -- for syntax error log -- */
  | LETAND VAR error {
        let (_, vn) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ vn ^ " ..<!>..") $1))
      }
  | LETAND CTRLSEQ error {
        let (ln, csname) = $2 in
          raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
            ("and " ^ csname ^ " ..<!>..") ln))
      }
;
nxwhl:
  | WHILE nxlet DO nxwhl { Types.WhileDo($2, $4) }
  | nxif { $1 }
/* -- for syntax error log --*/
  | WHILE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'while'" "while ..<!>.." $1))
      }
  | WHILE nxlet DO error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'do'" "do ..<!>.." $3))
      }
nxif:
  | IF nxlet THEN nxlet ELSE nxlet       { Types.IfThenElse($2, $4, $6) }
  | IFCLASSISVALID nxlet ELSE nxlet      { Types.IfClassIsValid($2, $4) }
  | IFCLASSISVALID THEN nxlet ELSE nxlet { Types.IfClassIsValid($3, $5) }
  | IFIDISVALID nxlet ELSE nxlet         { Types.IfIDIsValid($2, $4) }
  | IFIDISVALID THEN nxlet ELSE nxlet    { Types.IfIDIsValid($3, $5) }
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
  | nxlambda BEFORE nxbfr { Types.Sequential($1, $3) }
  | nxlambda { $1 }
/* -- for syntax error log -- */
  | nxlambda BEFORE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'before'" "before ..<!>.." $2))
      }
;
nxlambda:
  | VAR OVERWRITEEQ nxlor { let (_, vn) = $1 in Types.Overwrite(vn, $3) }
  | LAMBDA argvar ARROW nxlor { curry_lambda_abstract $2 $4 }
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
;
argvar:
  | VAR argvar { let (_, vn) = $1 in Types.ArgumentVariableCons(vn, $2) }
  | { Types.EndOfArgumentVariable }
;
nxlor:
  | nxland LOR nxlor { Types.Apply(Types.Apply(ContentOf("||"), $1), $3) }
  | nxland { $1 }
/* -- for syntax error log -- */
  | nxland LOR error {
        raise (ParseErrorDetail(error_reporting "illegal token after '||'" "|| ..<!>.." $2))
      }
;
nxland:
  | nxcomp LAND nxland { Types.Apply(Types.Apply(ContentOf("&&"), $1), $3) }
  | nxcomp { $1 }
/* -- for syntax error log -- */
  | nxcomp LAND error {
        raise (ParseErrorDetail(error_reporting "illegal token after '&&'" "&& ..<!>.." $2))
      }
;
nxcomp:
  | nxconcat EQ nxcomp  { Types.Apply(Types.Apply(ContentOf("=="), $1), $3) }
  | nxconcat NEQ nxcomp { Types.Apply(Types.Apply(ContentOf("<>"), $1), $3) }
  | nxconcat GEQ nxcomp { Types.Apply(Types.Apply(ContentOf(">="), $1), $3) }
  | nxconcat LEQ nxcomp { Types.Apply(Types.Apply(ContentOf("<="), $1), $3) }
  | nxconcat GT nxcomp  { Types.Apply(Types.Apply(ContentOf(">"), $1), $3) }
  | nxconcat LT nxcomp  { Types.Apply(Types.Apply(ContentOf("<"), $1), $3) }
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
  | nxlplus CONCAT nxconcat { Types.Apply(Types.Apply(ContentOf("^"), $1), $3) }
  | nxlplus { $1 }
/* -- for syntax error log -- */
  | nxlplus CONCAT error {
        raise (ParseErrorDetail(error_reporting "illegal token after '^'" "^ ..<!>.." $2))
      }
;
nxlplus:
  | nxlminus PLUS nxrplus { Types.Apply(Types.Apply(ContentOf("+"), $1), $3) }
  | nxlminus { $1 }
/* -- for syntax error log -- */
  | nxlminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxlminus:
  | nxlplus MINUS nxrtimes { Types.Apply(Types.Apply(ContentOf("-"), $1), $3) }
  | nxltimes { $1 }
/* -- for syntax error log -- */
  | nxlplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxrplus:
  | nxrminus PLUS nxrplus { Types.Apply(Types.Apply(ContentOf("+"), $1), $3) }
  | nxrminus { $1 }
/* -- for syntax error log -- */
  | nxrminus PLUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '+'" "+ ..<!>.." $2))
      }
;
nxrminus:
  | nxrplus MINUS nxrtimes { Types.Apply(Types.Apply(ContentOf("-"), $1), $3) }
  | nxrtimes { $1 }
/* -- for syntax error log -- */
  | nxrplus MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after '-'" "- ..<!>.." $2))
      }
;
nxltimes:
  | nxun TIMES nxrtimes    { Types.Apply(Types.Apply(ContentOf("*"), $1), $3) }
  | nxltimes DIVIDES nxapp { Types.Apply(Types.Apply(ContentOf("/"), $1), $3) }
  | nxltimes MOD nxapp     { Types.Apply(Types.Apply(ContentOf("mod"), $1), $3) }
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
  | nxapp TIMES nxrtimes   { Types.Apply(Types.Apply(ContentOf("*"), $1), $3) }
  | nxrtimes DIVIDES nxapp { Types.Apply(Types.Apply(ContentOf("/"), $1), $3) }
  | nxrtimes MOD nxapp     { Types.Apply(Types.Apply(ContentOf("mod"), $1), $3) }
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
  | MINUS nxapp { Types.Apply(Types.Apply(ContentOf("-"), NumericConstant(0)), $2) }
  | LNOT nxapp  { Types.Apply(ContentOf("not"), $2) }
  | nxapp { $1 }
/* -- for syntax error log -- */
  | MINUS error {
        raise (ParseErrorDetail(error_reporting "illegal token after unary '-'" "- ..<!>.." $1))
      }
  | LNOT error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'not'" "not ..<!>.." $1))
      }
;
nxapp:
  | nxapp nxbot { Types.Apply($1, $2) }
  | nxbot { $1 }
;
nxbot:
  | VAR      { let (_, vn) = $1 in Types.ContentOf(vn) }
  | NUMCONST { let (_, cs) = $1 in Types.NumericConstant(int_of_string cs) }
  | TRUE  { Types.BooleanConstant(true) }
  | FALSE { Types.BooleanConstant(false) }
  | LPAREN nxlet RPAREN    { $2 }
  | OPENSTR sxsep CLOSESTR { $2 }
  | OPENQT sxsep CLOSEQT   { omit_spaces $2 }
  | BLIST ELIST              { Types.EndOfList }
  | BLIST nxlet nxlist ELIST { Types.ListCons($2, $3) }
  | REFNOW VAR   { let (_, vn) = $2 in Types.Reference(vn) }
  | REFFINAL VAR { let (_, vn) = $2 in Types.ReferenceFinal(vn) }
  | UNITVALUE    { Types.UnitConstant }
  | FINISH       { Types.FinishHeaderFile }
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
  | LISTPUNCT nxlet nxlist { Types.ListCons($2, $3) }
  | { Types.EndOfList }
/* -- for syntax error log -- */
  | LISTPUNCT error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after ';'" "; ..<!>.." $1))
      }
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
  | sxblock SEP sxsepsub { ListCons($1, $3) }
  | { EndOfList }
/* -- for syntax error log -- */
  | sxblock SEP error {
        raise (ParseErrorDetail(
          error_reporting "illegal token after '|'" "| ..<!>.." $2))
      }
;
sxblock:
  | sxbot sxblock { Types.Concat($1, $2) }
  | { Types.StringEmpty }
  ;
sxbot:
  | CHAR  { let (_, ch) = $1 in Types.StringConstant(ch) }
  | SPACE { Types.StringConstant(" ") }
  | BREAK { Types.BreakAndIndent }
  | VARINSTR END { let (_, vn) = $1 in Types.ContentOf(vn) }
  | CTRLSEQ sxclsnm sxidnm narg sarg {
        let (_, csname) = $1 in
          convert_into_numeric_apply csname $2 $3 (append_argument_list $4 $5)
      }
/* -- for syntax error log -- */
  | CTRLSEQ error {
        let (ln, csname) = $1 in
        raise (ParseErrorDetail(error_reporting ("illegal token after '" ^ csname ^ "'") (csname ^ " ..<!>..") ln))
      }
sxclsnm:
  | CLASSNAME { let (_, clsnm) = $1 in class_name_to_abstract_tree clsnm }
  | { NoContent }
sxidnm:
  | IDNAME { let (_, idnm) = $1 in id_name_to_abstract_tree idnm }
  | { NoContent }
;
narg: /* -> Types.argument_cons */
  | OPENNUM nxlet CLOSENUM narg { Types.ArgumentCons($2, $4) }
  | { Types.EndOfArgument }
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
sarg: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub { Types.ArgumentCons($2, $4) }
  | OPENQT sxsep CLOSEQT sargsub { Types.ArgumentCons(omit_spaces $2, $4) }
  | END { Types.EndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub { Types.ArgumentCons($2, $4) }
  | OPENQT sxsep CLOSEQT sargsub { Types.ArgumentCons(omit_spaces $2, $4) }
  | { Types.EndOfArgument }
/* -- for syntax error log */
  | BGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '{'" "{ ..<!>.." $1))
      }
  | BGRP sxsep EGRP error {
        raise (ParseErrorDetail(error_reporting "illegal token after '}'" "} ..<!>.." $3))
      }
;
