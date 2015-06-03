%{
  open Types
%}
%token <Types.var_name> NUMVAR
%token <Types.var_name> STRVAR
%token <string> NUMCONST
%token <string> CHAR
%token <string> CTRLSEQ
%token <string> IDNAME
%token <string> CLASSNAME
%token END
%token LET IN DEFEQ
%token IF THEN ELSE
%token EOI
%token LPAREN RPAREN
%token TIMES DIVIDES
%token MOD
%token PLUS MINUS
%token EQ NEQ GEQ LEQ GT LT
%token LNOT
%token LAND
%token LOR
%token CONCAT
%token OPENQT CLOSEQT
%token OPENSTR CLOSESTR
%token OPENNUM CLOSENUM
%token BGRP EGRP
/*
%nonassoc DEFEQ IN
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%left PLUS MINUS
%left MOD
%left TIMES DIVIDES
%nonassoc NUMVAR
%nonassoc IF THEN ELSE
%nonassoc LET
%nonassoc LPAREN
*/

%start main
%type <Types.abstract_tree> main
%type <Types.abstract_tree> nxlor
%type <Types.abstract_tree> nxland
%type <Types.abstract_tree> nxcomp
%type <Types.abstract_tree> nxconcat
%type <Types.abstract_tree> nxplus
%type <Types.abstract_tree> nxtimes
%type <Types.abstract_tree> nxun
%type <Types.abstract_tree> nxapp
%type <Types.abstract_tree> nxbot
%type <Types.abstract_tree> sblock
%type <Types.abstract_tree> sbot
%type <Types.abstract_tree> narg
%type <Types.abstract_tree> sarg
%type <Types.abstract_tree> sargsub

%%

main:
  | nxlet EOI { $1 }
nxlet:
  | LET NUMVAR DEFEQ nxlet IN nxlet { LetNumIn($2, $4, $6) }
  | LET STRVAR DEFEQ nxlet IN nxlet { LetStrIn($2, $4, $6) }
  | nxif { $1 }
nxif:
  | IF nxif THEN nxif ELSE nxif { IfThenElse($2, $4, $6) }
  | nxlor { $1 }
nxlor:
  | nxland LOR nxlor { LogicalOr($1, $3) }
  | nxland { $1 }
nxland:
  | nxcomp LAND nxland { LogicalAnd($1, $3) }
  | nxcomp { $1 }
nxcomp:
  | nxconcat EQ nxcomp { EqualTo($1, $3) }
  | nxconcat NEQ nxcomp { LogicalNot(EqualTo($1, $3)) }
  | nxconcat GEQ nxcomp { LogicalNot(LessThan($1, $3)) }
  | nxconcat LEQ nxcomp { LogicalNot(GreaterThan($1, $3)) }
  | nxconcat GT nxcomp { GreaterThan($1, $3) }
  | nxconcat LT nxcomp { LessThan($1, $3) }
  | nxconcat { $1 }
nxconcat:
  | nxplus CONCAT nxconcat { Concat($1, $3) }
  | nxplus { $1 }
nxplus:
  | nxtimes PLUS nxrplus { Plus($1, $3) }
  | nxplus MINUS nxrtimes { Minus($1, $3) }
  | nxtimes { $1 }
nxrplus:
  | nxrtimes PLUS nxrplus { Plus($1, $3) }
  | nxrplus MINUS nxrtimes { Minus($1, $3) }
  | nxrtimes { $1 }
nxtimes:
  | nxun TIMES nxrtimes { Times($1, $3) }
  | nxtimes DIVIDES nxapp { Divides($1, $3) }
  | nxtimes MOD nxapp { Mod($1, $3) }
  | nxun { $1 }
nxrtimes:
  | nxapp TIMES nxrtimes { Times($1, $3) }
  | nxrtimes DIVIDES nxapp { Divides($1, $3) }
  | nxrtimes MOD nxapp { Mod($1, $3) }
  | nxapp { $1 }
nxun:
  | MINUS nxapp { Minus(NumericConstant(0), $2) }
  | LNOT nxapp { LogicalNot($2) }
  | nxapp { $1 }
nxapp:
  | nxapp nxbot { Apply($1, $2) }
  | nxbot { $1 }
nxbot:
  | NUMVAR { NumericContentOf($1) }
  | NUMCONST { NumericConstant(int_of_string $1) }
  | LPAREN nxlet RPAREN { $2 }
  | OPENSTR sblock CLOSESTR { $2 }
  | OPENQT sblock CLOSEQT { $2 }
sblock:
  | sbot sblock { Concat($1, $2) }
  | { StringEmpty }
sbot:
  | CHAR { StringConstant($1) }
  | STRVAR END { StringContentOf($1) }
  | CTRLSEQ narg sarg { ControlSequence($1, NoClass, NoID, $2, $3) }
  | CTRLSEQ CLASSNAME narg sarg { ControlSequence($1, $2, NoID, $3, $4) }
  | CTRLSEQ IDNAME narg sarg { ControlSequence($1, NoClass, $2, $3, $4) }
  | CTRLSEQ CLASSNAME IDNAME narg sarg { ControlSequence($1, $2, $3, $4, $5) }
narg:
  | OPENNUM nxlet CLOSENUM narg { NumericArgument($2, $4) }
  | { NumericEmpty }
sarg:
  | BGRP sblock EGRP sargsub { StringArgument($2, $4) }
  | OPENQT sblock CLOSEQT sargsub { StringArgument($2, $4) }
  | END { StringEmpty }
sargsub:
  | BGRP sblock EGRP sargsub { StringArgument($2, $4) }
  | OPENQT sblock CLOSEQT sargsub { StringArgument($2, $4) }
  | { StringEmpty }
  ;
