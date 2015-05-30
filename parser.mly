%{
  open Types
%}
%token <Types.var_name> NUMVAR
%token <Types.var_name> STRVAR
/*
%token <string> CONSTNUM
%token <string> CTRLSEQ
%token <string> IDNAME
%token <string> CLASSNAME
*/
%token LET IN DEFEQ
/*
%token IF THEN ELSE
*/
%token EOI
%token LPAREN RPAREN
%token UMINUS
%token TIMES DIVIDES
%token MOD
%token PLUS MINUS
%token EQ NEQ GEQ LEQ GT LT
%token LNOT
%token LAND
%token LOR

%nonassoc DEFEQ IN
%right LOR
%right LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%left PLUS MINUS
%left MOD
%left TIMES DIVIDES
%nonassoc UMINUS
%nonassoc LPAREN NUMVAR LET

%start main
%type <Types.abstract_tree> main
%type <Types.abstract_tree> expr

%%

main:
  | expr EOI { $1 }
expr:
  | NUMVAR { NumericContentOf($1) }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { Plus($1, $3) }
  | expr MINUS expr { Minus($1, $3) }
  | expr MOD expr { Mod($1, $3) }
  | expr TIMES expr { Times($1, $3) }
  | expr DIVIDES expr { Divides($1, $3) }
  | UMINUS expr { Minus(NumericConst(0), $2) }
  | expr EQ expr { EqualTo($1, $3) }
  | expr NEQ expr { LogicalNot(EqualTo($1, $3)) }
  | expr GEQ expr { LogicalNot(LessThan($1, $3)) }
  | expr LEQ expr { LogicalNot(GreaterThan($1, $3)) }
  | expr GT expr { GreaterThan($1, $3) }
  | expr LT expr { LessThan($1, $3) }
  | LNOT expr { LogicalNot($2) }
  | expr LAND expr { LogicalAnd($1, $3) }
  | expr LOR expr { LogicalOr($1, $3) }
  | LET NUMVAR DEFEQ expr IN expr { LetNumIn($2, $4, $6) }
  | LET STRVAR DEFEQ expr IN expr { LetStrIn($2, $4, $6) }
/*
  | IF expr THEN expr ELSE expr { IfThenElse($2, $4, $6) }
*/
;
