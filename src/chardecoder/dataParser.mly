%{
  open CharBasis
%}

%token EOI
%token<int> CODEPOINT
%token<int * int> CODEPOINTRANGE
%token<string> DATA

%start<(CharBasis.code_point_kind * string) list> main

%%

main:
  | lst=list(element); EOI { lst }
;
element:
  | cp=CODEPOINT; data=DATA         { (CodePoint(cp), data) }
  | cprng=CODEPOINTRANGE; data=DATA { let (cp1, cp2) = cprng in (CodePointRange(cp1, cp2), data) }
;
