%{
  type code_point = string

  type code_point_kind =
    | CodePoint      of code_point
    | CodePointRange of code_point * code_point

  let empty_map = UCoreLib.UMap.empty

  let add_to_map = UCoreLib.UMap.add

  let add_range_to_map = UCoreLib.UMap.add_range

%}

%token EOI COMMENT DOTS SEMICOLON
%token<code_point> CODEPOINT
%token<string> IDENTIFIER
%start<string UCoreLib.UMap.t> main

%%

main:
  | lst=list(element); EOI {
        lst |> List.fold_left (fun accmap elem ->
          match elem with
          | (CodePoint(cp), scr)            -> accmap |> add_to_map cp scr
          | (CodePointRange(cp1, cp2), scr) -> accmap |> add_range_to_map cp1 cp2 scr
        ) empty_map
      }
;
element:
  | cp=CODEPOINT; SEMICOLON; scr=IDENTIFIER            { (CodePoint(cp), scr) }
  | cp1=CODEPOINT; DOTS; cp2=CODEPOINT; scr=IDENTIFIER { (CodePointRange(cp1, cp2), scr) }
;
