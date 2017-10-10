%{
  type code_point = int

  type code_point_kind =
    | CodePoint      of code_point
    | CodePointRange of code_point * code_point

  let empty_map = UCoreLib.UMap.empty ~eq:(=)

  let add_to_map cp scr umap =
    match UCoreLib.UChar.of_int cp with
    | Some(uch_ucore) -> umap |> UCoreLib.UMap.add uch_ucore scr
    | None            -> umap  (* needs reconsideration; maybe should cause an error *)

  let add_range_to_map cp1 cp2 scr umap =
    match (UCoreLib.UChar.of_int cp1, UCoreLib.UChar.of_int cp2) with
    | (Some(uch_ucore1), Some(uch_ucore2)) -> umap |> UCoreLib.UMap.add_range uch_ucore1 uch_ucore2 scr
    | _                                    -> umap  (* needs reconsideration; maybe should canse an error *)

%}

%token EOI
%token<int> CODEPOINT
%token<int * int> CODEPOINTRANGE
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
  | cp=CODEPOINT; scr=IDENTIFIER         { (CodePoint(cp), scr) }
  | cprng=CODEPOINTRANGE; scr=IDENTIFIER { let (cp1, cp2) = cprng in (CodePointRange(cp1, cp2), scr) }
;
