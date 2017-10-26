
open CharBasis

exception InputFileBroken


let class_of_string s =
  match s with
  | "CM"  -> CM
  | "SG"  -> SG
  | "WJ"  -> WJ
  | "ZW"  -> ZW
  | "GL"  -> GL
  | "SP"  -> SP
  | "ZWJ" -> ZWJ
  | "B2"  -> B2
  | "BA"  -> BA
  | "BB"  -> BB
  | "HY"  -> HY
  | "CB"  -> CB
  | "CL"  -> CL
  | "CP"  -> CP
  | "EX"  -> EX
  | "IN"  -> IN
  | "NS"  -> NS
  | "OP"  -> OP
  | "QU"  -> QU  (* maybe not necessary *)
  | "IS"  -> IS
  | "NU"  -> NU
  | "PO"  -> PO
  | "PR"  -> PR
  | "SY"  -> SY
  | "AI"  -> AI
  | "AL"  -> AL
  | "CJ"  -> CJ
  | "EB"  -> EB  (* maybe not necessary *)
  | "EM"  -> EM  (* maybe not necessary *)
  | "H2"  -> H2
  | "H3"  -> H3
  | "HL"  -> HL
  | "ID"  -> ID
  | "JL"  -> JL
  | "JV"  -> JV
  | "JT"  -> JT
  | "RI"  -> RI
  | "SA"  -> SA
  | "XX"  -> XX
  | "BK"  -> INBR
  | "CR"  -> INBR
  | "LF"  -> INBR
  | "NL"  -> INBR
  | _     -> raise InputFileBroken


(* temporary; should depend on the language of the context *)
let line_break_class_overriding_list =
  [
  (* -- U+3000..U+30FF -- *)
    (0x3001, IDNS);  (* IDEOGRAPHIC COMMA                 : ideographic nonstarter           *)
    (0x3002, IDNS);  (* IDEOGRAPHIC FULL STOP             : ideographic nonstarter           *)
    (0x3008, IDOP);  (* LEFT ANGLE BRACKET                : ideographic open punctuation     *)
    (0x3009, IDCP);  (* RIGHT ANGLE BRACKET               : ideographic close punctuation    *)
    (0x300A, IDOP);  (* LEFT DOUBLE ANGLE BRACKET         : ideographic open punctuation     *)
    (0x300B, IDCP);  (* RIGHT DOUBLE ANGLE BRACKET        : ideographic close punctuation    *)
    (0x300C, IDOP);  (* LEFT CORNER BRACKET               : ideographic open punctuation     *)
    (0x300D, IDCP);  (* RIGHT CORNER BRACKET              : ideographic close punctuation    *)
    (0x300E, IDOP);  (* LEFT WHITE CORNER BRACKET         : ideographic open punctuation     *)
    (0x300F, IDCP);  (* RIGHT WHITE CORNER BRACKET        : ideographic close punctuation    *)
    (0x3010, IDOP);  (* LEFT BLACK LENTICULAR BRACKET     : ideographic open punctuation     *)
    (0x3011, IDCP);  (* RIGHT BLACK LENTICULAR BRACKET    : ideographic close punctuation    *)
    (0x3014, IDOP);  (* LEFT TORTOISE SHELL BRACKET       : ideographic open punctuation     *)
    (0x3015, IDCP);  (* RIGHT TORTOISE SHELL BRACKET      : ideographic close punctuation    *)
    (0x3016, IDOP);  (* LEFT WHITE LENTICULAR BRACKET     : ideographic open punctuation     *)
    (0x3017, IDCP);  (* RIGHT WHITE LENTICULAR BRACKET    : ideographic close punctuation    *)
    (0x3018, IDOP);  (* LEFT WHITE TORTOISE SHELL BRACKET : ideographic open punctuation     *)
    (0x3019, IDCP);  (* RIGHT WHITE TORTOISE SHELL BRACKET: ideographic close punctuation    *)
    (0x301A, IDOP);  (* LEFT WHITE SQUARE BRACKET         : ideographic open punctuation     *)
    (0x301B, IDCP);  (* RIGHT WHITE SQUARE BRACKET        : ideographic close punctuation    *)
    (0xFF08, IDOP);  (* FULLWIDTH LEFT PARENTHESIS        : ideographic open punctuation     *)
    (0xFF09, IDCP);  (* FULLWIDTH RIGHT PARENTHESIS       : ideographic close punctuation    *)
    (0xFF0C, IDNS);  (* FULLWIDTH COMMA                   : ideographic nonstarter           *)
    (0xFF0E, IDNS);  (* FULLWIDTH FULL STOP               : ideographic nonstarter           *)
    (0xFF3B, IDOP);  (* FULLWIDTH LEFT SQUARE BRACKET     : ideographic open punctuation     *)
    (0xFF3D, IDCP);  (* FULLWIDTH RIGHT SQUARE BRACKET    : ideographic close punctuation    *)
    (0xFF5B, IDOP);  (* FULLWIDTH LEFT CURLY BRACKET      : ideographic open punctuation     *)
    (0xFF5D, IDCP);  (* FULLWIDTH RIGHT CURLY BRACKET     : ideographic close punctuation    *)
    (0xFF5F, IDOP);  (* FULLWIDTH LEFT WHITE PARENTHESIS  : ideographic open punctuation     *)
    (0xFF60, IDCP);  (* FULLWIDTH RIGHT WHITE PARENTHESIS : ideographic close punctuation    *)
  ]


let line_break_map_ref : (line_break_class UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let line_break_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let line_break_map_raw = line_break_list |> CharBasis.map_of_list class_of_string in
  let line_break_map =
    List.fold_left (fun mapacc (cp, lbc) ->
      match UCoreLib.UChar.of_int cp with
      | None            -> mapacc  (* needs reconsideration; maybe should warn emptyness *)
      | Some(uch_ucore) -> mapacc |> UCoreLib.UMap.add uch_ucore lbc
    ) line_break_map_raw line_break_class_overriding_list
  in
  begin
    line_break_map_ref := line_break_map;
  end


let find uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> XX  (* temporary *)
  | Some(uch_ucore) ->
      match (!line_break_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None      -> XX  (* temporary *)
      | Some(lbc) -> lbc


(* --
  append line break class to uchar, and then eliminate every INBR character
  if it is adjacent to a character of a nonspacing script (e.g. han ideographic, kana, etc.)
-- *)
(*
let append_property (uchlst : Uchar.t list) =

  let is_in_nonspacing_class (_, lbc) =
    match lbc with
    | ( ID | CJ | IN | SA | IDNS | IDOP | IDCP ) -> true
    | _                                          -> false
  in

  let bispace = (Uchar.of_int 32, SP) in  (* -- space character -- *)

  let rec trim_break prevopt bilst =
    let aux = trim_break in
    match bilst with
    | [] ->
        begin
          match prevopt with
          | None                     -> []
          | Some(((_, INBR), biacc)) -> List.rev (bispace :: biacc)
                                          (* temporary; should take the adjacent embedded command into consideration *)
          | Some((biprev, biacc))    -> List.rev (biprev :: biacc)
        end

    | (_, INBR) :: bitail ->
        begin
          match prevopt with
          | None ->
              aux (Some((bispace, []))) bitail
                (* -- replaces the forefront INBR character with a space -- *)
          | Some((biprev, biacc)) ->
              if is_in_nonspacing_class biprev then
                aux prevopt bitail
                  (* -- ignores a INBR character after a character of a nonspacing class -- *)
              else
                aux (Some((bispace, biprev :: biacc))) bitail
                  (* -- replaces the INBR character with a space -- *)
        end

    | bihead :: bitail ->
        begin
          match prevopt with
          | None ->
              aux (Some(bihead, [])) bitail

          | Some(((_, INBR), biacc)) ->
              if is_in_nonspacing_class bihead then
                aux (Some(bihead, biacc)) bitail
                  (* -- ignores a INBR character before a character of a nonspacing class -- *)
              else
                aux (Some(bihead, bispace :: biacc)) bitail
                  (* -- replaces the INBR character with a space -- *)

          | Some((biprev, biacc)) -> aux (Some((bihead, biprev :: biacc))) bitail
        end
  in

  let bilst = uchlst |> List.map (fun uch -> (uch, find uch)) in
    trim_break None bilst
*)

let set lbclst = LBRESet(lbclst)
let notof lbclst = LBRENotOf(lbclst)
let exact lbc = set [lbc]
let star lbre = LBREStar(lbre)
let spaced = set [AL]
let nonspaced = set [ID; CJ; IN; SA; IDNS; IDOP; IDCP]


let bispace = (Uchar.of_int 32, SP)  (* -- space character -- *)


(* -- the rules for normalizing texts about spaces, break letters, etc. -- *)
let normalization_rule =
  [
  (* -- ignore spaces or break letters *)
    ([nonspaced; set [SP; INBR]], [], [spaced]);
    ([spaced; set [SP; INBR]], [], [nonspaced]);
  (* -- ignore break letters between nonspaced characters -- *)
    ([nonspaced; exact INBR], [], [nonspaced]);
    ([exact INBR], [bispace], []);
  ]


(* -- the rules for inserting line break opportunities based on [UAX#14 Section 6] -- *)
let line_break_rule =
    [
    (* -- LB7 -- *)
      ([], PreventBreak, [exact SP]);
      ([], PreventBreak, [exact ZW]);
    (* -- LB8 -- *)
      ([exact ZW; star [exact SP]], AllowBreak, []);
    (* -- LB8a -- *)
      ([exact ZWJ], PreventBreak, [set [ID; EB; EM]]);
    (* -- LB11 -- *)
      ([], PreventBreak, [exact WJ]);
      ([exact WJ], PreventBreak, []);
    (* -- LB12 -- *)
      ([exact GL], PreventBreak, []);
    (* -- LB12a -- *)
      ([notof [SP; BA; HY]], PreventBreak, [exact GL]);
    (* -- LB13 -- *)
    (* -- Original: ideographic close punctuations -- *)
      ([], PreventBreak, [set [CL; CP; EX; IS; SY; IDCP]]);
    (* -- LB14 -- *)
    (* -- Original: ideographic open punctuations -- *)
      ([set [OP; IDOP]; star [exact SP]], PreventBreak, []);
    (* -- LB15 -- *)
      ([exact QU; star [exact SP]], PreventBreak, [exact OP]);
    (* -- LB16 -- *)
    (* -- Original: ideographic close punctuations -- *)
      ([set [CL; CP]; star [exact SP]], PreventBreak, [set [NS; CJ; IDNS]]);
    (* -- LB17 -- *)
      ([exact B2; star [exact SP]], PreventBreak, [exact B2]);
    (* -- LB18 -- *)
      ([exact SP], AllowBreak, []);
    (* -- LB19 -- *)
      ([], PreventBreak, [exact QU]);
      ([exact QU], PreventBreak, []);
    (* -- LB20 -- *)
      ([], AllowBreak, [exact CB]);
      ([exact CB], AllowBreak, []);
    (* -- LB21 -- *)
    (* -- Original: ideographic close punctuation -- *)
      ([], PreventBreak, [set [BA; HY; NS; CJ; IDNS]]);
      ([exact BB], PreventBreak, []);
    (* -- LB21a -- *)
      ([exact HL; set [HY; BA]], PreventBreak, []);
    (* -- LB21b -- *)
      ([exact SY], PreventBreak, [exact HL]);
    (* -- LB22 -- *)
      ([set [AL; HL; EX; ID; EB; EM; IN; NU]], PreventBreak, [exact NU]);
    (* -- LB23 -- *)
      ([set [AL; HL]], PreventBreak, [exact NU]);
      ([exact NU], PreventBreak, [set [AL; HL]]);
    (* -- LB23a -- *)
      ([exact PR], PreventBreak, [set [ID; EB; EM]]);
      ([set [ID; EB; EM]], PreventBreak, [exact PO]);
    (* -- LB24 -- *)
      ([set [PR; PO]], PreventBreak, [set [AL; HL]]);
      ([set [AL; HL]], PreventBreak, [set [PR; PO]]);
    (* -- LB25 -- *)
      ([set [CL; CP; NU]], PreventBreak, [set [PR; PO]]);
      ([set [PR; PO]], PreventBreak, [set [OP; NU]]);
      ([set [HY; IS; NU; SY]], PreventBreak, [exact NU]);
    (* -- LB26 -- *)
      ([exact JL], PreventBreak, [set [JL; JV; H2; H3]]);
      ([set [JV; H2]], PreventBreak, [set [JV; JT]]);
      ([set [JT; H3]], PreventBreak, [exact JT]);
    (* -- LB27 -- *)
      ([set [JL; JV; JT; H2; H3]], PreventBreak, [set [IN; PO]]);
      ([exact PR], PreventBreak, [set [JL; JV; JT; H2; H3]]);
    (* -- LB28 --*)
      ([set [AL; HL]], PreventBreak, [set [AL; HL]]);
    (* -- LB29 --*)
      ([exact IS], PreventBreak, [set [AL; HL]]);
    (* -- LB30 --*)
      ([set [AL; HL; NU]], PreventBreak, [exact OP]);
      ([exact CP], PreventBreak, [set [AL; HL; NU]]);
    (* -- LB30a -- *)  (* temporary; incomplete rule *)
      ([notof [RI]; star [exact RI; exact RI]; exact RI], PreventBreak, [exact RI]);
    (* -- LB30b -- *)
      ([exact EB], PreventBreak, [exact EM]);
    ]


(* -- a naive, greedy regexp matching (it suffices to use greedy one) -- *)
let match_prefix (type a) (getf : a -> line_break_class) (trilst : a list) (lregexp : line_break_regexp) =
  let rec cut trilst lregexp =
    match lregexp with
    | [] -> Some(trilst)

    | LBRESet(lbclst) :: lregexptail ->
        begin
          match trilst with
          | []                 -> None
          | trihead :: tritail -> if List.mem (getf trihead) lbclst then cut tritail lregexptail else None
        end

    | LBRENotOf(lbclst) :: lregexptail ->
        begin
          match trilst with
          | []                 -> None
          | trihead :: tritail -> if not (List.mem (getf trihead) lbclst) then cut tritail lregexptail else None
        end

    | LBREStar(lregexpsub) :: lregexptail ->
        let trilstsub = cut_by_star trilst lregexpsub in
          cut trilstsub lregexptail

  and cut_by_star trilst lregexp =
    match cut trilst lregexp with
    | None            -> trilst
    | Some(trilstsub) -> cut_by_star trilstsub lregexp
      
  in
    match cut trilst lregexp with
    | None    -> false
    | Some(_) -> true


let match_postfix  getf trilst lregexp =
  let rec reverse lregexp =
    let lregexpsub =
      lregexp |> List.map (function
        | LBREStar(lregexpiter) -> LBREStar(reverse lregexpiter)
        | other                 -> other
      )
    in
      List.rev lregexpsub
  in
    match_prefix getf trilst (reverse lregexp)


let find_first_match rules proj1 proj2 acc lst =
  rules |> List.fold_left (fun resopt (lregexp1, rescand, lregexp2) ->
    match resopt with
    | Some(_) -> resopt
    | None ->
        let b1 = match_postfix proj1 acc lregexp1 in
        let b2 = match_prefix proj2 lst lregexp2 in
        if b1 && b2 then
          let () = PrintForDebug.lbc (" (" ^ (show_lregexp lregexp1) ^ ", " ^ (show_lregexp lregexp2) ^ ")") in  (* for debug *)
          Some(rescand)
        else
          None
          
  ) None


let proj_bi (_, lbc) = lbc
let proj_tri (_, lbc, _) = lbc


let append_property (uchlst : Uchar.t list) : (Uchar.t * line_break_class) list =

  let rec normalize biacc bilst =
    match bilst with
    | [] -> List.rev biacc

    | bihead :: bitail ->
        let replopt = find_first_match normalization_rule proj_bi proj_bi (bihead :: biacc) bitail in
          match replopt with
          | None       -> normalize (bihead :: biacc) bitail
          | Some(repl) ->
              let () = PrintForDebug.lbcE "" in  (* for debug *)
                normalize (List.rev_append repl biacc) bitail
  in

  let bilst = uchlst |> List.map (fun uch -> (uch, find uch)) in
    normalize [] bilst


let append_break_opportunity (uchlst : Uchar.t list) =

  let alw_last = PreventBreak in  (* temporary; should take the adjacent embedded command into consideration *)

  let should_prevent_break triacc bilst =
    let alwopt = find_first_match line_break_rule proj_tri proj_bi triacc bilst in
      match alwopt with
      | None               -> false
      | Some(PreventBreak) -> true
      | Some(AllowBreak)   -> false
  in

  let rec aux triacc bilst =
    match bilst with
    | [] -> []

    | (uch, lbc) :: bitail ->
        begin
          PrintForDebug.lbc (InternalText.to_utf8 (InternalText.of_uchar uch));  (* for debug *)
          PrintForDebug.lbc (" " ^ (show_lb_class lbc));  (* for debug *)
          match bitail with
          | [] ->
              begin
                PrintForDebug.lbcE "";  (* for debug *)
                List.rev ((uch, lbc, alw_last) :: triacc)
              end

          | _ :: _ ->
              begin
                let b = should_prevent_break ((uch, lbc, PreventBreak (* dummy *)) :: triacc) bitail in
                PrintForDebug.lbcE "";  (* for debug *)
                let alw = if b then PreventBreak else AllowBreak in
                  aux ((uch, lbc, alw) :: triacc) bitail
              end
        end
  in
  let bilstinit = append_property uchlst in
    aux [] bilstinit


(* for debug *)
let print_trilist trilst =
  trilst |> List.iter (fun (uch, lbc, alw) ->
    let sc = InternalText.to_utf8 (InternalText.of_uchar uch) in
    let sa = match alw with AllowBreak -> "/" | PreventBreak -> "." in
      PrintForDebug.lbc (sc ^ sa)
  ); PrintForDebug.lbcE ""

(*
(* unit test *)
let () =
  set_from_file "./lib-satysfi/dist/unidata/LineBreak.txt";
  let uchlst =
    InternalText.to_uchar_list (InternalText.of_utf8
      "The quick brown fox")
  in
  let trilst = append_break_opportunity uchlst in
  print_trilist trilst
*)

