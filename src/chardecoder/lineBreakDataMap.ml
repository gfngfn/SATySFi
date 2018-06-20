
open CharBasis

exception InputFileBroken


let class_of_string _ s =
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


(* temporary; should depend on the language system of the context *)
let line_break_class_overriding_list =
  [
(*
  (* -- U+0000..U+00FF -- *)
    (0x00AB, OP  );  (* LEFT-POINTING DOUBLE ANGLE QUOTATION MARK  *)
    (0x00BB, CP  );  (* RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK *)
*)
  (* -- U+2000..U+20FF -- *)
    (0x2018, OP  );  (* LEFT SINGLE QUOTATION MARK             *)
    (0x2019, CP  );  (* RIGHT SINGLE QUOTATION MARK            *)
    (0x201B, OP  );  (* SINGLE HIGH-REVERSED-9 QUOTATION MARK  *)
    (0x201C, OP  );  (* LEFT DOUBLE QUOTATION MARK             *)
    (0x201D, CP  );  (* RIGHT DOUBLE QUOTATION MARK            *)
    (0x201F, OP  );  (* DOUBLE HIGH-REVERSED-9 QUOTATION MARK  *)
  (* -- U+3000..U+30FF -- *)
    (0x3001, JLCM);  (* IDEOGRAPHIC COMMA                      *)
    (0x3002, JLFS);  (* IDEOGRAPHIC FULL STOP                  *)
    (0x3008, JLOP);  (* LEFT ANGLE BRACKET                     *)
    (0x3009, JLCP);  (* RIGHT ANGLE BRACKET                    *)
    (0x300A, JLOP);  (* LEFT DOUBLE ANGLE BRACKET              *)
    (0x300B, JLCP);  (* RIGHT DOUBLE ANGLE BRACKET             *)
    (0x300C, JLOP);  (* LEFT CORNER BRACKET                    *)
    (0x300D, JLCP);  (* RIGHT CORNER BRACKET                   *)
    (0x300E, JLOP);  (* LEFT WHITE CORNER BRACKET              *)
    (0x300F, JLCP);  (* RIGHT WHITE CORNER BRACKET             *)
    (0x3010, JLOP);  (* LEFT BLACK LENTICULAR BRACKET          *)
    (0x3011, JLCP);  (* RIGHT BLACK LENTICULAR BRACKET         *)
    (0x3014, JLOP);  (* LEFT TORTOISE SHELL BRACKET            *)
    (0x3015, JLCP);  (* RIGHT TORTOISE SHELL BRACKET           *)
    (0x3016, JLOP);  (* LEFT WHITE LENTICULAR BRACKET          *)
    (0x3017, JLCP);  (* RIGHT WHITE LENTICULAR BRACKET         *)
    (0x3018, JLOP);  (* LEFT WHITE TORTOISE SHELL BRACKET      *)
    (0x3019, JLCP);  (* RIGHT WHITE TORTOISE SHELL BRACKET     *)
    (0x301A, JLOP);  (* LEFT WHITE SQUARE BRACKET              *)
    (0x301B, JLCP);  (* RIGHT WHITE SQUARE BRACKET             *)
    (0x30FC, JLPL);  (* KATAKANA-HIRAGARA PROLONGED SOUND MARK *)
    (0xFF08, JLOP);  (* FULLWIDTH LEFT PARENTHESIS             *)
    (0xFF09, JLCP);  (* FULLWIDTH RIGHT PARENTHESIS            *)
    (0xFF0C, JLCM);  (* FULLWIDTH COMMA                        *)
    (0xFF0E, JLFS);  (* FULLWIDTH FULL STOP                    *)
    (0xFF3B, JLOP);  (* FULLWIDTH LEFT SQUARE BRACKET          *)
    (0xFF3D, JLCP);  (* FULLWIDTH RIGHT SQUARE BRACKET         *)
    (0xFF5B, JLOP);  (* FULLWIDTH LEFT CURLY BRACKET           *)
    (0xFF5D, JLCP);  (* FULLWIDTH RIGHT CURLY BRACKET          *)
    (0xFF5F, JLOP);  (* FULLWIDTH LEFT WHITE PARENTHESIS       *)
    (0xFF60, JLCP);  (* FULLWIDTH RIGHT WHITE PARENTHESIS      *)
  ]


let line_break_map_ref : (line_break_class BatIMap.t) ref = ref (BatIMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let line_break_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let line_break_map_raw = line_break_list |> CharBasis.map_of_list class_of_string in
  let line_break_map =
    List.fold_left (fun mapacc (cp, lbc) ->
      mapacc |> BatIMap.add cp lbc
    ) line_break_map_raw line_break_class_overriding_list
  in
  begin
    line_break_map_ref := line_break_map;
  end


let find uch =
  try (!line_break_map_ref) |> BatIMap.find (Uchar.to_int uch)
  with Not_found -> XX  (* temporary *)


let set lbclst = LBRESet(lbclst)
let notof lbclst = LBRENotOf(lbclst)
let exact lbc = set [lbc]
let star lbre = LBREStar(lbre)
let spaced = set [AL]
let nonspaced = set [ID; CJ; IN; SA; JLOP; JLCP; JLNS; JLMD; JLFS; JLCM; JLPL; JLSM]
  (* -- does NOT contain JLHY -- *)


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
      ([], PreventBreak, [set [CL; CP; EX; IS; SY; JLCP; JLCM; JLFS]]);
    (* -- LB14 -- *)
    (* -- Original: ideographic open punctuations -- *)
      ([set [OP; JLOP]; star [exact SP]], PreventBreak, []);
    (* -- LB15 -- *)
      ([exact QU; star [exact SP]], PreventBreak, [exact OP]);
    (* -- LB16 -- *)
    (* -- Original: ideographic nonstarters -- *)
      ([set [CL; CP]; star [exact SP]], PreventBreak, [set [NS; CJ; JLNS; JLPL]]);
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
    (* -- Original: ideographic nonstarters -- *)
      ([], PreventBreak, [set [BA; HY; NS; CJ; JLNS; JLPL]]);
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
(*
          let () = PrintForDebug.lbc (" (" ^ (show_lregexp lregexp1) ^ ", " ^ (show_lregexp lregexp2) ^ ")") in  (* for debug *)
*)
          Some(rescand)
        else
          None

  ) None


let proj_bi (_, lbc) = lbc
let proj_tri (_, lbc, _) = lbc


let append_property (uchlst : Uchar.t list) : (Uchar.t * line_break_class) list =

  let rec normalize biacc bilst =
    match bilst with
    | [] -> Alist.to_list biacc

    | bihead :: bitail ->
        let replopt = find_first_match normalization_rule proj_bi proj_bi (bihead :: (Alist.to_list_rev biacc)) bitail in
          match replopt with
          | None       -> normalize (Alist.extend biacc bihead) bitail
          | Some(repl) -> normalize (Alist.append biacc repl) bitail
  in

  let bilst = uchlst |> List.map (fun uch -> (uch, find uch)) in
    normalize Alist.empty bilst


let append_break_opportunity (uchlst : Uchar.t list) (alw_last : break_opportunity) =

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
          match bitail with
          | [] ->
              begin
                Alist.to_list (Alist.extend triacc (uch, lbc, alw_last))
              end

          | _ :: _ ->
              begin
                let b = should_prevent_break ((uch, lbc, PreventBreak (* dummy *)) :: (Alist.to_list_rev triacc)) bitail in
                let alw = if b then PreventBreak else AllowBreak in
                  aux (Alist.extend triacc (uch, lbc, alw)) bitail
              end
        end
  in
  let bilstinit = append_property uchlst in
    aux Alist.empty bilstinit


(*
(* for debug *)
let print_trilist trilst =
  trilst |> List.iter (fun (uch, lbc, alw) ->
    let sc = InternalText.to_utf8 (InternalText.of_uchar uch) in
    let sa = match alw with AllowBreak -> "/" | PreventBreak -> "." in
      PrintForDebug.lbc (sc ^ sa)
  ); PrintForDebug.lbcE ""
*)

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
