
let print_for_debug msg =

  print_string msg;

  ()

let print_endline_for_debug msg =

  print_endline msg;

  ()


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
  | "BK"  -> BreakClass
  | "CR"  -> BreakClass
  | "LF"  -> BreakClass
  | "NL"  -> BreakClass
  | _     -> raise InputFileBroken


let line_break_map_ref : (line_break_class UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let line_break_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let line_break_map = line_break_list |> CharBasis.map_of_list class_of_string in
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

(*
let insert_break_opportunity lst =
  let rec aux acc lst =
    match lst with
    | []                                          -> List.rev acc
    | (uch, lbc) :: []                            -> aux2 uch lbc None acc []
    | (uch, lbc) :: (((_, lbcnext) :: _) as tail) -> aux2 uch lbc (Some(lbcnext)) acc tail

  and aux2 uch lbc lbcnextopt acc tail =
    match lbc with
    | AL -> 
    | BA -> aux (DirectBreak :: Character(uch) :: acc) tail
    |
  in
    aux [] lst
*)

let append_property (uchlst : Uchar.t list) =
  uchlst |> List.map (fun uch -> (uch, find uch))


let line_break_rule =
  let set lbclst = LBRESet(lbclst) in
  let notof lbclst = LBRENotOf(lbclst) in
  let exact lbc = set [lbc] in
  let star lbre = LBREStar(lbre) in
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
      ([], PreventBreak, [set [CL; CP; EX; IS; SY]]);
    (* -- LB14 -- *)
      ([exact OP; star [exact SP]], PreventBreak, []);
    (* -- LB15 -- *)
      ([exact QU; star [exact SP]], PreventBreak, [exact OP]);
    (* -- LB16 -- *)
      ([set [CL; CP]; star [exact SP]], PreventBreak, [set [NS; CJ]]);
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
      ([], PreventBreak, [set [BA; HY; NS; CJ]]);
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


(* -- a naive, greedy regexp matching -- *)
let match_prefix trilst lregexp =
  let rec cut trilst lregexp =
    match lregexp with
    | [] -> Some(trilst)

    | LBRESet(lbclst) :: lregexptail ->
        begin
          match trilst with
          | []                     -> None
          | (_, lbc, _) :: tritail -> if List.mem lbc lbclst then cut tritail lregexptail else None
        end

    | LBRENotOf(lbclst) :: lregexptail ->
        begin
          match trilst with
          | []                     -> None
          | (_, lbc, _) :: tritail -> if not (List.mem lbc lbclst) then cut tritail lregexptail else None
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


let match_postfix trilst lregexp =
  let rec reverse lregexp =
    let lregexpsub =
      lregexp |> List.map (function
        | LBREStar(lregexpiter) -> LBREStar(reverse lregexpiter)
        | other                 -> other
      )
    in
      List.rev lregexpsub
  in
    match_prefix trilst (reverse lregexp)


let append_break_opportunity (uchlst : Uchar.t list) =

  let should_prevent_break triacc trilst =
    let alwopt =
      line_break_rule |> List.fold_left (fun alwoptacc (lregexp1, alw, lregexp2) ->
        match alwoptacc with
        | Some(_) -> alwoptacc
        | None ->
            let b1 = match_postfix triacc lregexp1 in
            let b2 = match_prefix trilst lregexp2 in
            if b1 && b2 then
              let () = print_for_debug (" (" ^ (show_lregexp lregexp1) ^ ", " ^ (show_lregexp lregexp2) ^ ")") in  (* for debug *)
              Some(alw) else alwoptacc
      ) None
    in
      match alwopt with
      | None               -> false
      | Some(PreventBreak) -> true
      | Some(AllowBreak)   -> false
  in

  let rec aux triacc trilst =
    match trilst with
    | [] -> []

    | ((uch, lbc, alwref) as trihead) :: tritail ->
        let triaccnew = trihead :: triacc in
        let trilstnew = tritail in
        begin
          match tritail with
          | [] ->
              begin
                alwref := PreventBreak;
                List.rev triaccnew
              end

          | _ :: _ ->
              begin
                print_for_debug (InternalText.to_utf8 (InternalText.of_uchar uch));  (* for debug *)
                print_for_debug (" " ^ (show_lb_class lbc));  (* for debug *)
                let b = should_prevent_break triaccnew trilstnew in
                print_endline_for_debug "";  (* for debug *)
                if b then
                  begin alwref := PreventBreak; end
                else ();
                aux triaccnew trilstnew
              end
        end
  in
  let uchlblst = append_property uchlst in
  let trilstinit =
    uchlblst |> List.map (fun (uch, lbc) ->
      let alwref = ref AllowBreak in (uch, lbc, alwref)
    )
  in
    aux [] trilstinit


(* for debug *)
let print_trilist trilst =
  trilst |> List.iter (fun (uch, lbc, alwref) ->
    let sc = InternalText.to_utf8 (InternalText.of_uchar uch) in
    let sa = match !alwref with AllowBreak -> "/" | PreventBreak -> "." in
    print_for_debug (sc ^ sa)
  ); print_endline ""

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

