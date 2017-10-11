
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
  | None            -> AL  (* temporary *)
  | Some(uch_ucore) ->
      match (!line_break_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None      -> AL  (* temporary *)
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

let append_property uchlst =
  uchlst |> List.map (fun uch -> (uch, find uch))
