
exception InputFileBroken


(* -- line breaking classes [UAX #14 Table 1] -- *)
type line_break_class =
(* -- non-tailorable line breaking classes -- *)
  | CM  (* combining marks *)
  | SG  (* surrogates *)
  | WJ  (* word joiner *)
  | ZW  (* zero width space *)
  | GL  (* non-breaking; glue *)
  | SP  (* space *)
  | ZWJ (* zero width joiner *)
  | B2  (* break opportunity *)
  | BA  (* break after *)
  | BB  (* break before *)
  | HY  (* hyphen *)
  | CB  (* contingent break opportunity *)
(* -- characters prohibiting certain breaks -- *)
  | CL  (* close punctuation *)
  | CP  (* close parenthesis *)
  | EX  (* exclamation *)
  | IN  (* inseparable *)
  | NS  (* nonstarter *)
  | OP  (* open punctuation *)
  | QU  (* quotation *) (* maybe not necessary *)
(* -- numeric context -- *)
  | IS  (* infix numeric separator *)
  | NU  (* numeric *)
  | PO  (* postfix numeric *)
  | PR  (* prefix numeric *)
  | SY  (* symbols allowing break after *)
(* -- other characters -- *)
  | AI  (* ambiguous (alphabetic or ideographic) *)
  | AL  (* alphabetic *)
  | CJ  (* conditional Japanese starter *)
  | EB  (* emoji base *)  (* maybe not necessary *)
  | EM  (* empji modifier *)  (* maybe not necessary *)
  | H2  (* Hangul LV syllable *)
  | H3  (* Hangul LVT syllable *)
  | HL  (* Hebrew letter *)
  | ID  (* ideographic *)
  | JL  (* Hangul L Jamo *)
  | JV  (* Hangul V Jamo *)
  | JT  (* Hangul T Jamo *)
  | RI  (* regional indicator *)
  | SA  (* complex context dependent; south east Asian *)
  | XX  (* unknown *)


let class_of_string s =
  match s with
  | "CM" -> CM
  | "SG" -> SG
  | "WJ" -> WJ
  | "ZW" -> ZW
  | "GL" -> GL
  | "SP" -> SP
  | "ZWJ" -> ZWJ
  | "B2" -> B2
  | "BA" -> BA
  | "BB" -> BB
  | "HY" -> HY
  | "CB" -> CB
  | "CL" -> CL
  | "CP" -> CP
  | "EX" -> EX
  | "IN" -> IN
  | "NS" -> NS
  | "OP" -> OP
  | "QU" -> QU  (* maybe not necessary *)
  | "IS" -> IS
  | "NU" -> NU
  | "PO" -> PO
  | "PR" -> PR
  | "SY" -> SY
  | "AI" -> AI
  | "AL" -> AL
  | "CJ" -> CJ
  | "EB" -> EB  (* maybe not necessary *)
  | "EM" -> EM  (* maybe not necessary *)
  | "H2" -> H2
  | "H3" -> H3
  | "HL" -> HL
  | "ID" -> ID
  | "JL" -> JL
  | "JV" -> JV
  | "JT" -> JT
  | "RI" -> RI
  | "SA" -> SA
  | "XX" -> XX
  | _    -> raise InputFileBroken


let line_break_map_ref : (line_break_class UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let line_break_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let line_break_map = line_break_list |> CharBasis.map_of_list class_of_string in
  begin
    line_break_map_ref := line_break_map;
  end


let find_opt uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> None
  | Some(uch_ucore) -> (!line_break_map_ref) |> UCoreLib.UMap.find_opt uch_ucore
