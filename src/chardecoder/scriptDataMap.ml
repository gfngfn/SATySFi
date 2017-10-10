
type script =
  | Common              (*   --  ; Zyyy; Common *)
  | Inherited           (*   --  ; Zinh; Inherited *)
  | Unknown             (*   --  ; Zzzz; Unknown *)
  | HanIdeographic      (* 'hani'; Hani; Han *)
  | HiraganaOrKatakana  (* 'kana'; Hrkt; Hiragana_Or_Katakana *)
  | Latin               (* 'latn'; Latn; Latin *)
(* temporary; should add more scripts *)
  | Other


let read_script = function
  | "Common"   -> Common
  | "Han"      -> HanIdeographic
  | "Hiragana" -> HiraganaOrKatakana
  | "Katakana" -> HiraganaOrKatakana
  | "Latin"    -> Latin
  | _          -> Other


let script_map_ref : (script UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let script_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let script_map = script_list |> CharBasis.map_of_list read_script in
  begin
    script_map_ref := script_map;
  end


let find_opt uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> None
  | Some(uch_ucore) -> (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore
