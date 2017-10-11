
open CharBasis


let read_script = function
  | "Common"   -> Common
  | "Han"      -> HanIdeographic
  | "Hiragana" -> HiraganaOrKatakana
  | "Katakana" -> HiraganaOrKatakana
  | "Latin"    -> Latin
(* temporary; should add more scripts *)
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
  | None            -> Other
  | Some(uch_ucore) ->
      match (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None         -> Other
      | Some(script) -> script


let divide_by_script uchlst =
  let rec aux resacc scracc uchlst =
    match uchlst with
    | [] ->
        begin
          match scracc with
          | None                       -> List.rev resacc
          | Some((prevscript, uchacc)) -> List.rev ((prevscript, List.rev uchacc) :: resacc)
        end

    | uch :: tail ->
        let script = find_opt uch in
          match scracc with
          | None                       -> aux resacc (Some((script, [uch]))) tail
          | Some((prevscript, uchacc)) ->
              if script_equal prevscript script then
                aux resacc (Some((script, uch :: uchacc))) tail
              else
                aux ((prevscript, List.rev uchacc) :: resacc) (Some((script, [uch]))) tail
                
  in
    aux [] None uchlst

