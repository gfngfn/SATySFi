
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


let find uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> Other
  | Some(uch_ucore) ->
      match (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None         -> Other
      | Some(script) -> script


let divide_by_script trilst =

  let rec aux resacc scraccopt trilst =
    match trilst with
    | [] ->
        begin
          match scraccopt with
          | None                       -> List.rev resacc
          | Some((prevscript, uchacc)) -> List.rev ((prevscript, List.rev uchacc) :: resacc)
        end

    | ((uch, _, alwref) as trihead) :: tritail ->
        let script = find uch in
        begin
          match !alwref with
          | AllowBreak ->
              begin
                match scraccopt with
                | None                       -> aux ((script, [trihead]) :: resacc) None tritail
                | Some((prevscript, triacc)) ->
                    if script_equal prevscript script then
                      aux ((prevscript, List.rev (trihead :: triacc)) :: resacc) None tritail
                    else
                      aux ((script, [trihead]) :: (prevscript, List.rev triacc) :: resacc) None tritail
              end

          | PreventBreak ->
              begin
                match scraccopt with
                | None                       -> aux resacc (Some((script, [trihead]))) tritail
                | Some((prevscript, triacc)) ->
                    if script_equal prevscript script then
                      aux resacc (Some((script, trihead :: triacc))) tritail
                    else
                      aux ((prevscript, List.rev triacc) :: resacc) (Some((script, [trihead]))) tritail
              end
        end
  in

  let scrlst = aux [] None trilst in
    scrlst

