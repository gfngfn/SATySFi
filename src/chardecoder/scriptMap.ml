
let script_map_ref : (string UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))

let set_from_channel channel =
  let script_map = ScriptParser.main ScriptLexer.expr (Lexing.from_channel channel) in
  begin
    script_map_ref := script_map;
  end

let find_opt uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> None
  | Some(uch_ucore) -> (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore
