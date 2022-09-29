
open MyUtil
open CharBasis

module YS = Yojson.SafePos
module MYU = MyYojsonUtil


type dir_path    = string
type file_path   = string
type key         = string
type font_abbrev = string
type script      = string


let read_single_assoc assoc =
    let abbrev = assoc |> MYU.find "font-name" |> YS.Util.to_string in
    let ratio = assoc |> MYU.find "ratio" |> YS.Util.to_float in
    let rising = assoc |> MYU.find "rising" |> YS.Util.to_float in
    (abbrev, ratio, rising)


let read_assoc (assoc : MYU.assoc) =
  List.fold_left (fun mapacc (key_script, script) ->
    let singleassoc = assoc |> MYU.find key_script |> MYU.make_assoc in
    let triple = read_single_assoc singleassoc in
    mapacc |> ScriptSchemeMap.add script triple
  ) ScriptSchemeMap.empty [
    ("han-ideographic", HanIdeographic);
    ("latin"          , Latin);
    ("kana"           , HiraganaOrKatakana);
    ("other-script"   , OtherScript);
  ]


let main (abspath : abs_path) : (font_abbrev * float * float) ScriptSchemeMap.t =
  let pathstr = get_abs_path_string abspath in
  try
    let json = YS.from_file ~fname:pathstr pathstr in
        (* -- may raise 'Sys_error' -- *)
    let assoc = json |> MYU.make_assoc in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error pathstr msg
