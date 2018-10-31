
open MyUtil
open CharBasis

module YS = Yojson.SafePos
module MYU = MyYojsonUtil


type dir_path    = string
type file_path   = string
type key         = string
type font_abbrev = string
type script      = string


let read_single_assoc key_script script assoc =
    let abbrev = assoc |> MYU.find "font-name" |> YS.Util.to_string in
    let ratio = assoc |> MYU.find "ratio" |> YS.Util.to_float in
    let rising = assoc |> MYU.find "rising" |> YS.Util.to_float in
    (abbrev, ratio, rising)


let read_assoc (assoc : MYU.assoc) =
  List.fold_left (fun mapacc (key_script, script) ->
    let singleassoc = assoc |> MYU.find key_script |> MYU.make_assoc in
    let triple = read_single_assoc key_script script singleassoc in
    mapacc |> ScriptSchemeMap.add script triple
  ) ScriptSchemeMap.empty [
    ("han-ideographic", HanIdeographic);
    ("latin"          , Latin);
    ("kana"           , HiraganaOrKatakana);
    ("other-script"   , OtherScript);
  ]


let main () : (font_abbrev * float * float) ScriptSchemeMap.t =
  let srcpath = Config.resolve_dist_file "dist/hash/default-font.satysfi-hash" in
    try
      let json = YS.from_file ~fname:srcpath srcpath in
          (* -- may raise 'Sys_error' -- *)
      let assoc = json |> MYU.make_assoc in
      read_assoc assoc
    with
    | Yojson.Json_error(msg) -> MYU.syntax_error srcpath msg
