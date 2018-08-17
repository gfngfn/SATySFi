
open MyUtil
open CharBasis

type dir_path    = string
type file_path   = string
type key         = string
type font_abbrev = string
type script      = string

exception InvalidYOJSON              of file_path * string
exception OtherThanDictionary        of file_path
exception MissingRequiredScriptKey   of file_path * key
exception MissingRequiredKey         of file_path * key * key
exception ElementOtherThanDictionary of file_path * key * string
exception InvalidDataTypeOfKey       of file_path * key * key

type error =
  | NotADictionary  of key * string
  | MissingScript   of key
  | MissingKey      of key * key
  | InvalidDataType of key * key


let convert e opt =
  let open ResultMonad in
    match opt with
    | Some(v) -> return v
    | None    -> err e


let get_string e json =
  let open ResultMonad in
    match json with
    | `String(s) -> return s
    | _          -> err e


let get_float e json =
  let open ResultMonad in
    match json with
    | `Float(c) -> return c
    | _         -> err e


let read_single_assoc key_script script mapres assoc =
  let open MyUtil.ResultMonad in
    mapres >>= fun map ->
    List.assoc_opt "font-name" assoc |> convert (MissingKey(key_script, "font-name")) >>=
      get_string (InvalidDataType(key_script, "font-name")) >>= fun abbrev ->
    List.assoc_opt "ratio"     assoc |> convert (MissingKey(key_script, "ratio")) >>=
      get_float (InvalidDataType(key_script, "ratio")) >>= fun ratio ->
    List.assoc_opt "rising"    assoc |> convert (MissingKey(key_script, "rising")) >>=
      get_float (InvalidDataType(key_script, "rising")) >>= fun rising ->
    return (map |> ScriptSchemeMap.add script (abbrev, ratio, rising))


let read_assoc srcpath assoc =
  let open MyUtil.ResultMonad in
  let res =
    List.fold_left (fun mapres (key_script, script) ->
      assoc |> List.assoc_opt key_script |> convert (MissingScript(key_script)) >>= fun json ->
        match json with
        | `Assoc(assoc) -> read_single_assoc key_script script mapres assoc
        | _             -> err (NotADictionary(key_script, Yojson.Safe.to_string json))
    ) (Ok(ScriptSchemeMap.empty)) [
      ("han-ideographic", HanIdeographic);
      ("latin"          , Latin);
      ("kana"           , HiraganaOrKatakana);
      ("other-script"   , OtherScript);
    ]
  in
    match res with
    | Ok(map)  -> map
    | Error(e) ->
        begin
          match e with
          | NotADictionary(key, jsonstr)     -> raise (ElementOtherThanDictionary(srcpath, key, jsonstr))
          | MissingScript(key_script)        -> raise (MissingRequiredScriptKey(srcpath, key_script))
          | MissingKey(key_script, key)      -> raise (MissingRequiredKey(srcpath, key_script, key))
          | InvalidDataType(key_script, key) -> raise (InvalidDataTypeOfKey(srcpath, key_script, key))
        end


let main () : (font_abbrev * float * float) ScriptSchemeMap.t =
  let srcpath = Config.resolve_dist_file "dist/hash/default-font.satysfi-hash" in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
        match json with
        | `Assoc(assoc) -> read_assoc srcpath assoc
        | json_other    -> raise (OtherThanDictionary(srcpath))
    with
    | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))
