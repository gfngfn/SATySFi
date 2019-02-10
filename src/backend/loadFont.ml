
open MyUtil

module YS = Yojson.SafePos
module MYU = MyYojsonUtil

type font_abbrev = string

type data =
  | Single     of lib_path
  | Collection of lib_path * int


let read_path_from_dict ((pos, _) as assoc) : lib_path =
  let srcopt = assoc |> MYU.find_opt "src" in
  let srcdistopt = assoc |> MYU.find_opt "src-dist" in
  let relpathstr =
  match (srcopt, srcdistopt) with
  | (None, None) ->
      raise (MYU.MissingRequiredKey(MYU.make_range pos, "src"))

  | (None, Some(srcdist)) ->
      let rng = MYU.make_range pos in
      Logging.warn_deprecated ("at " ^ (Range.message rng) ^ ": the key 'src-dist' in font hash files is deprecated; consider using 'src'.");
      let s = srcdist |> YS.Util.to_string in
      Filename.concat "dist/fonts" s

  | (Some(src), None) ->
      src |> YS.Util.to_string

  | (Some(src), Some(_)) ->
      let rng = MYU.make_range pos in
      Logging.warn_deprecated ("at " ^ (Range.message rng) ^ ": the key 'src-dist' in font hash files is deprecated; the entry of 'src' is used.");
      src |> YS.Util.to_string
  in
  make_lib_path relpathstr


let read_assoc_single (json : YS.json) : data =
  let assoc = json |> MYU.make_assoc in
  let relpath = assoc |> read_path_from_dict in
  Single(relpath)


let read_assoc_ttc (json : YS.json) =
  let assoc = json |> MYU.make_assoc in
  let relpath = assoc |> read_path_from_dict in
  let index = assoc |> MYU.find "index" |> YS.Util.to_int in
  Collection(relpath, index)


let read_assoc (assoc : MYU.assoc) =
  assoc |> MYU.fold (fun abbrev json acc ->
    let data =
      json |> MYU.decode_variant [
        ("Single", MYU.Arg(read_assoc_single));
        ("Collection", MYU.Arg(read_assoc_ttc));
      ]
    in
    Alist.extend acc (abbrev, data)
  ) Alist.empty |> Alist.to_list


let main (abspath : abs_path) : (font_abbrev * data) list =
  let pathstr = get_abs_path_string abspath in
  try
    let json = YS.from_file ~fname:pathstr pathstr in
        (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
    let assoc = MYU.make_assoc json in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error pathstr msg
