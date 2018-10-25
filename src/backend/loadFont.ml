
open MyUtil

module YS = Yojson.SafePos
module MYU = MyYojsonUtil


type file_path = string
type dir_path = string
type font_abbrev = string

exception InvalidYOJSON                   of file_path * string
exception FontHashOtherThanDictionary     of file_path
exception FontHashElementOtherThanVariant of file_path * font_abbrev * string
exception MultipleDesignation             of file_path * font_abbrev * string
exception UnexpectedYOJSONKey             of file_path * font_abbrev * string
exception UnexpectedYOJSONValue           of file_path * font_abbrev * string * string
exception MissingRequiredYOJSONKey        of file_path * font_abbrev * string


type data =
  | Single     of string
  | Collection of string * int


let read_assoc_single (json : YS.json) =
  let assoc = json |> MYU.make_assoc in
  let path = assoc |> MYU.find "src-dist" |> YS.Util.to_string in
  Single(path)


let read_assoc_ttc (json : YS.json) =
  let assoc = json |> MYU.make_assoc in
  let path = assoc |> MYU.find "src-dist" |> YS.Util.to_string in
  let index = assoc |> MYU.find "index" |> YS.Util.to_int in
  Collection(path, index)


let read_assoc (srcpath : file_path) (assoc : MYU.assoc) =
  assoc |> MYU.fold (fun abbrev json acc ->
    let data =
      json |> MYU.decode_variant [
        ("Single", MYU.Arg(read_assoc_single));
        ("Collection", MYU.Arg(read_assoc_ttc));
      ]
    in
    Alist.extend acc (abbrev, data)
  ) Alist.empty |> Alist.to_list


let main (filename : file_path) =
  let srcpath = Config.resolve_dist_file (Filename.concat "dist/hash" filename) in
    try
      let json = YS.from_file ~fname:srcpath srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
      let assoc = MYU.make_assoc json in
      read_assoc srcpath assoc
    with
    | Yojson.Json_error(msg)                 -> raise (InvalidYOJSON(srcpath, msg))
    | Yojson.Safe.Util.Type_error(msg, json) -> raise (InvalidYOJSON(srcpath, msg))
