
type file_path = string
type dir_path = string
type font_abbrev = string

exception InvalidYOJSON                   of file_path * string
exception FontHashOtherThanDictionary     of file_path * string
exception FontHashElementOtherThanVariant of file_path * string
exception MultipleDesignation             of file_path * font_abbrev * string
exception UnexpectedYOJSONKey             of file_path * font_abbrev * string
exception UnexpectedYOJSONValue           of file_path * font_abbrev * string * string
exception MissingRequiredYOJSONKey        of file_path * font_abbrev * string


let read_assoc_single (srcpath : file_path) (abbrev : font_abbrev) (dir_dist : dir_path) assoc =
  let opt =
    assoc |> List.fold_left (fun opt pair ->
      match pair with
      | ("src-dist", `String(data)) ->
          begin
            match opt with
            | None    -> Some(Filename.concat dir_dist data)
            | Some(_) -> raise (MultipleDesignation(srcpath, abbrev, "src-dist"))
          end

      | ("src-dist", jsonerr) ->
          raise (UnexpectedYOJSONValue(srcpath, abbrev, "src-dist", Yojson.Safe.to_string jsonerr))

      | (keyerr, _) ->
          raise (UnexpectedYOJSONKey(srcpath, abbrev, keyerr))
    ) None
  in
  match opt with
  | None       -> raise (MissingRequiredYOJSONKey(srcpath, abbrev, "src-dist"))
  | Some(data) -> data


let read_assoc (srcpath : file_path) (dir_dist : dir_path) assoc =
  assoc |> List.fold_left (fun acc (abbrev, json) ->
    match json with
    | `Variant("Single", Some(`Assoc(assocsingle))) ->
        let data = read_assoc_single srcpath abbrev dir_dist assocsingle in
          Alist.extend acc (abbrev, data)

    | json_other ->
        raise (FontHashElementOtherThanVariant(srcpath, Yojson.Safe.to_string json_other))
  ) Alist.empty |> Alist.to_list


let main (satysfi_root_dir : dir_path) (filename : file_path) =
(*
  Format.printf "LoadFont> main %s\n" filename;  (* for debug *)
*)
  let dir_dist = Filename.concat satysfi_root_dir "dist/fonts" in
  let srcpath = Filename.concat satysfi_root_dir (Filename.concat "dist/hash" filename) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
        match json with
        | `Assoc(assoc) -> read_assoc srcpath dir_dist assoc
        | json_other    -> raise (FontHashOtherThanDictionary(srcpath, Yojson.Safe.to_string json_other))
    with
    | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))
