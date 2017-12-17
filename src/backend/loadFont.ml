
exception InvalidYOJSON             of string
exception UnexpectedFontHashTop     of string
exception UnexpectedFontHashElement of string
exception MultipleDesignation       of string
exception UnexpectedYOJSONKey       of string
exception UnexpectedYOJSONValue     of string * string
exception MissingRequiredYOJSONKey  of string


let read_assoc_single dir_dist assoc =
  let opt =
    assoc |> List.fold_left (fun opt pair ->
      match pair with
      | ("src-dist", `String(data)) ->
          begin
            match opt with
            | None    -> Some(Filename.concat dir_dist data)
            | Some(_) -> raise (MultipleDesignation("src-dist"))
          end

      | ("src-dist", jsonerr) ->
          raise (UnexpectedYOJSONValue("src-dist", Yojson.Safe.to_string jsonerr))

      | (keyerr, _) ->
          raise (UnexpectedYOJSONKey(keyerr))
    ) None
  in
  match opt with
  | None       -> raise (MissingRequiredYOJSONKey("src-dist"))
  | Some(data) -> data


let read_assoc dir_dist assoc =
  assoc |> List.fold_left (fun acc (abbrev, json) ->
    match json with
    | `Variant("Single", Some(`Assoc(assocsingle))) ->
        let data = read_assoc_single dir_dist assocsingle in
          (abbrev, data) :: acc

    | jsonerr ->
        raise (UnexpectedFontHashElement(Yojson.Safe.to_string jsonerr))
  ) []


let main satysfi_root_dir filename =
  Format.printf "LoadFont> main %s\n" filename;  (* for debug *)
  try
    let dir_dist = Filename.concat satysfi_root_dir "dist/fonts" in
    let srcpath = Filename.concat satysfi_root_dir (Filename.concat "dist/hash" filename) in
    let json = Yojson.Safe.from_file srcpath in  (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
    match json with
    | `Assoc(assoc) -> read_assoc dir_dist assoc
    | jsonerr       -> raise (UnexpectedFontHashTop(Yojson.Safe.to_string jsonerr))
  with
  | Yojson.Json_error(msg) -> raise (InvalidYOJSON(msg))
