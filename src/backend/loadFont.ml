
exception InvalidExtendedJSON of string
exception InvalidFontHashTop
exception InvalidFontHashElement
exception MultipleDesignation
exception InvalidKey


let read_assoc_single dir_dist assoc =
  let opt =
    assoc |> List.fold_left (fun opt pair ->
      match pair with
      | ("src-dist", `String(data)) ->
          begin
            match opt with
            | None    -> Some(Filename.concat dir_dist data)
            | Some(_) -> raise MultipleDesignation
          end

      | _ -> raise InvalidKey
    ) None
  in
  match opt with
  | None       -> raise InvalidKey
  | Some(data) -> data


let read_assoc dir_dist assoc =
  assoc |> List.fold_left (fun acc (abbrev, json) ->
    match json with
    | `Variant("Single", Some(`Assoc(assocsingle))) -> (abbrev, read_assoc_single dir_dist assocsingle) :: acc
    | _                                             -> raise InvalidFontHashElement
  ) []


let main satysfi_root_dir filename =
  Format.printf "LoadFont> main\n";  (* for debug *)
  try
    let dir_dist = Filename.concat satysfi_root_dir "dist/fonts" in
    let srcpath = Filename.concat satysfi_root_dir (Filename.concat "dist/hash" filename) in
    let json = Yojson.Safe.from_file srcpath in
    match json with
    | `Assoc(assoc) -> read_assoc dir_dist assoc
    | _             -> raise InvalidFontHashTop
  with
  | Yojson.Json_error(msg) -> raise (InvalidExtendedJSON(msg))
