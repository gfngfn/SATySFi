open Config
open MyUtil

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


let read_assoc_single (srcpath : file_path) (abbrev : font_abbrev) assoc =
  let opt =
    assoc |> List.fold_left (fun opt pair ->
      match pair with
      | ("src-dist", `String(path)) ->
          begin
            match opt with
            | None    -> Some(path)
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
  | Some(path) -> Single(path)


let read_assoc_ttc (srcpath : file_path) (abbrev : font_abbrev) assoc =
  let opts =
    assoc |> List.fold_left (fun (pathopt, iopt) pair ->
      match pair with
      | ("src-dist", `String(path)) ->
          begin
            match pathopt with
            | None    -> (Some(path), iopt)
            | Some(_) -> raise (MultipleDesignation(srcpath, abbrev, "src-dist"))
          end

      | ("src-dist", jsonerr) ->
          raise (UnexpectedYOJSONValue(srcpath, abbrev, "src-dist", Yojson.Safe.to_string jsonerr))

      | ("index", `Int(i)) ->
          begin
            match iopt with
            | None    -> (pathopt, Some(i))
            | Some(_) -> raise (MultipleDesignation(srcpath, abbrev, "index"))
          end

      | ("index", jsonerr) ->
          raise (UnexpectedYOJSONValue(srcpath, abbrev, "index", Yojson.Safe.to_string jsonerr))

      | (keyerr, _) ->
          raise (UnexpectedYOJSONKey(srcpath, abbrev, keyerr))

    ) (None, None)
  in
  match opts with
  | (Some(path), Some(i)) -> Collection(path, i)
  | (None, _)             -> raise (MissingRequiredYOJSONKey(srcpath, abbrev, "src-dist"))
  | (Some(_), _)          -> raise (MissingRequiredYOJSONKey(srcpath, abbrev, "index"))


let read_assoc (srcpath : file_path) assoc =
  assoc |> List.fold_left (fun acc (abbrev, json) ->
    match json with
    | `Variant("Single", Some(`Assoc(assocsingle))) ->
        let data = read_assoc_single srcpath abbrev assocsingle in
          Alist.extend acc (abbrev, data)

    | `Variant("Collection", Some(`Assoc(assocttc))) ->
        let data = read_assoc_ttc srcpath abbrev assocttc in
          Alist.extend acc (abbrev, data)

    | _ ->
        raise (FontHashElementOtherThanVariant(srcpath, abbrev, Yojson.Safe.to_string json))
  ) Alist.empty |> Alist.to_list


let main (filename : file_path) =
(*
  Format.printf "LoadFont> main %s\n" filename;  (* for debug *)
*)
  let srcpath = resolve_dist_path (Filename.concat "dist/hash" filename) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
      let assoc = Yojson.Safe.Util.to_assoc json in
      read_assoc srcpath assoc
    with
    | Yojson.Json_error(msg)                 -> raise (InvalidYOJSON(srcpath, msg))
    | Yojson.Safe.Util.Type_error(msg, json) -> raise (InvalidYOJSON(srcpath, msg))
