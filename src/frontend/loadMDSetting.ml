
open MyUtil
open MyYojsonUtil


type file_path = string

exception MultipleDesignation of file_path * string
exception InvalidYOJSON       of file_path * string
exception MissingRequiredKey  of file_path * string
exception InvalidValueForKey  of file_path * string


let cut_module_names (exn : exn) (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: mdlnmsrev -> (List.rev mdlnmsrev, varnm)
  | _                  -> raise exn


let get_command_main exn (prefix : char) (s : string) =
  try
    if Char.equal (String.get s 0) prefix then
      let stail = (String.sub s 1 (String.length s - 1)) in
      let (mdlnms, varnm) = cut_module_names exn stail in
      (mdlnms, (String.make 1 prefix) ^ varnm)
    else
      raise exn
  with
  | Invalid_argument(_) -> raise exn


let get_command (srcpath : file_path) (prefix : char) (k : string) (assoc : assoc) =
  let json = assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, k)) k in
  let exn = InvalidValueForKey(srcpath, k) in
  match json with
  | `String(s) -> get_command_main exn prefix s
  | _          -> raise exn


let make_code_name_map (srcpath : file_path) (prefix : char) (json : Yojson.Safe.json) : (string list * string) DecodeMD.CodeNameMap.t =
  let open DecodeMD in
  match json with
  | `List(pairs) ->
      pairs |> List.fold_left (fun accmap json ->
        match json with
        | `Tuple(`String(name) :: `String(s) :: []) ->
            if accmap |> CodeNameMap.mem name then
              raise (MultipleDesignation(srcpath, name))
            else
              let cmd = get_command_main (InvalidValueForKey(srcpath, name)) prefix s in
              accmap |> CodeNameMap.add name cmd

        | _ ->
            raise (InvalidYOJSON(srcpath, "not a pair: " ^ (Yojson.Safe.to_string json)))
      ) CodeNameMap.empty

  | _ ->
      raise (InvalidYOJSON(srcpath, "not a list: " ^ (Yojson.Safe.to_string json)))


let read_assoc (srcpath : file_path) (assoc : assoc) =
  let open DecodeMD in
  let block = get_command srcpath '+' in
  let inline = get_command srcpath '\\' in
  let name k assoc =
    let json =
      assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, k)) k
    in
    let exn = InvalidValueForKey(srcpath, k) in
    match json with
    | `String(name) -> cut_module_names exn name
    | _             -> raise exn
  in
  let code_block assoc =
    assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, "code-block")) "code-block"
          |> make_code_name_map srcpath '+'
  in
  let code assoc =
    assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, "code")) "code"
          |> make_code_name_map srcpath '\\'
  in
  let string k assoc =
    let json =
      assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, k)) k
    in
    match json with
    | `String(s) -> s
    | _          -> raise (InvalidValueForKey(srcpath, k))
  in
  let cmdrcd =
    {
      document           = assoc |> name "document";
      header_default     = assoc |> string "header-default";

      paragraph          = assoc |> block "paragraph";
      hr                 = assoc |> block "hr";
      h1                 = assoc |> block "h1";
      h2                 = assoc |> block "h2";
      h3                 = assoc |> block "h3";
      h4                 = assoc |> block "h4";
      h5                 = assoc |> block "h5";
      h6                 = assoc |> block "h6";
      ul_inline          = assoc |> block "ul-inline";
      ul_block           = assoc |> block "ul-block";
      ol_inline          = assoc |> block "ol-inline";
      ol_block           = assoc |> block "ol-block";
      code_block_map     = assoc |> code_block;
      code_block_default = assoc |> block "code-block-default";
      blockquote         = assoc |> block "blockquote";
      err_block          = assoc |> block "err-block";

      emph               = assoc |> inline "emph";
      bold               = assoc |> inline "bold";
      hard_break         = assoc |> inline "hard-break";
      code_map           = assoc |> code;
      code_default       = assoc |> inline "code-default";
      url                = assoc |> inline "url";
      err_inline         = assoc |> inline "err-inline";
    }
  in
  let depends =
    let json =
      assoc |> MyYojsonUtil.find (MissingRequiredKey(srcpath, "depends")) "depends"
    in
    match json with
    | `List(jsons) ->
        jsons |> List.map (function `String(s) -> s | json -> raise (InvalidYOJSON(srcpath, "not a string: " ^ (Yojson.Safe.to_string json))))

    | _ ->
        raise (InvalidYOJSON(srcpath, "not a list: " ^ (Yojson.Safe.to_string json)))
  in
  (cmdrcd, depends)


let exception_for_multiple srcpath k =
  MultipleDesignation(srcpath, k)


let main (key : string) : DecodeMD.command_record * string list =
  let srcpath = Config.resolve_dist_file (Filename.concat "dist/md" (key ^ ".satysfi-md")) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
      let kvs = Yojson.Safe.Util.to_assoc json in
      let assoc = make_assoc (exception_for_multiple srcpath) kvs in
      read_assoc srcpath assoc
    with
    | Yojson.Json_error(msg)                 -> raise (InvalidYOJSON(srcpath, msg))
    | Yojson.Safe.Util.Type_error(msg, json) -> raise (InvalidYOJSON(srcpath, msg))
