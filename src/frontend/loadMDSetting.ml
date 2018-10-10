
open MyUtil
open MyYojsonUtil


type file_path = string

exception MultipleDesignation of file_path * string
exception InvalidYOJSON       of file_path * string
exception InvalidEntry        of file_path * string


let cut_module_names (exn : exn) (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: mdlnmsrev -> (List.rev mdlnmsrev, varnm)
  | _                  -> raise exn


let get_command (exnf : string -> exn) (prefix : char) (k : string) (assoc : assoc) =
  let exn = exnf k in
  let json = assoc |> MyYojsonUtil.find exn k in
  match json with
  | `String(s) ->
      if Char.equal (String.get s 0) prefix then
        let stail = (String.sub s 1 (String.length s - 1)) in
        let (mdlnms, varnm) = cut_module_names exn stail in
        (mdlnms, "+" ^ varnm)
      else
        raise exn

  | _ ->
      raise exn


let read_assoc (srcpath : file_path) (assoc : assoc) =
  let open DecodeMD in
  let exnf k = InvalidEntry(srcpath, k) in
  let block = get_command exnf '+' in
  let inline = get_command exnf '\\' in
    {
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
      code_block_map     = CodeNameMap.empty;
      code_block_default = assoc |> block "code-block-default";

      emph               = assoc |> inline "emph";
      bold               = assoc |> inline "bold";
      hard_break         = assoc |> inline "hard-break";
      code_map           = CodeNameMap.empty;
      code_default       = assoc |> inline "code-inline-default";
    }


let exception_for_multiple srcpath k =
  MultipleDesignation(srcpath, k)


let main (key : string) =
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
