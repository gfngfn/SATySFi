
open MyUtil

module MYU = MyYojsonUtil
module YS = Yojson.SafePos
type json = YS.json

type file_path = string

exception MultipleCodeNameDesignation of Range.t * string
exception NotACommandName             of Range.t * string


let err_not_a_command rng s =
  raise (NotACommandName(rng, s))


let cut_module_names (rng : Range.t) (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: mdlnmsrev -> (List.rev mdlnmsrev, varnm)
  | _                  -> err_not_a_command rng s


let get_command_main (rng : Range.t) (prefix : char) (s : string) =
  try
    if Char.equal (String.get s 0) prefix then
      let stail = (String.sub s 1 (String.length s - 1)) in
      let (mdlnms, varnm) = cut_module_names rng stail in
      (mdlnms, (String.make 1 prefix) ^ varnm)
    else
      err_not_a_command rng s
  with
  | Invalid_argument(_) -> err_not_a_command rng s


let get_command (srcpath : file_path) (prefix : char) (k : string) (assoc : MYU.assoc) =
  let (pos, s) =
    let (pos, _) as json =
      assoc |> MYU.find k
    in
      (pos, YS.Util.to_string json)
  in
  let rng = MYU.make_range pos in
  get_command_main rng prefix s


let make_code_name_map (srcpath : file_path) (prefix : char) ((pos, _) as json : json) : (string list * string) DecodeMD.CodeNameMap.t =
  let open DecodeMD in
  let pairs = json |> YS.Util.to_list in
  pairs |> List.fold_left (fun accmap json ->
    match json with
    | (pos, `Tuple(json1 :: ((pos2, _) as json2) :: [])) ->
        let name = json1 |> YS.Util.to_string in
        let s = json2 |> YS.Util.to_string in
        let rng2 = MYU.make_range pos2 in
        if accmap |> CodeNameMap.mem name then
          let rng = MYU.make_range pos in
          raise (MultipleCodeNameDesignation(rng, name))
        else
          let cmd = get_command_main rng2 prefix s in
          accmap |> CodeNameMap.add name cmd

    | _ ->
        raise (YS.Util.Type_error("Expecting a pair", json))

  ) CodeNameMap.empty


let read_assoc (srcpath : file_path) (assoc : MYU.assoc) =
  let open DecodeMD in
  let block = get_command srcpath '+' in
  let inline = get_command srcpath '\\' in
  let name k assoc =
    let (pos, _) as json =
      assoc |> MYU.find k
    in
    let rng = MYU.make_range pos in
    let name = json |> YS.Util.to_string in
    cut_module_names rng name
  in
  let code_block assoc =
    assoc |> MYU.find "code-block"
          |> make_code_name_map srcpath '+'
  in
  let code assoc =
    assoc |> MYU.find "code"
          |> make_code_name_map srcpath '\\'
  in
  let string k assoc =
    assoc |> MYU.find k
          |> YS.Util.to_string
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
    assoc
      |> MYU.find "depends"
      |> YS.Util.to_list
      |> List.map YS.Util.to_string
  in
  (cmdrcd, depends)


let main (key : string) : DecodeMD.command_record * string list =
  let srcpath = Config.resolve_dist_file (Filename.concat "dist/md" (key ^ ".satysfi-md")) in
  try
    let json = YS.from_file ~fname:srcpath srcpath in
      (* -- may raise 'Sys_error' -- *)
    let assoc = MYU.make_assoc json in
    read_assoc srcpath assoc
  with
  | Yojson.Json_error(msg) -> raise (MYU.SyntaxError(srcpath, msg))
