
open MyUtil

module MYU = MyYojsonUtil
module YS = Yojson.SafePos
type json = YS.json

exception MultipleCodeNameDesignation of Range.t * string
exception NotAnInlineCommand          of Range.t * string
exception NotABlockCommand            of Range.t * string


let err_json msg json =
  raise (YS.Util.Type_error(msg, json))


let err_not_a_block_command rng s =
  raise (NotABlockCommand(rng, s))


let err_not_an_inline_command rng s =
  raise (NotAnInlineCommand(rng, s))


let pair_block = ('+', err_not_a_block_command)
let pair_inline = ('\\', err_not_an_inline_command)


let cut_module_names (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: mdlnmsrev -> (List.rev mdlnmsrev, varnm)
  | _                  -> assert false  (* -- 'String.split_on_char' always returns a non-empty list -- *)


let get_command_main ((prefix, errf) : char * (Range.t -> string -> 'a)) (rng : Range.t) (s : string) : DecodeMD.command =
  try
    if Char.equal (String.get s 0) prefix then
      let stail = (String.sub s 1 (String.length s - 1)) in
      let (mdlnms, varnm) = cut_module_names stail in
      (rng, (mdlnms, (String.make 1 prefix) ^ varnm))
    else
      errf rng s
  with
  | Invalid_argument(_) -> errf rng s


let get_command pair (k : string) (assoc : MYU.assoc) =
  let (pos, _) as json = assoc |> MYU.find k in
  let s = YS.Util.to_string json in
  let rng = MYU.make_range pos in
  get_command_main pair rng s


let make_code_name_map pair (json : json) : DecodeMD.command DecodeMD.CodeNameMap.t =
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
          let cmd = get_command_main pair rng2 s in
          accmap |> CodeNameMap.add name cmd

    | _ ->
        err_json "Expects a pair" json

  ) CodeNameMap.empty


let read_assoc (assoc : MYU.assoc) =
  let open DecodeMD in
  let block = get_command pair_block in
  let inline = get_command pair_inline in
  let inline_option k assoc =
    let json = assoc |> MYU.find k in
    json |> MYU.decode_variant [
      ("None", MYU.NoArg(fun () ->
        None
      ));
      ("Some", MYU.Arg(fun ((pos, _) as json) ->
        let rng = MYU.make_range pos in
        let s = json |> YS.Util.to_string in
        let cmd = get_command_main pair_inline rng s in
        Some(cmd)
      ));
    ]
  in
  let name k assoc =
    let (pos, _) as json = assoc |> MYU.find k in
    let rng = MYU.make_range pos in
    let name = json |> YS.Util.to_string in
    (rng, cut_module_names name)
  in
  let code_block assoc =
    assoc
      |> MYU.find "code-block"
      |> make_code_name_map pair_block
  in
  let code assoc =
    assoc
      |> MYU.find "code"
      |> make_code_name_map pair_inline
  in
  let string k assoc =
    assoc
      |> MYU.find k
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
      hard_break         = assoc |> inline_option "hard-break";
      code_map           = assoc |> code;
      code_default       = assoc |> inline "code-default";
      url                = assoc |> inline "url";
      reference          = assoc |> inline "reference";
      img                = assoc |> inline "img";
      embed_block        = assoc |> inline "embed-block";
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


let main (abspath : abs_path) : DecodeMD.command_record * string list =
  let pathstr = get_abs_path_string abspath in
  try
    let json = YS.from_file ~fname:pathstr pathstr in
      (* -- may raise 'Sys_error' or 'Yojson.Json_error' -- *)
    let assoc = MYU.make_assoc json in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error pathstr msg
