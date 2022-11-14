
open Types


type section_level =
  | H1 | H2 | H3 | H4 | H5 | H6

type block_element =
  | Section of section_level * inline * block
  | Paragraph of inline
  | UlBlock of block list
  | OlBlock of block list
  | Blockquote of block
  | CodeBlock of string * string
  | Hr

and block =
  block_element list

and inline_element =
  | Text of string
  | Emph of inline
  | Bold of inline
  | Code of string
  | Br
  | Url of string * inline * string
  | Img of string * string * string

and inline =
  inline_element list


let rec make_inline_of_element (oi : Omd.attributes Omd.inline) =
  let single ilne = [ ilne ] in
  match oi with
  | Omd.Concat(_attr, ois) ->
      make_inline ois

  | Omd.Text(_attr, s) ->
      single @@ Text(s)

  | Omd.Emph(_attr, oi) ->
      single @@ Emph(make_inline_of_element oi)

  | Omd.Strong(_attr, oi) ->
      single @@ Bold(make_inline_of_element oi)

  | Omd.Code(_attr, s) ->
      single @@ Code(s)

  | Omd.Hard_break(_attr) ->
      single @@ Br

  | Omd.Soft_break(_attr) ->
      single @@ Br

  | Omd.Link(_attr, { label; destination; title = title_opt }) ->
      let title = title_opt |> Option.value ~default:"" in (* TODO *)
      single @@ Url(destination, make_inline_of_element label, title)

  | Omd.Image(_attr, { label = _; destination; title = title_opt }) ->
      let alt = "" in (* TODO *)
      let title = title_opt |> Option.value ~default:"" in (* TODO *)
      single @@ Img(alt, destination, title)

  | Omd.Html(_attr, _s) ->
      failwith "make_inline_of_element, Omd.Html"


and make_inline (md : (Omd.attributes Omd.inline) list) : inline =
  md |> List.map make_inline_of_element |> List.concat


and make_block_of_element (ob : Omd.attributes Omd.block) : block =
  let single blke = [ blke ] in
  match ob with
  | Omd.Paragraph(_attr, oi) ->
      single @@ Paragraph(make_inline_of_element oi)

  | Omd.List(_attr, list_type, _list_spacing, obss) ->
      begin
        match list_type with
        | Omd.Ordered(_n, _ch) -> (* TODO *)
            single @@ OlBlock(obss |> List.map make_block)

        | Omd.Bullet(_ch) ->
            single @@ UlBlock(obss |> List.map make_block)
      end

  | Omd.Blockquote(_attr, obs) ->
      single @@ Blockquote(make_block obs)

  | Omd.Thematic_break(_attr) ->
      single @@ Hr

  | Omd.Heading(_attr, _level, _oi) ->
      assert false
        (* Must be omitted by `normalize_section` *)

  | Omd.Code_block(_attr, name, s) ->
      single @@ CodeBlock(name, s)

  | Omd.Html_block(_attr, _s) ->
      failwith "make_block_of_element, Omd.Html_block"

  | Omd.Definition_list(_attr, _defs) ->
      failwith "make_block_of_element, Omd.Definition_list"


and make_block (md : (Omd.attributes Omd.block) list) : block =
  md |> List.map make_block_of_element |> List.concat


type middle_record = {
  pre_contents        : (Omd.attributes Omd.block) list;
  current_heading     : inline;
  current_accumulated : (Omd.attributes Omd.block) Alist.t;
  accumulated         : (inline * (Omd.attributes Omd.block) list) Alist.t;
}

type accumulator =
  | Beginning of (Omd.attributes Omd.block) Alist.t
  | Middle    of middle_record


let finish_section (midrcd : middle_record) : (inline * (Omd.attributes Omd.block) list) Alist.t =
  let inner = Alist.to_list midrcd.current_accumulated in
  let pair = (midrcd.current_heading, inner) in
  Alist.extend midrcd.accumulated pair


let normalize_section (nomf : Omd.attributes Omd.block -> (Omd.attributes Omd.inline) option) (obs : (Omd.attributes Omd.block) list) =
  let acc =
    obs |> List.fold_left (fun acc ob ->
      match nomf ob with
      | Some(heading) ->
          begin
            match acc with
            | Beginning(eacc) ->
                Middle{
                  pre_contents        = Alist.to_list eacc;
                  current_heading     = make_inline_of_element heading;
                  current_accumulated = Alist.empty;
                  accumulated         = Alist.empty;
                }

            | Middle(midrcd) ->
                let mainacc = finish_section midrcd in
                Middle{
                  pre_contents        = midrcd.pre_contents;
                  current_heading     = make_inline_of_element heading;
                  current_accumulated = Alist.empty;
                  accumulated         = mainacc;
                }
          end

      | None ->
          begin
            match acc with
            | Beginning(eacc) ->
                Beginning(Alist.extend eacc ob)

            | Middle(midrcd) ->
                Middle({ midrcd with
                  current_accumulated = Alist.extend midrcd.current_accumulated ob;
                })
          end

    ) (Beginning(Alist.empty))
  in
  match acc with
  | Beginning(eacc) ->
      (Alist.to_list eacc, [])

  | Middle(midrcd) ->
      let mainacc = finish_section midrcd in
      (midrcd.pre_contents, Alist.to_list mainacc)


let normalize_h (seclev : section_level) (nomf : Omd.attributes Omd.block -> (Omd.attributes Omd.inline) option) (subf : (Omd.attributes Omd.block) list -> block) md =
  let (pre, inner) = normalize_section nomf md in
  List.append (subf pre) (inner |> List.map (fun (heading, mdsub) -> Section(seclev, heading, subf mdsub)))


let normalize_h6 =
  normalize_h H6 (function Omd.Heading(_, 6, oi_heading) -> Some(oi_heading) | _ -> None) make_block

let normalize_h5 =
  normalize_h H5 (function Omd.Heading(_, 5, heading) -> Some(heading) | _ -> None) normalize_h6

let normalize_h4 =
  normalize_h H4 (function Omd.Heading(_, 4, heading) -> Some(heading) | _ -> None) normalize_h5

let normalize_h3 =
  normalize_h H3 (function Omd.Heading(_, 3, heading) -> Some(heading) | _ -> None) normalize_h4

let normalize_h2 =
  normalize_h H2 (function Omd.Heading(_, 2, heading) -> Some(heading) | _ -> None) normalize_h3

let normalize_h1 =
  normalize_h H1 (function Omd.Heading(_, 1, heading) -> Some(heading) | _ -> None) normalize_h2


module CodeNameMap = Map.Make(String)


type command = Range.t * (module_name list * var_name)

type command_record = {
  document           : command;
  header_default     : string;

  paragraph          : command;
  hr                 : command;
  h1                 : command;
  h2                 : command;
  h3                 : command;
  h4                 : command;
  h5                 : command;
  h6                 : command;
  ul_inline          : command;
  ul_block           : command;
  ol_inline          : command;
  ol_block           : command;
  code_block_map     : command CodeNameMap.t;
  code_block_default : command;
  blockquote         : command;
  err_block          : command;

  emph               : command;
  bold               : command;
  hard_break         : command option;
  code_default       : command;
  url                : command;
  reference          : command;
  img                : command;
  embed_block        : command;
  err_inline         : command;
}


let dummy_range = Range.dummy "decodeMD"


let make_list_tree utastlst =
  List.fold_right (fun utast utasttail ->
    (dummy_range, UTListCons(utast, utasttail))
  ) utastlst (dummy_range, UTEndOfList)


let make_inline_application ((rng, (modnms, csnm)) : command) (utasts : untyped_abstract_tree list) =
  let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
  let utast_cmd = (rng, UTContentOf(modidents, (rng, csnm))) in
  [(dummy_range, UTInlineTextApplyCommand(utast_cmd, utasts |> List.map (fun x -> UTCommandArg([], x))))]


let make_block_application ((rng, (modnms, csnm)) : command) (utasts : untyped_abstract_tree list) =
  let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
  let utast_cmd = (rng, UTContentOf(modidents, (rng, csnm))) in
  [(dummy_range, UTBlockTextApplyCommand(utast_cmd, utasts |> List.map (fun x -> UTCommandArg([], x))))]


let rec convert_inline_element (cmdrcd : command_record) (ilne : inline_element) : untyped_inline_text_element list =
  match ilne with
  | Text(s) ->
      [(dummy_range, UTInlineTextString(s))]

  | Emph(iln) ->
      let utastarg = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.emph [utastarg]

  | Bold(iln) ->
      let utastarg = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.bold [utastarg]


  | Code(s) ->
      let cmd = cmdrcd.code_default in
      let utastarg = (dummy_range, UTStringConstant(s)) in
      make_inline_application cmd [utastarg]

  | Br ->
      begin
        match cmdrcd.hard_break with
        | Some(cmd) -> make_inline_application cmd []
        | None      -> [(dummy_range, UTInlineTextString("\n"))]
      end

  | Url(href, iln, _title) ->
      let utastarg1 = (dummy_range, UTStringConstant(href)) in
      let utastarg2 = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.url [utastarg1; utastarg2]

  | Img(alt, src, title) ->
      let utastarg1 = (dummy_range, UTStringConstant(alt)) in
      let utastarg2 = (dummy_range, UTStringConstant(src)) in
      let utastarg3 = (dummy_range, UTStringConstant(title)) in
      make_inline_application cmdrcd.img [utastarg1; utastarg2; utastarg3]


and convert_inline (cmdrcd : command_record) (iln : inline) : untyped_abstract_tree =
  let ibacc =
    iln |> List.fold_left (fun ibacc ilne ->
      Alist.append ibacc (convert_inline_element cmdrcd ilne)
    ) Alist.empty
  in
  let utih = Alist.to_list ibacc in
  (dummy_range, UTInlineText(utih))


and convert_block_element (cmdrcd : command_record) (blke : block_element) : untyped_block_text_element list =
  match blke with
  | Paragraph(iln) ->
      let utastarg = convert_inline cmdrcd iln in
      make_block_application cmdrcd.paragraph [utastarg]

  | Section(seclev, iln, blk) ->
      let utastarg1 = convert_inline cmdrcd iln in
      let utastarg2 = convert_block cmdrcd blk in
      let cmd =
        match seclev with
        | H1 -> cmdrcd.h1
        | H2 -> cmdrcd.h2
        | H3 -> cmdrcd.h3
        | H4 -> cmdrcd.h4
        | H5 -> cmdrcd.h5
        | H6 -> cmdrcd.h6
      in
      make_block_application cmd [utastarg1; utastarg2]

  | Hr ->
      make_block_application cmdrcd.hr []

  | OlBlock(blks) ->
      let utastlst = List.map (convert_block cmdrcd) blks in
      let utastarg = make_list_tree utastlst in
      make_block_application cmdrcd.ol_block [utastarg]

  | UlBlock(blks) ->
      let utastlst = List.map (convert_block cmdrcd) blks in
      let utastarg = make_list_tree utastlst in
      make_block_application cmdrcd.ul_block [utastarg]

  | CodeBlock(name, s) ->
      let utastarg = (dummy_range, UTStringConstant(s)) in
      let cmd =
        if String.equal name "" then
          cmdrcd.code_block_default
        else
          match cmdrcd.code_block_map |> CodeNameMap.find_opt name with
          | None ->
              Format.printf "! Warning: unknown name '%s' for code block\n" name;
                (* temporary; should warn in a more sophisticated manner *)
              cmdrcd.code_block_default

          | Some(cmd) ->
              cmd
      in
      make_block_application cmd [utastarg]

  | Blockquote(blk) ->
      let utastarg = convert_block cmdrcd blk in
      make_block_application cmdrcd.blockquote [utastarg]


and convert_block (cmdrcd : command_record) (blk : block) : untyped_abstract_tree =
  let bbacc =
    blk |> List.fold_left (fun bbacc blke ->
      Alist.append bbacc (convert_block_element cmdrcd blke)
    ) Alist.empty
  in
  let utiv = Alist.to_list bbacc in
  (dummy_range, UTBlockText(utiv))


let extract_comment (s : string) : string option =
  let s = String.trim s in
  if Core.String.is_prefix s ~prefix:"<!--" && Core.String.is_suffix s ~suffix:"-->" then
    let len = String.length s in
    if len < 8 then
      None
    else
      Some(String.trim (String.sub s 4 (len - 8)))
  else
    None


let parse_expression (s_expr : string) : untyped_abstract_tree =
  match ParserInterface.process_text "(markdown)" s_expr with
  | Ok(UTDocumentFile([], [], utast)) -> utast
  | _                                 -> failwith "TODO (error): invalid header expression"


type t = {
  extra_expression : untyped_abstract_tree;
  main_contents    : block;
}


let decode (s : string) : DocumentAttribute.t * module_name * t =
  let obs = Omd.of_string s in
  let (s_dependencies, modnm, s_extra, obs) =
    match obs with
    | Omd.Html_block(_attr1, s1) :: Omd.Html_block(_attr2, s2) :: Omd.Html_block(_attr3, s3) :: obs ->
        Format.printf "**** STR: %s, %s, %s\n" s1 s2 s3;
        begin
          match (extract_comment s1, extract_comment s2, extract_comment s3) with
          | (Some(s_dependencies), Some(modnm), Some(s_extra)) ->
              Format.printf "**** EXTR: %s, %s, %s\n" s_dependencies modnm s_extra;
              (s_dependencies, modnm, s_extra, obs)

          | (opt1, opt2, opt3) ->
              Format.printf "**** EXTR: %a, %a, %a\n"
                Format.(pp_print_option pp_print_string) opt1
                Format.(pp_print_option pp_print_string) opt2
                Format.(pp_print_option pp_print_string) opt3;
              failwith "TODO (error): not a comment 1"
        end

    | _ ->
        failwith "TODO (error): not a comment 2"
  in
  let utast_dependencies = parse_expression s_dependencies in
  let utast_extra = parse_expression s_extra in
  let main_contents = normalize_h1 obs in
  let document_attributes_res =
    DocumentAttribute.make [
      (dummy_range, UTAttribute("dependencies", utast_dependencies))
    ]
  in
  match document_attributes_res with
  | Error(_) ->
      failwith "TODO (error): failed to decode document attributes"

  | Ok(document_attributes) ->
      let md =
        {
          extra_expression = utast_extra;
          main_contents;
        }
      in
      (document_attributes, modnm, md)


let convert (cmdrcd : command_record) (md : t) =
  let utast_body = convert_block cmdrcd md.main_contents in
  let utast_doccmd =
    let (rng, (modnms, varnm)) = cmdrcd.document in
    let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
    (rng, UTContentOf(modidents, (rng, varnm)))
  in
  let utast_extra = md.extra_expression in
  (dummy_range, UTApply([], (dummy_range, UTApply([], utast_doccmd, utast_extra)), utast_body))
