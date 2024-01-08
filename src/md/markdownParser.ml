
open EnvelopeSystemBase
open Types


type error =
  | InvalidHeaderComment
  | InvalidExtraExpression

type 'a ok = ('a, error) result


type section_level =
  | H1 | H2 | H3 | H4 | H5 | H6

type block_element =
  | Section    of section_level * inline * block
  | Paragraph  of inline
  | Ul         of block list
  | Ol         of block list
  | Blockquote of block
  | CodeBlock  of string * string
  | Hr

and block =
  block_element list

and inline_element =
  | Text   of string
  | Emph   of inline
  | Strong of inline
  | Code   of string
  | Br
  | Link   of string * inline * string
  | Img    of string * string * string

and inline =
  inline_element list


let rec make_inline_of_element (oi : Omd.attributes Omd.inline) =
  let single ie = [ ie ] in
  match oi with
  | Omd.Concat(_attr, ois) ->
      make_inline ois

  | Omd.Text(_attr, s) ->
      single @@ Text(s)

  | Omd.Emph(_attr, oi) ->
      single @@ Emph(make_inline_of_element oi)

  | Omd.Strong(_attr, oi) ->
      single @@ Strong(make_inline_of_element oi)

  | Omd.Code(_attr, s) ->
      single @@ Code(s)

  | Omd.Hard_break(_attr) ->
      single @@ Br

  | Omd.Soft_break(_attr) ->
      single @@ Br

  | Omd.Link(_attr, { label; destination; title = title_opt }) ->
      let title = title_opt |> Option.value ~default:"" in (* TODO *)
      single @@ Link(destination, make_inline_of_element label, title)

  | Omd.Image(_attr, { label = _; destination; title = title_opt }) ->
      let alt = "" in (* TODO *)
      let title = title_opt |> Option.value ~default:"" in (* TODO *)
      single @@ Img(alt, destination, title)

  | Omd.Html(_attr, _s) ->
      failwith "make_inline_of_element, Omd.Html"


and make_inline (md : (Omd.attributes Omd.inline) list) : inline =
  md |> List.map make_inline_of_element |> List.concat


and make_block_of_element (ob : Omd.attributes Omd.block) : block =
  let single be = [ be ] in
  match ob with
  | Omd.Paragraph(_attr, oi) ->
      single @@ Paragraph(make_inline_of_element oi)

  | Omd.List(_attr, list_type, _list_spacing, obss) ->
      begin
        match list_type with
        | Omd.Ordered(_n, _ch) -> (* TODO *)
            single @@ Ol(obss |> List.map make_block)

        | Omd.Bullet(_ch) ->
            single @@ Ul(obss |> List.map make_block)
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

  | Omd.Table(_attr, _alignments, _cellss) ->
      failwith "make_block_of_element, Omd.Table"


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


let dummy_range = Range.dummy "Markdown-parser"


let make_list_tree utasts =
  List.fold_right (fun utast utast_tail ->
    (dummy_range, UTListCons(utast, utast_tail))
  ) utasts (dummy_range, UTEndOfList)


let make_inline_application (icmd : long_inline_command) (utasts : untyped_abstract_tree list) =
  let LongInlineCommand{ modules = modnms; main_without_prefix } = icmd in
  let csnm = Printf.sprintf "\\%s" main_without_prefix in
  let rng = Range.dummy "MarkdownParser.make_inline_application" in
  let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
  let utast_cmd = (rng, UTContentOf(modidents, (rng, csnm))) in
  [(dummy_range, UTInlineTextApplyCommand(utast_cmd, utasts |> List.map (fun x -> UTCommandArg([], x))))]


let make_block_application (bcmd : long_block_command) (utasts : untyped_abstract_tree list) =
  let LongBlockCommand{ modules = modnms; main_without_prefix } = bcmd in
  let csnm = Printf.sprintf "+%s" main_without_prefix in
  let rng = Range.dummy "MarkdownParser.make_block_application" in
  let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
  let utast_cmd = (rng, UTContentOf(modidents, (rng, csnm))) in
  [(dummy_range, UTBlockTextApplyCommand(utast_cmd, utasts |> List.map (fun x -> UTCommandArg([], x))))]


let rec convert_inline_element (conv : markdown_conversion) (ie : inline_element) : untyped_inline_text_element list =
  let
    MarkdownConversion{
      emph;
      strong;
      hard_break;
      code;
      link;
      img;
      _
    } = conv
  in
  match ie with
  | Text(s) ->
      [(dummy_range, UTInlineTextString(s))]

  | Emph(inline) ->
      let utast_arg = convert_inline conv inline in
      make_inline_application emph [ utast_arg ]

  | Strong(inline) ->
      let utast_arg = convert_inline conv inline in
      make_inline_application strong [ utast_arg ]


  | Code(s) ->
      let cmd = code in
      let utast_arg = (dummy_range, UTStringConstant(s)) in
      make_inline_application cmd [ utast_arg ]

  | Br ->
      begin
        match hard_break with
        | Some(cmd) -> make_inline_application cmd []
        | None      -> [ (dummy_range, UTInlineTextString("\n")) ]
      end

  | Link(href, inline, _title) ->
      let utast_arg1 = (dummy_range, UTStringConstant(href)) in
      let utast_arg2 = convert_inline conv inline in
      make_inline_application link [ utast_arg1; utast_arg2 ]

  | Img(alt, src, title) ->
      let utast_arg1 = (dummy_range, UTStringConstant(alt)) in
      let utast_arg2 = (dummy_range, UTStringConstant(src)) in
      let utast_arg3 = (dummy_range, UTStringConstant(title)) in
      make_inline_application img [ utast_arg1; utast_arg2; utast_arg3 ]


and convert_inline (conv : markdown_conversion) (inline : inline) : untyped_abstract_tree =
  let ibacc =
    inline |> List.fold_left (fun ibacc ie ->
      Alist.append ibacc (convert_inline_element conv ie)
    ) Alist.empty
  in
  let utih = Alist.to_list ibacc in
  (dummy_range, UTInlineText(utih))


and convert_block_element (conv : markdown_conversion) (be : block_element) : untyped_block_text_element list =
  let
    MarkdownConversion{
      paragraph;
      hr;
      h1; h2; h3; h4; h5; h6;
      ul;
      ol;
      code_block;
      blockquote;
      _
    } = conv
  in
  match be with
  | Paragraph(inline) ->
      let utast_arg = convert_inline conv inline in
      make_block_application paragraph [ utast_arg ]

  | Section(level, inline, block) ->
      let utast_arg1 = convert_inline conv inline in
      let utast_arg2 = convert_block conv block in
      let cmd =
        match level with
        | H1 -> h1
        | H2 -> h2
        | H3 -> h3
        | H4 -> h4
        | H5 -> h5
        | H6 -> h6
      in
      make_block_application cmd [ utast_arg1; utast_arg2 ]

  | Hr ->
      make_block_application hr []

  | Ol(blocks) ->
      let utasts = blocks |> List.map (convert_block conv) in
      let utast_arg = make_list_tree utasts in
      make_block_application ol [ utast_arg ]

  | Ul(blocks) ->
      let utasts = blocks |> List.map (convert_block conv) in
      let utast_arg = make_list_tree utasts in
      make_block_application ul [ utast_arg ]

  | CodeBlock(name, s) ->
      let utast_arg1 = (dummy_range, UTStringConstant(name)) in
      let utast_arg2 = (dummy_range, UTStringConstant(s)) in
      make_block_application code_block [ utast_arg1; utast_arg2 ]

  | Blockquote(block) ->
      let utast_arg = convert_block conv block in
      make_block_application blockquote [ utast_arg ]


and convert_block (conv : markdown_conversion) (block : block) : untyped_abstract_tree =
  let bbacc =
    block |> List.fold_left (fun bbacc be ->
      Alist.append bbacc (convert_block_element conv be)
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


let parse_expression (s_expr : string) : untyped_abstract_tree ok =
  let open ResultMonad in
  match ParserInterface.process_text "(markdown)" s_expr with
  | Ok(UTDocumentFile([], [], utast)) -> return utast
  | _                                 -> err InvalidExtraExpression


type t = {
  class_module_name : module_name;
  extra_expression  : untyped_abstract_tree;
  main_contents     : block;
}


let decode (s : string) : t ok =
  let open ResultMonad in
  let obs = Omd.of_string s in
  let* (class_module_name, s_extra, obs) =
    match obs with
    | Omd.Html_block(_attr1, s1) :: Omd.Html_block(_attr2, s2) :: obs ->
        begin
          match (extract_comment s1, extract_comment s2) with
          | (Some(modnm), Some(s_extra)) -> return (modnm, s_extra, obs)
          | _                            -> err InvalidHeaderComment
              (* TODO: assert that `modnm` be a capitalized identifier *)
        end

    | _ ->
        err InvalidHeaderComment
  in
  let* utast_extra = parse_expression s_extra in
  return {
    class_module_name = class_module_name;
    extra_expression  = utast_extra;
    main_contents     = normalize_h1 obs;
  }


let get_class_module_name (md : t) : module_name =
  md.class_module_name


let convert (conv : markdown_conversion) (md : t) =
  let utast_body = convert_block conv md.main_contents in
  let utast_doccmd =
    let MarkdownConversion{ document; _ } = conv in
    let LongIdentifier{ modules = modnms; main = varnm } = document in
    let rng = Range.dummy "MarkdownParser.convert" in
    let modidents = modnms |> List.map (fun modnm -> (rng, modnm)) in
    (rng, UTContentOf(modidents, (rng, varnm)))
  in
  let utast_extra = md.extra_expression in
  (dummy_range, UTApply([], (dummy_range, UTApply([], utast_doccmd, utast_extra)), utast_body))
