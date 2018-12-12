
open Types


type section_level =
  | H1 | H2 | H3 | H4 | H5 | H6
[@@deriving show { with_path = false; }]

type block_element =
  | Section of section_level * inline * block
  | Paragraph of inline
  | UlBlock of block list
  | UlInline of inline list
  | OlBlock of block list
  | OlInline of inline list
  | Blockquote of block
  | CodeBlock of Omd.name * string
      [@printer (fun fmt (name, s) -> Format.fprintf fmt "CodeBlock(\"%s\",@ \"%s\")" name s)]
  | Hr
  | BlockRaw of string

and block = block_element list

and inline_element =
  | Text of string
  | Emph of inline
  | Bold of inline
  | Code of Omd.name * string
      [@printer (fun fmt (name, s) -> Format.fprintf fmt "Code(\"%s\",@ \"%s\")" name s)]
  | Br
  | Url of string * inline * string
  | InlineRaw of string
  | EmbeddedBlock of block

and inline = inline_element list
[@@deriving show { with_path = false; }]

type middle_record =
  {
    pre_contents        : block;
    current_heading     : Omd.t;
    current_accumulated : Omd.element Alist.t;
    accumulated         : (inline * Omd.t) Alist.t;
  }

type accumulator =
  | Beginning of Omd.element Alist.t
  | Middle    of middle_record


let rec make_inline_of_element (mde : Omd.element) =
  let single ilne = [ilne] in
  let empty = [] in
  match mde with
  | Omd.H1(_) | Omd.H2(_) | Omd.H3(_) | Omd.H4(_) | Omd.H5(_) | Omd.H6(_)
      -> assert false  (* -- should be omitted by 'normalize_section' -- *)

  | Omd.Paragraph(_)
  | Omd.Code_block(_)
  | Omd.Html_block(_)
  | Omd.Blockquote(_)
  | Omd.Hr
  | Omd.Ul(_)
  | Omd.Ulp(_)
  | Omd.Ol(_)
  | Omd.Olp(_)
  | Omd.Raw_block(_) ->
      single @@ EmbeddedBlock(make_block_of_element mde)

  | Omd.Html_comment(_) ->
      empty

  | Omd.Html(_) ->
      failwith ("HTML; remains to be supported: " ^ Omd.to_text [mde])

  | Omd.X(_) ->
      failwith ("extension; remains to be supported: " ^ Omd.to_text [mde])

  | Omd.Text(s)       -> single @@ Text(s)
  | Omd.Emph(md)      -> single @@ Emph(make_inline md)
  | Omd.Bold(md)      -> single @@ Bold(make_inline md)
  | Omd.Code(name, s) -> single @@ Code(name, s)
  | Omd.Br            -> single @@ Br
  | Omd.NL            -> single @@ Br

  | Omd.Url(href, md, title) ->
      single @@ Url(href, make_inline md, title)

  | Omd.Ref(_, name, s, _) ->
      failwith (Printf.sprintf "Ref; remains to be supported: name='%s', s='%s'" name s)

  | Omd.Img(alt, src, title) ->
      failwith (Printf.sprintf "Img; remiains to be supported: alt='%s', src'%s', title='%s'" alt src title)

  | Omd.Img_ref(_, name, alt, _) ->
      failwith (Printf.sprintf "Img_ref; remains to be supported: name='%s', alt='%s'" name alt)

  | Omd.Raw(s) ->
      single @@ InlineRaw(s)


and make_inline (md : Omd.t) : inline =
  md |> List.map make_inline_of_element |> List.concat


and make_block_of_element (mde : Omd.element) =
  let single blke = [blke] in
  let empty = [] in
  match mde with
  | Omd.H1(_) | Omd.H2(_) | Omd.H3(_) | Omd.H4(_) | Omd.H5(_) | Omd.H6(_) ->
      assert false
        (* -- should be omitted by 'normalize_section' -- *)

  | Omd.Text(_)
  | Omd.Emph(_)
  | Omd.Bold(_)
  | Omd.Code(_)
  | Omd.Url(_)
  | Omd.Ref(_)
  | Omd.Img(_)
  | Omd.Img_ref(_)
  | Omd.Raw(_)
  | Omd.Html(_) ->
      Format.printf "! [Warning] not a block: %s@," (Omd.to_text [mde]);
        (* temporary; should warn in a more sophisticated manner *)
      single @@ Paragraph(make_inline [mde])

  | Omd.Br
  | Omd.NL ->
      empty

  | Omd.Html_comment(s) ->
      Format.printf "  [Comment] %s@," s;  (* TEMPORARY *)
      empty

  | Omd.Html_block(_) ->
      failwith ("HTML block; remains to be supported: " ^ Omd.to_text [mde])

  | Omd.X(_) ->
      failwith ("extension; remains to be supported: " ^ Omd.to_text [mde])

  | Omd.Paragraph(md)       -> single @@ Paragraph(make_inline md)
  | Omd.Ul(mds)             -> single @@ UlInline(List.map make_inline mds)
  | Omd.Ulp(mds)            -> single @@ UlBlock(List.map make_block mds)
  | Omd.Ol(mds)             -> single @@ OlInline(List.map make_inline mds)
  | Omd.Olp(mds)            -> single @@ OlBlock(List.map make_block mds)
  | Omd.Code_block(name, s) -> single @@ CodeBlock(name, s)
  | Omd.Hr                  -> single @@ Hr
  | Omd.Blockquote(md)      -> single @@ Blockquote(make_block md)
  | Omd.Raw_block(s)        -> single @@ BlockRaw(s)


and make_block (md : Omd.t) : block =
  md |> List.map make_block_of_element |> List.concat


let finish_section (midrcd : middle_record) : (inline * Omd.t) Alist.t =
  let inner = Alist.to_list midrcd.current_accumulated in
  let pair = (make_inline midrcd.current_heading, inner) in
  Alist.extend midrcd.accumulated pair


let normalize_section nomf (md : Omd.t) =
  let acc =
    md |> List.fold_left (fun acc mde ->
      match nomf mde with
      | Some(heading) ->
          begin
            match acc with
            | Beginning(eacc) ->
                Middle{
                  pre_contents        = make_block (Alist.to_list eacc);
                  current_heading     = heading;
                  current_accumulated = Alist.empty;
                  accumulated         = Alist.empty;
                }

            | Middle(midrcd) ->
                let mainacc = finish_section midrcd in
                Middle{
                  pre_contents        = midrcd.pre_contents;
                  current_heading     = heading;
                  current_accumulated = Alist.empty;
                  accumulated         = mainacc;
                }
          end

      | None ->
          begin
            match acc with
            | Beginning(eacc) ->
                Beginning(Alist.extend eacc mde)

            | Middle(midrcd) ->
                Middle({ midrcd with
                  current_accumulated = Alist.extend midrcd.current_accumulated mde;
                })
          end

    ) (Beginning(Alist.empty))
  in
  match acc with
  | Beginning(eacc) ->
      (make_block (Alist.to_list eacc), [])

  | Middle(midrcd) ->
      let mainacc = finish_section midrcd in
      (midrcd.pre_contents, Alist.to_list mainacc)


let normalize_h seclev nomf subf md =
  let (pre, inner) = normalize_section nomf md in
  List.append pre (inner |> List.map (fun (heading, mdsub) -> Section(seclev, heading, subf mdsub)))


let normalize_h6 = normalize_h H6 (function Omd.H6(heading) -> Some(heading) | _ -> None) make_block
let normalize_h5 = normalize_h H5 (function Omd.H5(heading) -> Some(heading) | _ -> None) normalize_h6
let normalize_h4 = normalize_h H4 (function Omd.H4(heading) -> Some(heading) | _ -> None) normalize_h5
let normalize_h3 = normalize_h H3 (function Omd.H3(heading) -> Some(heading) | _ -> None) normalize_h4
let normalize_h2 = normalize_h H2 (function Omd.H2(heading) -> Some(heading) | _ -> None) normalize_h3
let normalize_h1 = normalize_h H1 (function Omd.H1(heading) -> Some(heading) | _ -> None) normalize_h2


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
  code_map           : command CodeNameMap.t;
  code_default       : command;
  url                : command;
  embed_block        : command;
  err_inline         : command;
}


let dummy_range = Range.dummy "decodeMD"


let make_list_tree utastlst =
  List.fold_right (fun utast utasttail ->
    (dummy_range, UTListCons(utast, utasttail))
  ) utastlst (dummy_range, UTEndOfList)


let make_inline_application ((rng, (mdlnmlst, cmdnm)) : command) (utasts : untyped_abstract_tree list) =
  let utastcmd = (rng, UTContentOf(mdlnmlst, cmdnm)) in
  [(dummy_range, UTInputHorzEmbedded(utastcmd, utasts |> List.map (fun x -> UTMandatoryArgument(x))))]


let make_block_application ((rng, (mdlnmlst, cmdnm)) : command) (utasts : untyped_abstract_tree list) =
  let utastcmd = (rng, UTContentOf(mdlnmlst, cmdnm)) in
  [(dummy_range, UTInputVertEmbedded(utastcmd, utasts |> List.map (fun x -> UTMandatoryArgument(x))))]


let rec convert_inline_element (cmdrcd : command_record) (ilne : inline_element) : untyped_input_horz_element list =
  match ilne with
  | Text(s) ->
      [(dummy_range, UTInputHorzText(s))]

  | Emph(iln) ->
      let utastarg = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.emph [utastarg]

  | Bold(iln) ->
      let utastarg = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.bold [utastarg]


  | Code(name, s) ->
      let cmd =
        if String.equal name "" then
          cmdrcd.code_default
        else
          match cmdrcd.code_map |> CodeNameMap.find_opt name with
          | None ->
              Format.printf "! Warning: unknown name '%s' for inline code\n" name;
                (* -- temporary; should warn in a more sophisticated manner -- *)
              cmdrcd.code_default

          | Some(cmd) ->
              cmd
      in
      let utastarg = (dummy_range, UTStringConstant(s)) in
      make_inline_application cmd [utastarg]

  | Br ->
      begin
        match cmdrcd.hard_break with
        | Some(cmd) -> make_inline_application cmd []
        | None      -> [(dummy_range, UTInputHorzText("\n"))]
      end

  | Url(href, iln, title) ->
      let utastarg1 = (dummy_range, UTStringConstant(href)) in
      let utastarg2 = convert_inline cmdrcd iln in
      make_inline_application cmdrcd.url [utastarg1; utastarg2]

  | EmbeddedBlock(blk) ->
      let utastarg = convert_block cmdrcd blk in
      make_inline_application cmdrcd.embed_block [utastarg]

  | InlineRaw(s) ->
      let utastarg = (dummy_range, UTStringConstant(s)) in
      make_inline_application cmdrcd.err_inline [utastarg]


and convert_inline (cmdrcd : command_record) (iln : inline) : untyped_abstract_tree =
  let ibacc =
    iln |> List.fold_left (fun ibacc ilne ->
      Alist.append ibacc (convert_inline_element cmdrcd ilne)
    ) Alist.empty
  in
  let utih = Alist.to_list ibacc in
  (dummy_range, UTInputHorz(utih))


and convert_block_element (cmdrcd : command_record) (blke : block_element) : untyped_input_vert_element list =
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

  | OlInline(ilns) ->
      let utastlst = List.map (convert_inline cmdrcd) ilns in
      let utastarg = make_list_tree utastlst in
      make_block_application cmdrcd.ol_inline [utastarg]

  | OlBlock(blks) ->
      let utastlst = List.map (convert_block cmdrcd) blks in
      let utastarg = make_list_tree utastlst in
      make_block_application cmdrcd.ol_block [utastarg]

  | UlInline(ilns) ->
      let utastlst = List.map (convert_inline cmdrcd) ilns in
      let utastarg = make_list_tree utastlst in
      make_block_application cmdrcd.ul_inline [utastarg]

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

  | BlockRaw(s) ->
      let cmd = cmdrcd.err_block in
      make_block_application cmd [(dummy_range, UTInputHorz[(dummy_range, UTInputHorzText(s))])]


and convert_block (cmdrcd : command_record) (blk : block) : untyped_abstract_tree =
  let bbacc =
    blk |> List.fold_left (fun bbacc blke ->
      Alist.append bbacc (convert_block_element cmdrcd blke)
    ) Alist.empty
  in
  let utiv = Alist.to_list bbacc in
  (dummy_range, UTInputVert(utiv))


let decode (cmdrcd : command_record) (s : string) =
  let utastdoccmd =
    let (rng, (mdlnms, varnm)) = cmdrcd.document in
    (rng, UTContentOf(mdlnms, varnm))
  in
  let md = Omd.of_string s in
  let (strheader, md) =
    match md with
    | Omd.Html_comment(s) :: md ->
        let len = String.length s in
        let s =
          if len < 8 then
            assert false
          else
            String.sub s 4 (len - 8)
        in
(*
        Format.printf "  [Header] %s@," s;  (* for debug *)
 *)
        (s, md)

    | _ ->
        (cmdrcd.header_default, md)
  in
  let lexbuf = Lexing.from_string strheader in
  let (_, utasthead) = ParserInterface.process "(markdown)" lexbuf in
  let blk = normalize_h1 md in
(*
  Format.printf "BLOCK: %a\n" pp_block blk;  (* for debug *)
 *)
  let utastbody = convert_block cmdrcd blk in
  (dummy_range, UTApply((dummy_range, UTApply(utastdoccmd, utasthead)), utastbody))
