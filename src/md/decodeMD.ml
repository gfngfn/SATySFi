

type block_element =
  | Section of inline * block
  | Paragraph of inline
  | UlBlock of block list
  | UlInline of inline list
  | OlBlock of block list
  | OlInline of inline list
  | CodeBlock of Omd.name * string
  | Hr
  | BlockRaw of string

and block = block_element list

and inline_element =
  | Text of string
  | Emph of inline
  | Bold of inline
  | Code of Omd.name * string
  | Br
  | InlineRaw of string


and inline = inline_element list

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


let rec make_inline_element (mde : Omd.element) : inline_element =
  match mde with
  | Omd.H1(_) | Omd.H2(_) | Omd.H3(_) | Omd.H4(_) | Omd.H5(_) | Omd.H6(_)
      -> assert false  (* -- should be omitted by 'normalize_section' -- *)

  | Omd.Text(s)       -> Text(s)
  | Omd.Emph(md)      -> Emph(make_inline md)
  | Omd.Bold(md)      -> Bold(make_inline md)
  | Omd.Code(name, s) -> Code(name, s)
  | Omd.Br            -> Br
  | _                 -> InlineRaw(Omd.to_text [mde])
    (* temporary; should support more kinds of constructs *)


and make_inline (md : Omd.t) : inline =
  md |> List.map make_inline_element


let rec make_block_element (mde : Omd.element) : block_element =
  match mde with
  | Omd.H1(_) | Omd.H2(_) | Omd.H3(_) | Omd.H4(_) | Omd.H5(_) | Omd.H6(_)
      -> assert false  (* -- should be omitted by 'normalize_section' -- *)

  | Omd.Paragraph(md)       -> Paragraph(make_inline md)
  | Omd.Ul(mds)             -> UlInline(List.map make_inline mds)
  | Omd.Ulp(mds)            -> UlBlock(List.map make_block mds)
  | Omd.Ol(mds)             -> OlInline(List.map make_inline mds)
  | Omd.Olp(mds)            -> OlBlock(List.map make_block mds)
  | Omd.Code_block(name, s) -> CodeBlock(name, s)
  | Omd.Hr                  -> Hr
  | _                       -> BlockRaw(Omd.to_text [mde])


and make_block (md : Omd.t) : block =
  md |> List.map make_block_element


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


let normalize_h3 md =
  let (pre, inner) = normalize_section (function Omd.H3(heading) -> Some(heading) | _ -> None) md in
  List.append pre (inner |> List.map (fun (heading, mdsub) -> Section(heading, make_block mdsub)))


let normalize_h2 md =
  let (pre, inner) = normalize_section (function Omd.H2(heading) -> Some(heading) | _ -> None) md in
  List.append pre (inner |> List.map (fun (heading, mdsub) -> Section(heading, normalize_h3 mdsub)))


let normalize_h1 md =
  let (pre, inner) = normalize_section (function Omd.H1(heading) -> Some(heading) | _ -> None) md in
  List.append pre (inner |> List.map (fun (heading, mdsub) -> Section(heading, normalize_h2 mdsub)))


let decode s =
  let md = Omd.of_string s in
    normalize_h1 md
