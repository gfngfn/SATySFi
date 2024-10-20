
open CharBasis
open LengthInterface
open LineBreakBox
open HorzBox


let normalize_script (ctx : context_main) (script_raw : script) : script =
  match script_raw with
  | CharBasis.CommonNarrow
  | CharBasis.Inherited    -> ctx.dominant_narrow_script
  | CharBasis.CommonWide   -> ctx.dominant_wide_script
  | _                      -> script_raw



let find (ctx : context_main) ((uch, _) : uchar_segment) : script =
  match ctx.script_map |> ScriptDataMap.find uch with
  | None         -> OtherScript
  | Some(script) -> normalize_script ctx script


let divide_by_script (ctx : context_main) (trilst : line_break_element list) : line_break_chunk_main list =

  let ideographic script lbc uchseg alw =
    IdeographicChunk(script, lbc, uchseg, alw)
  in

  let preword script lbcfirst lbclast uchseglst alw =
    AlphabeticChunk(script, lbcfirst, lbclast, uchseglst, alw)
  in

  let rec aux resacc (scraccopt : (line_break_class * script * line_break_class * uchar_segment list) option) trilst =
    match trilst with
    | [] ->
        begin
          match scraccopt with
          | None ->
              Alist.to_list resacc

          | Some((lbcfirst, scriptprev, lbcprev, uchsegacc)) ->
              let chunk = preword scriptprev lbcfirst lbcprev (List.rev uchsegacc) PreventBreak in
              Alist.to_list (Alist.extend resacc chunk)
        end

    | (_uch, SP, alw) :: tritail ->
        let chunkspace =
          match alw with
          | AllowBreak   -> Space
          | PreventBreak -> UnbreakableSpace
        in
        begin
          match scraccopt with
          | None ->
              aux (Alist.extend resacc chunkspace) None tritail

          | Some((lbcfirst, scriptprev, lbcprev, uchsegacc)) ->
              let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchsegacc) PreventBreak in
                aux (Alist.append resacc [chunkprev; chunkspace]) None tritail
        end

    | (uchseg, lbc, alw) :: tritail ->
        let script = find ctx uchseg in
        if is_ideographic_class lbc then
        (* temporary; whether 'AI' is ideographic or not should depend on the context *)
        (* -- if the spotted character is ideographic -- *)
          begin
            match scraccopt with
            | None ->
                let chunkideo = ideographic script lbc uchseg alw in
                  aux (Alist.extend resacc chunkideo) None tritail

            | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              (* -- if there accumulate some characters before the spotted character -- *)
                let chunkideo = ideographic script lbc uchseg alw in
                let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                  aux (Alist.append resacc [chunkprev; chunkideo]) None tritail
          end
        else
          begin
            match alw with
            | AllowBreak ->
              (* -- if the spotted non-ideographic character allows line break after it -- *)
                begin
                  match scraccopt with
                  | None ->
                      let chunk = preword script lbc lbc [uchseg] AllowBreak in
                        aux (Alist.extend resacc chunk) None tritail

                  | Some((lbcfirst, scriptprev, lbcprev, uchsegacc)) ->
                      if script_equal scriptprev script then
                        let chunk = preword script lbcfirst lbc (List.rev (uchseg :: uchsegacc)) AllowBreak in
                          aux (Alist.extend resacc chunk) None tritail
                      else
                        let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchsegacc) PreventBreak in
                        let chunk = preword script lbc lbc [uchseg] AllowBreak in
                          aux (Alist.append resacc [chunkprev; chunk]) None tritail
                end

          | PreventBreak ->
              begin
                match scraccopt with
                | None ->
                    aux resacc (Some((lbc, script, lbc, [uchseg]))) tritail

                | Some((lbcfirst, scriptprev, lbcprev, uchsegacc)) ->
                    if script_equal scriptprev script then
                      aux resacc (Some((lbcfirst, script, lbc, uchseg :: uchsegacc))) tritail
                    else
                      let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchsegacc) PreventBreak in
                        aux (Alist.extend resacc chunkprev) (Some((lbc, script, lbc, [uchseg]))) tritail
              end
          end
  in

  aux Alist.empty None trilst


let get_font_with_ratio (ctx : context_main) (script_raw : script) =
  let script = normalize_script ctx script_raw in
  match ctx.font_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
  | None          -> raise (FontIsNotSet{ raw = script_raw; normalized = script })
  | Some(fontsch) -> fontsch


let get_language_system (ctx : context_main) (script_raw : script) =
  let script = normalize_script ctx script_raw in
  match ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
  | None          -> CharBasis.NoLanguageSystem
  | Some(langsys) -> langsys


let get_string_info (ctx : context_main) (script_raw : script) =
  let (fontkey, ratio, rising_ratio) = get_font_with_ratio ctx script_raw in
    {
      font_key       = fontkey;
      text_font_size = ctx.font_size *% ratio;
      text_color     = ctx.text_color;
      rising         = ctx.manual_rising +% ctx.font_size *% rising_ratio;
    }
