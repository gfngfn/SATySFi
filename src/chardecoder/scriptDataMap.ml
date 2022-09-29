
open MyUtil
open CharBasis
open LineBreakBox
open HorzBox


let read_east_asian_width _ data =
  match data with
  | "H"  -> EAWHalfWidth
  | "F"  -> EAWFullWidth
  | "Na" -> EAWNarrow
  | "W"  -> EAWWide
  | "A"  -> EAWAmbiguous
  | "N"  -> EAWNeutral
  | _    -> assert false


let read_script eaw_map cp data =
  match data with
  | "Han"      -> HanIdeographic
  | "Hiragana" -> HiraganaOrKatakana
  | "Katakana" -> HiraganaOrKatakana
  | "Latin"    -> Latin
(* temporary; should add more scripts *)

  | "Common" ->
      begin
        try
          match eaw_map |> BatIMap.find cp with
          | EAWNarrow
          | EAWHalfWidth
          | EAWAmbiguous
          | EAWNeutral
              -> CommonNarrow

          | EAWFullWidth
          | EAWWide
              -> CommonWide
        with Not_found -> CommonNarrow
      end

  | _ -> OtherScript


let script_map_ref : (script BatIMap.t) ref = ref (BatIMap.empty ~eq:(=))


let set_from_file (abspath_S : abs_path) (abspath_EAW : abs_path) =
  let eaw_map =
    let channel_EAW = open_in_abs abspath_EAW in
    let eaw_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_EAW) in
    close_in channel_EAW;
    eaw_list |> CharBasis.map_of_list read_east_asian_width
  in
  let script_map =
    let channel_S = open_in_abs abspath_S in
    let script_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_S) in
    close_in channel_S;
    script_list |> CharBasis.map_of_list (read_script eaw_map)
  in
  begin
    script_map_ref := script_map;
  end


let find (ctx : context_main) ((uch, _) : uchar_segment) =
  try
    (!script_map_ref) |> BatIMap.find (Uchar.to_int uch)
                      |> normalize_script ctx
  with Not_found -> OtherScript


let divide_by_script (ctx : context_main) (trilst : line_break_element list) : LineBreakBox.line_break_chunk_main list =

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
