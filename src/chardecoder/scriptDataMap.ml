
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
        match UCoreLib.UChar.of_int cp with
        | None ->
            CommonNarrow  (* temporary; maybe should emit an error *)

        | Some(uch_ucore) ->
            begin
              match eaw_map |> UCoreLib.UMap.find_opt uch_ucore with
              | None
              | Some(EAWNarrow)
              | Some(EAWHalfWidth)
              | Some(EAWAmbiguous)
              | Some(EAWNeutral)
                  -> CommonNarrow

              | Some(EAWFullWidth)
              | Some(EAWWide)
                  -> CommonWide
            end
      end

  | _ -> OtherScript


let script_map_ref : (script UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename_S filename_EAW =
  let eaw_map =
    let channel_EAW = open_in filename_EAW in
    let eaw_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_EAW) in
    close_in channel_EAW;
    eaw_list |> CharBasis.map_of_list read_east_asian_width
  in
  let script_map =
    let channel_S = open_in filename_S in
    let script_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_S) in
    close_in channel_S;
    script_list |> CharBasis.map_of_list (read_script eaw_map)
  in
  begin
    script_map_ref := script_map;
  end


let find ctx uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> OtherScript
  | Some(uch_ucore) ->
      match (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None             -> OtherScript
      | Some(script_raw) -> normalize_script ctx script_raw


let divide_by_script (ctx : context_main) (trilst : line_break_element list) : LineBreakBox.line_break_chunk_main list =

  let ideographic script lbc uch alw =
    IdeographicChunk(script, lbc, uch, alw)
  in

  let preword script lbcfirst lbclast uchlst alw =
    AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw)
  in

  let rec aux resacc (scraccopt : (line_break_class * script * line_break_class * Uchar.t list) option) trilst =
    match trilst with
    | [] ->
        begin
          match scraccopt with
          | None ->
              Alist.to_list resacc

          | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              let chunk = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
              Alist.to_list (Alist.extend resacc chunk)
        end

    | (uch, SP, alw) :: tritail ->
        let chunkspace =
          match alw with
          | AllowBreak   -> Space
          | PreventBreak -> UnbreakableSpace
        in
        begin
          match scraccopt with
          | None ->
              aux (Alist.extend resacc chunkspace) None tritail

          | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                aux (Alist.append resacc [chunkprev; chunkspace]) None tritail
        end

    | (uch, lbc, alw) :: tritail ->
        let script = find ctx uch in
        if is_ideographic_class lbc then
        (* temporary; whether 'AI' is ideographic or not should depend on the context *)
        (* -- if the spotted character is ideographic -- *)
          begin
            match scraccopt with
            | None ->
                let chunkideo = ideographic script lbc uch alw in
                  aux (Alist.extend resacc chunkideo) None tritail

            | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              (* -- if there accumulate some characters before the spotted character -- *)
                let chunkideo = ideographic script lbc uch alw in
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
                      let chunk = preword script lbc lbc [uch] AllowBreak in
                        aux (Alist.extend resacc chunk) None tritail

                  | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
                      if script_equal scriptprev script then
                        let chunk = preword script lbcfirst lbc (List.rev (uch :: uchacc)) AllowBreak in
                          aux (Alist.extend resacc chunk) None tritail
                      else
                        let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                        let chunk = preword script lbc lbc [uch] AllowBreak in
                          aux (Alist.append resacc [chunkprev; chunk]) None tritail
                end

          | PreventBreak ->
              begin
                match scraccopt with
                | None ->
                    aux resacc (Some((lbc, script, lbc, [uch]))) tritail

                | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
                    if script_equal scriptprev script then
                      aux resacc (Some((lbcfirst, script, lbc, uch :: uchacc))) tritail
                    else
                      let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                        aux (Alist.extend resacc chunkprev) (Some((lbc, script, lbc, [uch]))) tritail
              end
          end
  in

  let scrlst = aux Alist.empty None trilst in
    scrlst
