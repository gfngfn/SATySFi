
open CharBasis
open LineBreakBox


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


let find uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> OtherScript
  | Some(uch_ucore) ->
      match (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None         -> OtherScript
      | Some(script) -> script


let divide_by_script (trilst : (Uchar.t * line_break_class * break_opportunity) list) : LineBreakBox.line_break_chunk_main list =

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
              List.rev resacc

          | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              let chunk = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
              List.rev (chunk :: resacc)
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
              aux (chunkspace :: resacc) None tritail

          | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                aux (chunkspace :: chunkprev :: resacc) None tritail
        end

    | (uch, lbc, alw) :: tritail ->
        let script = find uch in
        if is_ideographic_class lbc then
        (* temporary; whether 'AI' is ideographic or not should depend on the context *)
        (* -- if the spotted character is ideographic -- *)
          begin
            match scraccopt with
            | None ->
                let chunkideo = ideographic script lbc uch alw in
                  aux (chunkideo :: resacc) None tritail

            | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
              (* -- if there accumulate some characters before the spotted character -- *)
                let chunkideo = ideographic script lbc uch alw in
                let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                  aux (chunkideo :: chunkprev :: resacc) None tritail
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
                        aux (chunk :: resacc) None tritail

                  | Some((lbcfirst, scriptprev, lbcprev, uchacc)) ->
                      if script_equal scriptprev script then
                        let chunk = preword script lbcfirst lbc (List.rev (uch :: uchacc)) AllowBreak in
                          aux (chunk :: resacc) None tritail
                      else
                        let chunkprev = preword scriptprev lbcfirst lbcprev (List.rev uchacc) PreventBreak in
                        let chunk = preword script lbc lbc [uch] AllowBreak in
                          aux (chunk :: chunkprev :: resacc) None tritail
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
                        aux (chunkprev :: resacc) (Some((lbc, script, lbc, [uch]))) tritail
              end
          end
  in

  let scrlst = aux [] None trilst in
    scrlst
(*
    scrlst |> List.fold_left (fun (lbuacc, lbulastopt) (script, trilst) ->
      match script with
      | Common ->
          begin
            match lbulastopt with
            | None -> 
      | Inherited -> 
    ) ([], None)
*)
