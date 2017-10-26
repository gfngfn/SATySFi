
open CharBasis


let read_script = function
  | "Common"   -> Common
  | "Han"      -> HanIdeographic
  | "Hiragana" -> HiraganaOrKatakana
  | "Katakana" -> HiraganaOrKatakana
  | "Latin"    -> Latin
(* temporary; should add more scripts *)
  | _          -> Other


let script_map_ref : (script UCoreLib.UMap.t) ref = ref (UCoreLib.UMap.empty ~eq:(=))


let set_from_file filename =
  let channel = open_in filename in
  let script_list = DataParser.main DataLexer.expr (Lexing.from_channel channel) in
  let script_map = script_list |> CharBasis.map_of_list read_script in
  begin
    script_map_ref := script_map;
  end


let find uch =
  match UCoreLib.UChar.of_int (Uchar.to_int uch) with
  | None            -> Other
  | Some(uch_ucore) ->
      match (!script_map_ref) |> UCoreLib.UMap.find_opt uch_ucore with
      | None         -> Other
      | Some(script) -> script


let divide_by_script trilst =

  let preword alw script trilst =
    PreWord(script, trilst, alw)
  in

  let rec aux resacc scraccopt trilst =
    match trilst with
    | [] ->
        begin
          match scraccopt with
          | None                       -> List.rev resacc
          | Some((prevscript, triacc)) -> List.rev ((preword PreventBreak (* temporary; should consider whether there is an embedded command or the end of line after the last character *) prevscript (List.rev triacc)) :: resacc)
        end

    | ((uch, lbc, alw) as trihead) :: tritail ->
        let script = find uch in
        begin
          match alw with
          | AllowBreak ->
              (* -- if the spotted character allows line break after it -- *)
              begin
                match lbc with
                | SP ->
                    (* -- if the spotted character is a space -- *)
                    begin
                      match scraccopt with
                      | None -> 
                          aux (Space :: resacc) None tritail

                      | Some((prevscript, triacc)) ->
                          aux (Space :: (preword PreventBreak prevscript (List.rev triacc)) :: resacc) None tritail
                    end

                | IDCP ->
                    begin
                      match scraccopt with
                      | None ->
                          aux (IdeographicClose(script, trihead) :: resacc) None tritail

                      | Some((prevscript, triacc)) ->
                          aux (IdeographicClose(script, trihead) :: (preword PreventBreak prevscript (List.rev triacc) :: resacc)) None tritail
                    end

                | _ ->
                    begin
                      match scraccopt with
                      | None ->
                          aux ((preword AllowBreak script [trihead]) :: resacc) None tritail

                      | Some((prevscript, triacc)) ->
                          if script_equal prevscript script then
                            aux ((preword AllowBreak prevscript (List.rev (trihead :: triacc))) :: resacc) None tritail
                          else
                            aux ((preword AllowBreak script [trihead]) :: (preword PreventBreak prevscript (List.rev triacc)) :: resacc) None tritail
                    end
              end

          | PreventBreak ->
              begin
                match lbc with
                | SP ->
                    begin
                      match scraccopt with
                      | None ->
                          aux (UnbreakableSpace :: resacc) None tritail

                      | Some((prevscript, triacc)) ->
                          aux (UnbreakableSpace :: (preword PreventBreak prevscript (List.rev triacc)) :: resacc) None tritail
                    end

                | IDOP ->
                    begin
                      match scraccopt with
                      | None ->
                          aux (IdeographicOpen(script, trihead) :: resacc) None tritail

                      | Some((prevscript, triacc)) ->
                          aux (IdeographicOpen(script, trihead) :: (preword AllowBreak (* temporary *) prevscript (List.rev triacc)) :: resacc) None tritail
                    end

                | _ ->
                    begin
                      match scraccopt with
                      | None -> 
                          aux resacc (Some((script, [trihead]))) tritail

                      | Some((prevscript, triacc)) ->
                          if script_equal prevscript script then
                            aux resacc (Some((script, trihead :: triacc))) tritail
                          else
                            aux ((preword PreventBreak prevscript (List.rev triacc)) :: resacc) (Some((script, [trihead]))) tritail
                    end
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
