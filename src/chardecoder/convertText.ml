
open Types


(* temporary; should refer to the context for spacing between two scripts *)
let transition_space ctx script1 script2 =
  let (_, font_size) = get_font_info ctx ctx.dominant_script in
  match (script1, script2) with
  | ( (CharBasis.HanIdeographic, CharBasis.Latin)
    | (CharBasis.Latin, CharBasis.HanIdeographic)
    | (CharBasis.HiraganaOrKatakana, CharBasis.Latin)
    | (CharBasis.Latin, CharBasis.HiraganaOrKatakana) ) ->
        [CharBasis.CustomizedSpace(HorzBox.(font_size *% 0.24), HorzBox.(font_size *% 0.08), HorzBox.(font_size *% 0.16))]  (* temporary; shold refer to the context for spacing information between two scripts *)
  | _                                               -> []


let insert_script_transition_space ctx scrlst =
  let rec aux acc prevscriptopt lst =
    match lst with
    | []                 -> List.rev acc
    | scrhead :: scrtail ->
        let scriptopt =
          match scrhead with
          | CharBasis.PreWord(script, _, _)       -> Some(script)
          | CharBasis.Space                       -> None
          | CharBasis.CustomizedSpace(_, _, _)    -> None
          | CharBasis.UnbreakableSpace            -> None
          | CharBasis.IdeographicOpen(script, _)  -> Some(script)
          | CharBasis.IdeographicClose(script, _) -> Some(script)
        in
        begin
          match (prevscriptopt, scriptopt) with
          | (Some(prevscript), Some(script)) ->
              let space = transition_space ctx prevscript script in
                aux (scrhead :: (List.rev_append space acc)) scriptopt scrtail

          | _                                -> aux (scrhead :: acc) scriptopt scrtail
        end
  in
    aux [] None scrlst


let to_boxes ctx uchlst =
  let (_, font_size) = get_font_info ctx ctx.dominant_script in 
  let space_natural = HorzBox.(font_size *% ctx.space_natural) in
  let space_shrink  = HorzBox.(font_size *% ctx.space_shrink) in
  let space_stretch = HorzBox.(font_size *% ctx.space_stretch) in
  let adjacent_stretch = HorzBox.(font_size *% ctx.adjacent_stretch) in
  let trilst = LineBreakDataMap.append_break_opportunity uchlst in

  (* begin: for debug *)
  let () =
    trilst |> List.iter (fun (uch, lbc, alw) ->
      let sc = InternalText.to_utf8 (InternalText.of_uchar uch) in
      let sl = (* match lbc with CharBasis.AL -> "@" | _ -> "^" *) "" in
        match alw with
        | CharBasis.AllowBreak   -> PrintForDebug.lexhorz (sc ^ sl ^ "/")
        | CharBasis.PreventBreak -> PrintForDebug.lexhorz (sc ^ sl ^ ".")
    ); PrintForDebug.lexhorzE "" in
  (* end: for debug *)

  let scrlst = ScriptDataMap.divide_by_script trilst in
  let scrlstsp = insert_script_transition_space ctx scrlst in

  (* begin: for debug *)
  let () =
    scrlstsp |> List.iter (function
      | CharBasis.PreWord(script, trilst, alw) ->
          let sa = match alw with CharBasis.AllowBreak -> "(A)" | CharBasis.PreventBreak -> "(P)" in
          PrintForDebug.lexhorzE ((CharBasis.show_script script) ^ sa);
          LineBreakDataMap.print_trilist trilst
      | CharBasis.Space ->
          PrintForDebug.lexhorzE "SPACE"
      | CharBasis.CustomizedSpace(_) ->
          PrintForDebug.lexhorzE "CUSTOMIZED_SPACE"
      | CharBasis.UnbreakableSpace ->
          PrintForDebug.lexhorzE "UNBREAKABLE_SPACE"
      | CharBasis.IdeographicOpen(script, tri) ->
          PrintForDebug.lexhorzE ("IDEOGRAPHIC_OPEN " ^ (CharBasis.show_script script));
          LineBreakDataMap.print_trilist [tri]
      | CharBasis.IdeographicClose(script, tri) ->
          PrintForDebug.lexhorzE ("IDEOGRAPHIC_CLOSE " ^ (CharBasis.show_script script));
          LineBreakDataMap.print_trilist [tri]
    )
  in
  (* end: for debug *)

  let breakable_space badness natural shrink stretch =
    HorzBox.HorzDiscretionary(badness, Some(HorzBox.PHOuterEmpty(natural, shrink, stretch)), None, None)
  in

  let unbreakable_space natural shrink stretch =
    HorzBox.HorzPure(HorzBox.PHOuterEmpty(natural, shrink, stretch))
  in

  let fixed_string font_info uchlst =
    HorzBox.HorzPure(HorzBox.PHFixedString(font_info, uchlst))
  in

  let half_kern (_, font_size) =
    HorzBox.HorzPure(HorzBox.PHFixedEmpty(HorzBox.(font_size *% -0.5)))
  in

  let breakable_half badness (_, font_size) stretch_ratio =
    HorzBox.HorzDiscretionary(badness, Some(HorzBox.PHOuterEmpty(HorzBox.(font_size *% 0.5), HorzBox.Length.zero, HorzBox.(font_size *% stretch_ratio))), None, None)
  in

  scrlstsp |> List.map (function
    | CharBasis.PreWord(script, trilst, CharBasis.PreventBreak) ->
        [ fixed_string (get_font_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch)); ]

    | CharBasis.PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          fixed_string (get_font_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch));
          breakable_space 100 (* temporary *) HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]

    | CharBasis.Space ->
        [ breakable_space 100 (* temporary *) space_natural space_shrink space_stretch; ]

    | CharBasis.CustomizedSpace(wid_natural, wid_shrink, wid_stretch) ->
        [ breakable_space 100 (* temporary *) wid_natural wid_shrink wid_stretch; ]

    | CharBasis.UnbreakableSpace ->
        [ unbreakable_space space_natural space_shrink space_stretch; ]

    | CharBasis.IdeographicOpen(script, (uch, _, _)) ->
        let font_info = get_font_info ctx script in
        [
          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch;
          half_kern font_info;
          fixed_string font_info [uch];
        ]

    | CharBasis.IdeographicClose(script, (uch, _, _)) ->
        let font_info = get_font_info ctx script in
        [
          fixed_string font_info [uch];
          half_kern font_info;
          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch;
        ]

  ) |> List.concat

