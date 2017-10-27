
open Types
open CharBasis


let get_script_opt lbu =
  match lbu with
  | Space
  | UnbreakableSpace
  | CustomizedSpace(_, _, _) -> None

  | PreWord(script, _, _)
  | JLOpen(script, _)
  | JLClose(script, _)
  | JLMiddle(script, _)
  | JLNonstarter(script, _)
  | JLComma(script, _)
  | JLFullStop(script, _)    -> Some(script)


(* temporary; should refer to the context for spacing between two scripts *)
let transition_space ctx lbu1 lbu2 =
  let (_, font_size) = get_font_info ctx ctx.dominant_script in
  let half_space_soft = [CustomizedSpace(HorzBox.(font_size *% 0.5), HorzBox.(font_size *% 0.25 (* temporary *)), HorzBox.(font_size *% 0.25 (* temporary *)))] in
  let half_space_hard = [CustomizedSpace(HorzBox.(font_size *% 0.5), HorzBox.Length.zero, HorzBox.(font_size *% 0.25 (* temporary *)))] in
  let full_space = [CustomizedSpace(font_size, HorzBox.(font_size *% 0.5 (* temporary *)), HorzBox.(font_size *% 0.5 (* temporary *)))] in
  match (lbu1, lbu2) with
  | (PreWord(_, _, _)  , JLOpen(_, _)    ) -> half_space_soft
  | (JLClose(_, _)     , PreWord(_, _, _)) -> half_space_soft
  | (JLClose(_, _)     , JLOpen(_, _)    ) -> half_space_soft
  | (JLNonstarter(_, _), PreWord(_, _, _)) -> full_space
  | (JLComma(_, _)     , PreWord(_, _, _)) -> half_space_soft
  | (JLComma(_, _)     , JLOpen(_, _)    ) -> half_space_soft
  | (JLFullStop(_, _)  , PreWord(_, _, _)) -> half_space_hard
  | (JLFullStop(_, _)  , JLOpen(_, _)    ) -> half_space_hard
  | _ ->
      let scriptopt1 = get_script_opt lbu1 in
      let scriptopt2 = get_script_opt lbu2 in
        match (scriptopt1, scriptopt2) with
        | (Some(script1), Some(script2)) ->
            begin
              match (script1, script2) with
              | ( (HanIdeographic, Latin)
                | (Latin, HanIdeographic)
                | (HiraganaOrKatakana, Latin)
                | (Latin, HiraganaOrKatakana) ) ->
                    [CustomizedSpace(HorzBox.(font_size *% 0.24), HorzBox.(font_size *% 0.08), HorzBox.(font_size *% 0.16))]  (* temporary; shold refer to the context for spacing information between two scripts *)
              | _                                               -> []
            end

        | _ -> []


let insert_script_transition_space ctx lbulst =
  let rec aux acc prevlbuopt lbulst =
    match lbulst with
    | []                 -> List.rev acc
    | lbuhead :: lbutail ->
        let space =
          match prevlbuopt with
          | None          -> []
          | Some(prevlbu) -> transition_space ctx prevlbu lbuhead
        in
          aux (lbuhead :: (List.rev_append space acc)) (Some(lbuhead)) lbutail
  in
    aux [] None lbulst


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
      | PreWord(script, trilst, alw) ->
          let sa = match alw with AllowBreak -> "(A)" | PreventBreak -> "(P)" in
          PrintForDebug.lexhorzE ((CharBasis.show_script script) ^ sa);
          LineBreakDataMap.print_trilist trilst
      | Space ->
          PrintForDebug.lexhorzE "SPACE"
      | CustomizedSpace(_) ->
          PrintForDebug.lexhorzE "CUSTOMIZED_SPACE"
      | UnbreakableSpace ->
          PrintForDebug.lexhorzE "UNBREAKABLE_SPACE"
      | JLOpen(script, tri) ->
          PrintForDebug.lexhorzE ("JL_OPEN " ^ (CharBasis.show_script script));
          LineBreakDataMap.print_trilist [tri]
      | JLClose(script, tri) ->
          PrintForDebug.lexhorzE ("JL_CLOSE " ^ (CharBasis.show_script script));
          LineBreakDataMap.print_trilist [tri]
      | JLMiddle(script, tri) ->
          PrintForDebug.lexhorzE "JL_MIDDLE"
      | JLComma(script, tri) ->
          PrintForDebug.lexhorzE "JL_COMMA"
      | JLFullStop(script, tri) ->
          PrintForDebug.lexhorzE "JL_FULL_STOP"
      | JLNonstarter(script, tri) ->
          PrintForDebug.lexhorzE "JL_NONSTARTER"
    )
  in
  (* end: for debug *)

  let breakable_space badness natural shrink stretch =
    HorzBox.HorzDiscretionary(badness, Some(HorzBox.PHOuterEmpty(natural, shrink, stretch)), None, None)
  in

  let unbreakable_space natural shrink stretch =
    HorzBox.HorzPure(HorzBox.PHOuterEmpty(natural, shrink, stretch))
  in

  let fixed_string info uchlst =
    HorzBox.HorzPure(HorzBox.PHFixedString(info, uchlst))
  in

  let half_kern info =
    HorzBox.HorzPure(HorzBox.PHFixedEmpty(HorzBox.(info.font_size *% -0.5)))
  in
(*
  let breakable_half badness (_, font_size) stretch_ratio =
    HorzBox.HorzDiscretionary(badness, Some(HorzBox.PHOuterEmpty(HorzBox.(font_size *% 0.5), HorzBox.Length.zero, HorzBox.(font_size *% stretch_ratio))), None, None)
  in

  let breakable_full badness (_, font_size) stretch_ratio =
    HorzBox.HorzDiscretionary(badness, Some(HorzBox.PHOuterEmpty(font_size, HorzBox.Length.zero, HorzBox.(font_size *% stretch_ratio))), None, None)
  in    
*)
  scrlstsp |> List.map (function
    | PreWord(script, trilst, CharBasis.PreventBreak) ->
        [ fixed_string (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch)); ]

    | PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          fixed_string (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch));
          breakable_space 100 (* temporary *) HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]

    | Space ->
        [ breakable_space 100 (* temporary *) space_natural space_shrink space_stretch; ]

    | CustomizedSpace(wid_natural, wid_shrink, wid_stretch) ->
        [ breakable_space 100 (* temporary *) wid_natural wid_shrink wid_stretch; ]

    | UnbreakableSpace ->
        [ unbreakable_space space_natural space_shrink space_stretch; ]

    | JLOpen(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
          half_kern info;
          fixed_string info [uch];
        ]

    | JLClose(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [
          fixed_string info [uch];
          half_kern info;
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLNonstarter(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [
          fixed_string info [uch];
(*          breakable_full 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLComma(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [
          fixed_string info [uch];
          half_kern info;
        ]

    | JLFullStop(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [
          fixed_string info [uch];
          half_kern info;
        ]
        
    | JLMiddle(script, (uch, _, _)) ->
        let info = get_string_info ctx script in
        [ fixed_string info [uch]; ]
        
  ) |> List.concat

