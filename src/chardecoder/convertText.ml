
open HorzBox
open CharBasis
open LineBreakBox


let get_script_opt lbu =
  match lbu with
  | Space
  | UnbreakableSpace
  | CustomizedSpace(_, _, _)
    -> None

  | PreWord(script, _, _)
  | JLOpen(script, _)
  | JLClose(script, _)
  | JLMiddle(script, _)
  | JLNonstarter(script, _)
  | JLComma(script, _)
  | JLFullStop(script, _)
    -> Some(script)


(* temporary; should refer to the context for spacing between two scripts *)
let adjacent_space ctx lbu1 lbu2 =
  let (_, font_ratio, rising_ratio) = get_font_with_ratio ctx ctx.dominant_script in
  let size = ctx.font_size *% font_ratio in
  let half_space_soft = [CustomizedSpace(size *% 0.5, size *% 0.25 (* temporary *), size *% 0.25 (* temporary *))] in
  let half_space_hard = [CustomizedSpace(size *% 0.5, Length.zero, size *% 0.25 (* temporary *))] in
  let full_space = [CustomizedSpace(size, size *% 0.5 (* temporary *), size *% 0.5 (* temporary *))] in
  match (lbu1, lbu2) with
  | (PreWord(_, _, _)  , JLOpen(_, _)    ) -> half_space_soft
  | (JLClose(_, _)     , PreWord(_, _, _)) -> half_space_soft
  | (JLClose(_, _)     , JLOpen(_, _)    ) -> half_space_soft
  | (JLNonstarter(_, _), PreWord(_, _, _)) -> full_space
  | (JLComma(_, _)     , PreWord(_, _, _)) -> half_space_soft
  | (JLComma(_, _)     , JLOpen(_, _)    ) -> half_space_soft
  | (JLFullStop(_, _)  , PreWord(_, _, _)) -> half_space_hard
  | (JLFullStop(_, _)  , JLOpen(_, _)    ) -> half_space_hard
  | _ -> []


let insert_adjacent_space (ctx : input_context) (lbulst : ('a line_break_unit) list) : ('a line_break_unit) list =
  let rec aux acc prevlbuopt lbulst =
    match lbulst with
    | []                 -> List.rev acc
    | lbuhead :: lbutail ->
        let space =
          match prevlbuopt with
          | None          -> []
          | Some(prevlbu) -> adjacent_space ctx prevlbu lbuhead
        in
          aux (lbuhead :: (List.rev_append space acc)) (Some(lbuhead)) lbutail
  in
    aux [] None lbulst


let to_boxes_scheme ctx uchlst =
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
  let scrlstsp = insert_adjacent_space ctx scrlst in

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

  scrlstsp


let pure_space widnat widshrink widstretch : lb_pure_box =
  let widinfo = make_width_info widnat widshrink widstretch in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


let fixed_string (script : script) (hsinfo : horz_string_info) (uchlst : Uchar.t list) : lb_pure_box =
  let (otxt, wid, hgt, dpt) = FontInfo.get_metrics_of_word hsinfo uchlst in
    LBAtom((natural wid, hgt, dpt), EvHorzString(script, hsinfo, hgt, dpt, otxt))


let half_kern (hsinfo : horz_string_info) : lb_pure_box =
  LBAtom((natural (hsinfo.text_font_size *% -0.5), Length.zero, Length.zero), EvHorzEmpty)

(*
let breakable_half badness (_, fontsize) stretch_ratio =
  let dscrid = DiscretionaryID.fresh () in
  let widinfo = make_width_info (fontsize *% 0.5) Length.zero (fontsize *% stretch_ratio) in
    LBDiscretionary(badness, dscrid, Some(LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)), None, None)


let breakable_full badness (_, fontsize) stretch_ratio =
  let dscrid = DiscretionaryID.fresh () in
  let widinfo = make_width_info fontsize Length.zero (fontsize *% stretch_ratio) in
    LBDiscretionary(badness, dscrid, Some(LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)), None, None)
*)

let to_boxes ctx uchlst : lb_box list =
  let (_, font_ratio, rising_ratio) = get_font_with_ratio ctx ctx.dominant_script in
  let size = ctx.font_size *% font_ratio in
  let space_natural = size *% ctx.space_natural in
  let space_shrink  = size *% ctx.space_shrink in
  let space_stretch = size *% ctx.space_stretch in
  let adjacent_stretch = size *% ctx.adjacent_stretch in

  let breakable_space widnat widshrink widstretch : lb_box =
    let dscrid = DiscretionaryID.fresh () in
    let widinfo = make_width_info widnat widshrink widstretch in
    LBDiscretionary(ctx.badness_space, dscrid, [LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)], [], [])
  in

  let unbreakable_space widnat widshrink widstretch : lb_box =
    LBPure(pure_space widnat widshrink widstretch)
  in

  let scrlstsp = to_boxes_scheme ctx uchlst in

  scrlstsp |> List.map (function
    | PreWord(script, trilst, CharBasis.PreventBreak) ->
        [ LBPure(fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch))); ]

    | PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          LBPure(fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch)));
          breakable_space HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]

    | Space ->
        [ breakable_space space_natural space_shrink space_stretch; ]

    | CustomizedSpace(wid_natural, wid_shrink, wid_stretch) ->
        [ breakable_space wid_natural wid_shrink wid_stretch; ]

    | UnbreakableSpace ->
        [ unbreakable_space space_natural space_shrink space_stretch; ]

    | JLOpen(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
          LBPure(half_kern hsinfo);
          LBPure(fixed_string script hsinfo [uch]);
        ]

    | JLClose(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          LBPure(fixed_string script hsinfo [uch]);
          LBPure(half_kern hsinfo);
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLNonstarter(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          LBPure(fixed_string script hsinfo [uch]);
(*          breakable_full 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLComma(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          LBPure(fixed_string script hsinfo [uch]);
          LBPure(half_kern hsinfo);
        ]

    | JLFullStop(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          LBPure(fixed_string script hsinfo [uch]);
          LBPure(half_kern hsinfo);
        ]
        
    | JLMiddle(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [ LBPure(fixed_string script hsinfo [uch]); ]
        
  ) |> List.concat


let to_boxes_pure ctx uchlst : lb_pure_box list =
  let (_, font_ratio, rising_ratio) = get_font_with_ratio ctx ctx.dominant_script in
  let size = ctx.font_size *% font_ratio in
  let space_natural = size *% ctx.space_natural in
  let space_shrink  = size *% ctx.space_shrink in
  let space_stretch = size *% ctx.space_stretch in
  let adjacent_stretch = size *% ctx.adjacent_stretch in

  let scrlstsp = to_boxes_scheme ctx uchlst in

  scrlstsp |> List.map (function
    | PreWord(script, trilst, CharBasis.PreventBreak) ->
        [ fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch)); ]

    | PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch));
          pure_space HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]

    | Space ->
        [ pure_space space_natural space_shrink space_stretch; ]

    | CustomizedSpace(wid_natural, wid_shrink, wid_stretch) ->
        [ pure_space wid_natural wid_shrink wid_stretch; ]

    | UnbreakableSpace ->
        [ pure_space space_natural space_shrink space_stretch; ]

    | JLOpen(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
          half_kern hsinfo;
          fixed_string script hsinfo [uch];
        ]

    | JLClose(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          fixed_string script hsinfo [uch];
          half_kern hsinfo;
(*          breakable_half 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLNonstarter(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          fixed_string script hsinfo [uch];
(*          breakable_full 100 (* temporary *) font_info ctx.adjacent_stretch; *)
        ]

    | JLComma(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          fixed_string script hsinfo [uch];
          half_kern hsinfo;
        ]

    | JLFullStop(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [
          fixed_string script hsinfo [uch];
          half_kern hsinfo;
        ]
        
    | JLMiddle(script, (uch, _, _)) ->
        let hsinfo = get_string_info ctx script in
        [ fixed_string script hsinfo [uch]; ]
        
  ) |> List.concat


let insert_auto_space lhblst =

  let insert_between_scripts size script1 script2 =
    match (script1, script2) with
    | (HanIdeographic    , Latin             )
    | (Latin             , HanIdeographic    )
    | (HiraganaOrKatakana, Latin             )
    | (Latin             , HiraganaOrKatakana)
      ->
        [LBPure(LBAtom((natural (size *% 0.24), size *% 0.08, size *% 0.16), EvHorzEmpty))]
          (* temporary; shold refer to the context for spacing information between two scripts *)
    | _ -> []
  in

  let rec aux lhbacc scriptprevopt lhblst =
    match lhblst with
    | [] ->
        List.rev lhbacc

    | ((LBPure(LBAtom(metrics, EvHorzString(script, hsinfo, _, _, _))) as lhb)) :: lhbtail ->
        begin
          match scriptprevopt with
          | None ->
              aux (lhb :: lhbacc) (Some(script)) lhbtail

          | Some(scriptprev) ->

              (* begin: for debug *)
              let () =
                match (scriptprev, script) with
                | (Common, _) | (_, Common) -> ()
                | _  when scriptprev = script -> ()
                | _ ->
                  Format.printf "ConvertText> script1 = %a\n" pp_script scriptprev;
                  Format.printf "ConvertText> script2 = %a\n" pp_script script
              in
              (* end: for debug *)

              let size = hsinfo.text_font_size in
              let lhblstspace = insert_between_scripts size scriptprev script in
              aux (lhb :: (List.rev_append lhblstspace lhbacc)) (Some(script)) lhbtail
        end

    | lhb :: lhbtail ->
        aux (lhb :: lhbacc) None lhbtail
  in
    aux [] None lhblst
