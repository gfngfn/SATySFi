
open HorzBox
open CharBasis
open LineBreakBox

(*
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
*)

let to_chunk_main_list ctx uchlst : line_break_chunk_main list =
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
  let scrlstsp = (* insert_adjacent_space ctx *) scrlst in

  (* begin: for debug *)
  let () =
    scrlstsp |> List.iter (function
      | AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw) ->
          let sa = match alw with AllowBreak -> "" | PreventBreak -> "*" in
          PrintForDebug.lexhorz ("[Alph] " ^ (CharBasis.show_script script) ^ sa ^ " ");
          let s = uchlst |> List.map (fun uch -> InternalText.to_utf8 (InternalText.of_uchar uch)) |> String.concat "" in
          PrintForDebug.lexhorzE s
      | Space ->
          PrintForDebug.lexhorzE "[Space]"
      | UnbreakableSpace ->
          PrintForDebug.lexhorzE "[UnbreakableSpace]"
      | IdeographicChunk(script, lbc, uch, alw) ->
          let sa = match alw with AllowBreak -> "" | PreventBreak -> "*" in
          PrintForDebug.lexhorz ("[Ideo] " ^ (CharBasis.show_script script) ^ sa ^ " ");
          PrintForDebug.lexhorzE (InternalText.to_utf8 (InternalText.of_uchar uch))
    )
  in
  (* end: for debug *)

  scrlstsp


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

let to_chunks ctx uchlst : line_break_chunk list =
(*
*)
  let scrlstsp = to_chunk_main_list ctx uchlst in

  scrlstsp |> List.map (fun chunkmain -> (ctx, chunkmain))
(*
  scrlstsp |> List.map (function
    | AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw) ->
        [ LBPure(fixed_string script (get_string_info ctx script) uchlst); ]
(*
    | PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          LBPure(fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch)));
          breakable_space HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]
*)
    | Space ->
        [ breakable_space space_natural space_shrink space_stretch; ]

    | UnbreakableSpace ->
        [ unbreakable_space space_natural space_shrink space_stretch; ]

    | IdeographicChunk(script, lbc, uch, alw) ->
        [
          LBPure(fixed_string script (get_string_info ctx script) [uch]);
          breakable_space HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;  (* temporary *)
        ]

(*
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
*)        
  ) |> List.concat
*)
(*
let to_boxes_pure ctx uchlst : lb_pure_box list =
  let (_, font_ratio, rising_ratio) = get_font_with_ratio ctx ctx.dominant_script in
  let size = ctx.font_size *% font_ratio in
  let space_natural = size *% ctx.space_natural in
  let space_shrink  = size *% ctx.space_shrink in
  let space_stretch = size *% ctx.space_stretch in
(*
  let adjacent_stretch = size *% ctx.adjacent_stretch in
*)

  let scrlstsp = to_chunk_main_list ctx uchlst in

  scrlstsp |> List.map (function
    | AlphabeticChunk(script, lbcfirst, lbcprev, uchlst, alw) ->
        [ fixed_string script (get_string_info ctx script) uchlst; ]
(*
    | PreWord(script, trilst, CharBasis.AllowBreak) ->
        [
          fixed_string script (get_string_info ctx script) (trilst |> List.map (fun (uch, _, _) -> uch));
          pure_space HorzBox.Length.zero HorzBox.Length.zero adjacent_stretch;
        ]
*)
    | Space ->
        [ pure_space space_natural space_shrink space_stretch; ]

    | UnbreakableSpace ->
        [ pure_space space_natural space_shrink space_stretch; ]

    | IdeographicChunk(script, lbc, uch, alw) ->
        [ fixed_string script (get_string_info ctx script) uchlst; ]

(*
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
*)        
  ) |> List.concat
*)

(*
let insert_auto_space lhblst =

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
*)
(*
let adjacent_stretch = size *% ctx.adjacent_stretch in  (* temporary *)
*)

let pure_space_between_scripts size script1 script2 =
  match (script1, script2) with
  | (HanIdeographic    , Latin             )
  | (Latin             , HanIdeographic    )
  | (HiraganaOrKatakana, Latin             )
  | (Latin             , HiraganaOrKatakana)
    ->
      Some(LBAtom((natural (size *% 0.24), size *% 0.08, size *% 0.16), EvHorzEmpty))
        (* temporary; shold refer to the context for spacing information between two scripts *)
  | _ -> None


let space_width_info ctx : length_info =
(*
  let (_, font_ratio, _) = get_font_with_ratio ctx ctx.dominant_script in
*)
  let size = ctx.font_size (* *% font_ratio *) in
  let widnatural = size *% ctx.space_natural in
  let widshrink  = size *% ctx.space_shrink in
  let widstretch = size *% ctx.space_stretch in
    make_width_info widnatural widshrink widstretch


let pure_space ctx : lb_pure_box =
  let widinfo = space_width_info ctx in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


let breakable_space ctx : lb_box =
  let dscrid = DiscretionaryID.fresh () in
    LBDiscretionary(ctx.badness_space, dscrid, [pure_space ctx], [], [])


let unbreakable_space ctx : lb_box =
    LBPure(pure_space ctx)


let fixed_string (ctx : input_context) (script : script) (uchlst : Uchar.t list) : lb_pure_box =
  let hsinfo = get_string_info ctx script in
  let (otxt, wid, hgt, dpt) = FontInfo.get_metrics_of_word hsinfo uchlst in
    LBAtom((natural wid, hgt, dpt), EvHorzString(hsinfo, hgt, dpt, otxt))


let discretionary_if_breakable alw badns lphb =
  match alw with
  | AllowBreak ->
      let dscrid = DiscretionaryID.fresh () in
      LBDiscretionary(badns, dscrid, [lphb], [], [])

  | PreventBreak ->
      LBPure(lphb)


let adjacent_space ctx =
  let widstretch = ctx.font_size *% ctx.adjacent_stretch in
  let widinfo = make_width_info Length.zero Length.zero widstretch in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


let space_between_chunks (ctxprev, scriptprev, lbcprev) alw (ctx, script, lbc) : lb_box list =
  if not (script_equal scriptprev script) then
    let size = Length.max ctxprev.font_size ctx.font_size in
    let badns = ctx.badness_space in
      match pure_space_between_scripts size scriptprev script with
      | Some(lphb) -> [discretionary_if_breakable alw badns lphb]
      | None       -> [discretionary_if_breakable alw badns (adjacent_space ctx)]  (* temporary *)
  else
    [discretionary_if_breakable alw ctx.badness_space (adjacent_space ctx)]
  (* TEMPORARY; SHOULD WRITE MORE *)

(*
  LBPure(fixed_string ctx script [Uchar.of_int (Char.code 'A')])
*)

let space_between_chunks_pure (ctxprev, scriptprev, lbcprev) (ctx, script, lbc) : lb_pure_box list =
  if not (script_equal scriptprev script) then
    let size = Length.max ctxprev.font_size ctx.font_size in
      match pure_space_between_scripts size scriptprev script with
      | Some(lphb) -> [lphb]
      | None       -> [adjacent_space ctx]  (* temporary *)
  else
    [adjacent_space ctx]
  (* TEMPORARY; SHOULD WRITE MORE *)


let chunks_to_boxes (chunklst : line_break_chunk list) =
  let rec aux lhbacc prevopt chunklst =
    match chunklst with
    | [] ->
        List.rev lhbacc

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lhblstmain) =
          match chunkmain with
          | Space ->
              (None, [breakable_space ctx])

          | UnbreakableSpace ->
              (None, [unbreakable_space ctx])
              
          | AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw) ->
              let opt = Some(((ctx, script, lbclast), alw)) in
              let lhblststr = [LBPure(fixed_string ctx script uchlst)] in
              begin
                match prevopt with
                | None ->
                    (opt, lhblststr)

                | Some((previnfo, alw)) ->
                    let autospace = space_between_chunks previnfo alw (ctx, script, lbcfirst) in
                    (opt, List.append autospace lhblststr)
              end

          | IdeographicChunk(script, lbc, uch, alw) ->
              let opt = Some(((ctx, script, lbc), alw)) in
              let lhblststr = [LBPure(fixed_string ctx script [uch])] in
              begin
                match prevopt with
                | None ->
                    (opt, lhblststr)

                | Some((previnfo, alw)) ->
                    let autospace = space_between_chunks previnfo alw (ctx, script, lbc) in
                    (opt, List.append autospace lhblststr)
              end
        in
          aux (List.rev_append lhblstmain lhbacc) opt chunktail
  in
  aux [] None chunklst


let chunks_to_boxes_pure (chunklst : line_break_chunk list) : lb_pure_box list =
  let rec aux lphbacc prevopt chunklst =
    match chunklst with
    | [] ->
        List.rev lphbacc

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lphblstmain) =
          match chunkmain with
          | Space ->
              (None, [pure_space ctx])

          | UnbreakableSpace ->
              (None, [pure_space ctx])

          | AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw) ->
              let opt = Some(((ctx, script, lbclast), alw)) in
              begin
                match prevopt with
                | None ->
                    (opt, [fixed_string ctx script uchlst])

                | Some((previnfo, alw)) ->
                    let autospace = space_between_chunks_pure previnfo (ctx, script, lbcfirst) in
                    (opt, List.append autospace [fixed_string ctx script uchlst])
              end

          | IdeographicChunk(script, lbc, uch, alw) ->
              let opt = Some(((ctx, script, lbc), alw)) in
              begin
                match prevopt with
                | None ->
                    (opt, [fixed_string ctx script [uch]])

                | Some((previnfo, alw)) ->
                    let autospace = space_between_chunks_pure previnfo (ctx, script, lbc) in
                    (opt, List.append autospace [fixed_string ctx script [uch]])
              end
        in
          aux (List.rev_append lphblstmain lphbacc) opt chunktail
  in
  aux [] None chunklst
