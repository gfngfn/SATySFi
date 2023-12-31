(* -*- coding: utf-8 -*- *)

open MyUtil
open LengthInterface
open HorzBox


exception PageNumberLimitExceeded of int


type frame_breaking =
  | Beginning
  | Midway

type pb_vert_box = pb_vert_box_main * breakability
  (* --
     (1) main contents
     (2) whether page-breaking is allowed immediately after the contents
     -- *)

and pb_vert_box_main =
  | PBVertLine  of reachability * length * length * intermediate_horz_box list
  | PBVertSkip  of debug_margin_info * length
  | PBVertFrame of frame_breaking * paddings * decoration * decoration * decoration * decoration * length * pb_vert_box list
  | PBClearPage
  | PBHookPageBreak of (page_break_info -> point -> unit)

type pb_normalized =
  | Normalized of pb_vert_box list
  | NormalizedEmpty

type pb_rest =
  | Finished of evaled_vert_box Alist.t * evaled_vert_box Alist.t * length
  | Remains

type pb_division =
  | Outside
  | Inside of evaled_vert_box Alist.t * pb_normalized

type pb_answer = {
  division : pb_division;
  footnote : evaled_vert_box Alist.t;
  height   : length;  (* -- the total length of the page contents in the block-direction -- *)
  badness  : pure_badness;
}

type pb_accumulator = {
  depth            : int;  (* -- mainly for debugging -- *)
  skip_after_break : length;
  allow_break      : breakability;
  last_breakable   : pb_answer;
  solid_body       : evaled_vert_box Alist.t;
  solid_footnote   : evaled_vert_box Alist.t;
  discardable      : evaled_vert_box Alist.t;
  total_height     : length;  (* -- the length of the contents traversed so far -- *)
}


let initial_badness = 100000


(* --
   `chop_single_column` receives:

   * `pbinfo`: information available before page breaking starts,
   * `area_height`: required total height of the area, and
   * `pbvblst`: contents to break into pages,

   and then returns `(evvblst1, evvblst2, pbvblstopt)` where

   * `evvblst1`: the main contents of the newly created page,
   * `evvblst2`: the footnote of the page, and
   * `pbvblstopt`: contents that remains to be page-broken, or `None` if the new page is the last one.

   -- *)
let chop_single_column (pbinfo : page_break_info) (area_height : length) (pbvblst : pb_vert_box list) : evaled_vert_box list * evaled_vert_box list * (pb_vert_box list) option =

  let calculate_badness_of_page_break hgttotal =
    let hgtdiff = area_height -% hgttotal in
    let gap = int_of_float (hgtdiff /% (Length.of_pdf_point 0.1)) in
    if hgtdiff <% Length.zero then
      10000 - gap
        (* -- `gap` is negative -- *)
    else
      gap
  in

  (* --
     `normalize_after_break` just removes
     one `clear-page` or one breakable skip
     -- *)
  let normalize_after_break pbvblst =
    let rec omit_redundant_clear pbvblst =
      match pbvblst with
      | (PBClearPage, _) :: pbvbtail   -> Some(pbvbtail)
      | (PBVertSkip(_), _) :: pbvbtail -> omit_redundant_clear pbvbtail
      | (PBVertLine(_), _) :: _
      | (PBVertFrame(_), _) :: _       -> None
      | (PBHookPageBreak(_), _) :: _   -> assert false
      | []                             -> Some([])  (* -- the original `pbvblst` consists only of spaces -- *)
    in
    let pbvblst =
      match omit_redundant_clear pbvblst with
      | Some(pbvbomitted) -> pbvbomitted
      | None              -> pbvblst
    in
    match pbvblst with
    | (PBClearPage, _) :: _                  -> assert false
    | [] | (PBVertSkip(_), Breakable) :: []  -> NormalizedEmpty
    | (PBVertSkip(_), Breakable) :: pbvbtail -> Normalized(pbvbtail)
    | _                                      -> Normalized(pbvblst)
  in

  let getting_worse ans ans_last =
    ans.badness > ans_last.badness && ans_last.height <% ans.height
  in

  let rec aux (prev : pb_accumulator) (pbvblst : pb_vert_box list) : pb_answer * pb_rest =
    match pbvblst with
    | (PBVertLine(reach, hgt, dpt, imhbs), br) :: pbvbtail ->
        let hgtline = hgt +% (Length.negate dpt) in
        let (evhbs, imvbss_footnote) = PageInfo.embed_page_info pbinfo imhbs in
        let (evvbs_footnote, _) = PageInfo.embed_page_info_vert pbinfo (List.concat imvbss_footnote) in
          (* Ignores footnote designation in footnote. *)
        let hgtnewfootnote = get_height_of_evaled_vert_box_list evvbs_footnote in
        let hgttotalB = prev.total_height in
        let hgtB = hgttotalB +% prev.skip_after_break in
        let badnsB = calculate_badness_of_page_break hgtB in
        let hgttotalA = hgttotalB +% hgtline +% hgtnewfootnote in
        let hgtA = hgttotalA +% prev.skip_after_break in
        let badnsA = calculate_badness_of_page_break hgtA in
        let bodyA = Alist.extend (Alist.cat prev.solid_body prev.discardable) (EvVertLine(reach, hgt, dpt, evhbs)) in
        let footnoteA = Alist.append prev.solid_footnote evvbs_footnote in
        let last_breakable = prev.last_breakable in
        let open EscapeMonad in
        let esc =
          begin
            match prev.allow_break with
            | Unbreakable ->
                continue last_breakable

            | Breakable ->
              (* -- if the point immediately BEFORE the line is a page-breaking opportunity -- *)
                let ansB =
                  let bodyB = prev.solid_body in
                  let remainsB = Normalized(pbvblst) in
                  {
                    division = Inside(bodyB, remainsB);
                    footnote = prev.solid_footnote;
                    height   = hgtB;
                    badness  = badnsB;
                  }
                in
                if getting_worse ansB last_breakable then
                (* -- if getting worse, outputs a page at the last breakable point. -- *)
  (*
                  let () = Format.printf "PageBreak> page %d (%d): LINE\n" pbinfo.current_page_number prev.depth in  (* for debug *)
  *)
                  escape (last_breakable, Remains)
                else
                  continue ansB
          end >>= fun last_breakable ->
          begin
            match br with
            | Unbreakable ->
                continue last_breakable

            | Breakable ->
                let ansA =
                  {
                    division = Inside(prev.solid_body, normalize_after_break pbvblst);
                    footnote = footnoteA;
                    height   = hgtA;
                    badness  = badnsA;
                  }
                in
                if getting_worse ansA last_breakable then
                  escape (last_breakable, Remains)
                else
                  continue ansA
          end >>= fun last_breakable ->
            escape @@ aux {
              depth = prev.depth;  (* -- mainly for debugging -- *)
              skip_after_break = prev.skip_after_break;
              last_breakable = last_breakable;
              allow_break    = br;
              solid_body     = bodyA;
              solid_footnote = footnoteA;
              discardable    = Alist.empty;
              total_height   = hgttotalA;
            } pbvbtail
        in
        force esc

    | (PBVertSkip(debug_margins, vskip), br) :: pbvbtail ->
        let hgttotalB = prev.total_height in
        let hgtB = hgttotalB +% prev.skip_after_break in
        let badnsB = calculate_badness_of_page_break hgtB in
        let hgttotalA = hgttotalB +% vskip in
        let (bodyA, discardableA) =
          match br with
          | Breakable ->
              (prev.solid_body, Alist.extend prev.discardable (EvVertFixedEmpty(debug_margins, vskip)))

          | Unbreakable ->
              (Alist.extend (Alist.cat prev.solid_body prev.discardable) (EvVertFixedEmpty(debug_margins, vskip)), Alist.empty)
        in
        let last_breakable = prev.last_breakable in
        let open EscapeMonad in
        let esc =
          begin
            match prev.allow_break &-& br with
            | Unbreakable ->
              (* -- if either the point before the skip or that after the skip is unbreakable -- *)
                continue last_breakable

            | Breakable ->
                let ansB =
                  let bodyB = prev.solid_body in
                  let remainsB = normalize_after_break pbvblst in
                  let footnoteB = prev.solid_footnote in
                  {
                    division = Inside(bodyB, remainsB);
                    footnote = footnoteB;
                    height   = hgtB;
                    badness  = badnsB;
                  }
                in
                if getting_worse ansB last_breakable then
(*
                  let () = Format.printf "PageBreak> page %d (%d): BREAKABLE\n" pbinfo.current_page_number prev.depth in  (* for debug *)
                  begin  (* begin: for debug *)
                    match prev.last_breakable.division with
                    | Inside(evvbacc, remains) ->
                        begin
                          match (Alist.to_list_rev evvbacc, remains) with
                          | (e1 :: e2 :: e3 :: _, Normalized(p1 :: p2 :: p3 :: _)) ->
                              Format.printf "PageBreak> last breakable:@ @[<hov>body:@ ...@ %a@],@ @[<hov>rest:@ %a@ ...@]\n"
                                (Format.pp_print_list pp_evaled_vert_box) [e3; e2; e1]
                                (Format.pp_print_list pp_pb_vert_box) [p1; p2; p3]

                          | _ ->
                              Format.printf "PageBreak> last breakable: less than 3\n"
                        end

                    | Outside ->
                        Format.printf "PageBreak> last breakable: outside\n"
                  end;  (* end: for debug *)
*)
                  escape (last_breakable, Remains)
                else
                  continue ansB
          end >>= fun last_breakable ->
          escape @@ aux {
            depth = prev.depth;  (* -- mainly for debugging -- *)
            skip_after_break = prev.skip_after_break;
            last_breakable = last_breakable;
            allow_break    = br;
            solid_body     = bodyA;
            solid_footnote = prev.solid_footnote;
            discardable    = discardableA;
            total_height   = hgttotalA;
          } pbvbtail
        in
        force esc

    | (PBClearPage, _) :: pbvbtail ->
        let remains =
          match pbvbtail with
          | []     -> NormalizedEmpty
          | _ :: _ -> Normalized(pbvbtail)
        in
        let ans =
          {
            division = Inside(prev.solid_body, remains);
            footnote = prev.solid_footnote;
            height   = prev.total_height +% prev.skip_after_break;
            badness  = 0;
          }
        in
(*
        let () = Format.printf "PageBreak> page %d (%d): CLEAR-PAGE\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
        (ans, Remains)

    | (PBVertFrame(midway, pads, decoS, decoH, decoM, decoT, wid, pbvblstsub), br) :: pbvbtail ->
        let hgttotal = prev.total_height in
        let hgtB = hgttotal +% prev.skip_after_break in
        let badnsB = calculate_badness_of_page_break hgtB in
        let last_breakable = prev.last_breakable in
        let open EscapeMonad in
        let esc =
          begin
            match prev.allow_break with
            | Unbreakable ->
                continue last_breakable

            | Breakable ->
                let ansB =
                  {
                    division = Inside(prev.solid_body, Normalized(pbvblst));
                    footnote = prev.solid_footnote;
                    height   = hgtB;
                    badness  = badnsB;
                  }
                in
                if getting_worse ansB last_breakable then
                  escape (ansB, Remains)
                else
                  continue ansB
          end >>= fun last_breakable ->
          let (ans_sub, rest_sub) =
            let hgttotal_before = hgttotal +% pads.paddingT in
            aux {
              depth = prev.depth + 1;  (* -- mainly for debugging -- *)
              skip_after_break = pads.paddingB +% prev.skip_after_break;
              last_breakable = { last_breakable with division = Outside; };
              allow_break    = Unbreakable;
              solid_body     = Alist.empty;
              solid_footnote = prev.solid_footnote;
              discardable    = Alist.empty;
              total_height   = hgttotal_before;
            } pbvblstsub
              (* -- propagates total height and footnotes, but does NOT propagate body -- *)
          in
          begin
            match rest_sub with
            | Remains ->
              (* -- if the page-breaking point is determined when traversing the contents in the frame -- *)
                begin
                  match ans_sub.division with
                  | Outside ->
                    (* -- if the page-breaking should occur BEFORE entering the frame -- *)
(*
                      let () = Format.printf "PageBreak> page %d (%d): FRAME remains/outside\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
                      escape (last_breakable, Remains)

                  | Inside(body_sub, remains_sub) ->
                    (* -- if the page-breaking point was found in the frame -- *)
                      let bodyM =
                        let (decosub, pads) =
                          match midway with
                          | Midway    -> (decoM, pads)
                          | Beginning -> (decoH, pads)
                        (* --
                           design consideration:
                           `{ pads with paddingT = Length.zero; paddingB = Length.zero; }` may be better than `pads`
                           when `midway` is `Midway`.
                           -- *)
                        in
                        Alist.extend (Alist.cat prev.solid_body prev.discardable)
                          (EvVertFrame(pads, pbinfo, decosub, wid, Alist.to_list body_sub))
                      in
                      let remainsM =
                        match remains_sub with
                        | NormalizedEmpty         -> normalize_after_break pbvbtail
                        | Normalized(remains_sub) -> Normalized((PBVertFrame(Midway, pads, decoS, decoH, decoM, decoT, wid, remains_sub), br) :: pbvbtail)
                      in
(*
                      let () = Format.printf "PageBreak> page %d (%d): FRAME remains/inside\n" pbinfo.current_page_number prev.depth in  (* for debug *)
                      let () = let x = match remains_sub with NormalizedEmpty -> [] | Normalized(x) -> x in Format.printf "PageBreak> remains (%d): %a\n" (List.length x) (Format.pp_print_list pp_pb_vert_box) x in  (* for debug *)
*)
                      let ansM = { ans_sub with division = Inside(bodyM, remainsM); } in
                      escape (ansM, Remains)
                end

            | Finished(body_sub_all, footnote_sub_all, hgttotal_all) ->
              (* -- if the contents `pbvblstsub` was totally traversed before the page-breaking point is determined -- *)
                let hgttotal_after = hgttotal_all +% pads.paddingB in
                let hgtA = hgttotal_after +% prev.skip_after_break in
                let badnsA = calculate_badness_of_page_break hgtA in
                begin
                  match ans_sub.division with
                  | Outside ->
                    (* -- if no point in the frame was found suitable for page breaking,
                          i.e., the frame can be treated as if it were a single line -- *)
                      let bodyA =
                        let (deco_sub, pads) =
                          match midway with
                          | Midway    -> (decoT, pads)
                          | Beginning -> (decoS, pads)
                        in
                        Alist.extend (Alist.cat prev.solid_body prev.discardable)
                         (EvVertFrame(pads, pbinfo, deco_sub, wid, Alist.to_list body_sub_all))
                      in
                      let footnoteA = footnote_sub_all in
                      begin
                        match br with
                        | Unbreakable ->
(*
                            let () = Format.printf "PageBreak> page %d (%d) --> continue by FRAME finished/outside (non-breakable)\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
                            continue last_breakable

                        | Breakable ->
                          (* -- if breakable immediately AFTER the frame -- *)
                            let ansA =
                              {
                                division = Inside(bodyA, normalize_after_break pbvbtail);
                                footnote = footnoteA;
                                height   = hgtA;
                                badness  = badnsA;
                              }
                            in
                            if getting_worse ansA last_breakable then
(*
                              let () = Format.printf "PageBreak> page %d (%d): FRAME finished/outside\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
                              escape (last_breakable, Remains)
                            else
                            (* -- if the point immediately AFTER the frame should be a new last breakable opportunity -- *)
(*
                              let () = Format.printf "PageBreak> page %d (%d) --> continue by FRAME finished/outside (non-optimal %a)\n" pbinfo.current_page_number prev.depth pp_length hgttotal_after in  (* for debug *)
*)
                              continue ansA

                      end >>= fun last_breakable ->
                        escape @@ aux {
                          depth = prev.depth;  (* -- mainly for debugging -- *)
                          skip_after_break = prev.skip_after_break;
                          last_breakable = last_breakable;
                          allow_break    = br;
                          solid_body     = bodyA;
                          solid_footnote = footnoteA;
                          discardable    = Alist.empty;
                          total_height   = hgttotal_after;
                        } pbvbtail

                  | Inside(body_sub, remains_sub) ->
                      let last_breakable =
                        let bodyM =
                          let (deco, pads) =
                            match midway with
                            | Midway    -> (decoM, pads)
                            | Beginning -> (decoH, pads)
                          in
                          Alist.extend (Alist.cat prev.solid_body prev.discardable)
                            (EvVertFrame(pads, pbinfo, deco, wid, Alist.to_list body_sub))
                        in
                        let remainsM =
                          match remains_sub with
                          | NormalizedEmpty         -> normalize_after_break pbvbtail
                          | Normalized(remains_sub) -> Normalized((PBVertFrame(Midway, pads, decoS, decoH, decoM, decoT, wid, remains_sub), br) :: pbvbtail)
                        in
                        { ans_sub with division = Inside(bodyM, remainsM); }
                      in
                      let bodyA =
                        let (deco, pads) =
                          match midway with
                          | Midway    -> (decoT, pads)
                          | Beginning -> (decoS, pads)
                            (* --
                               design consideration:
                               `{ pads with paddingT = Length.zero; }` may be better than `pads`
                               when `midway` is `Midway`.
                               -- *)
                        in
                        Alist.extend (Alist.cat prev.solid_body prev.discardable)
                          (EvVertFrame(pads, pbinfo, deco, wid, Alist.to_list body_sub_all))
                      in
                      let footnoteA = footnote_sub_all in
                      begin
                        match br with
                        | Unbreakable ->
                            continue last_breakable

                        | Breakable ->
                            let ansA =
                              {
                                division = Inside(bodyA, normalize_after_break pbvbtail);
                                footnote = footnote_sub_all;
                                height   = hgtA;
                                badness  = badnsA;
                              }
                            in
                            if getting_worse ansA last_breakable then
                            (* -- if appropriate to break page at the last opportunity in the frame -- *)
(*
                              let () = Format.printf "PageBreak> page %d (%d): FRAME finished/inside\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
                              escape (last_breakable, Remains)
                            else
                            (* -- if the contents in the given frame is displayed in a single page -- *)
(*
                              let () = Format.printf "PageBreak> page %d (%d) --> continue by FRAME finished/inside (non-optimal %a)\n" pbinfo.current_page_number prev.depth pp_length hgttotal_after in  (* for debug *)
*)
                              continue ansA
                      end >>= fun last_breakable ->
                        escape @@ aux {
                          depth = prev.depth;  (* -- mainly for debugging -- *)
                          skip_after_break = prev.skip_after_break;
                          last_breakable = last_breakable;
                          allow_break    = br;
                          solid_body     = bodyA;
                          solid_footnote = footnoteA;
                          discardable    = Alist.empty;
                          total_height   = hgttotal_after;
                        } pbvbtail
                end
          end
        in
        force esc

    | (PBHookPageBreak(hookf), _) :: pbvbtail ->
        aux { prev with
          solid_body = Alist.extend prev.solid_body (EvVertHookPageBreak(hookf));
        } pbvbtail

    | [] ->
        let ans =
          if prev.depth = 0 then
            {
              division = Inside(prev.solid_body, NormalizedEmpty);
              footnote = prev.solid_footnote;
              height   = prev.total_height;
              badness  = prev.last_breakable.badness;
            }
          else
            prev.last_breakable
        in
(*
        let () = Format.printf "PageBreak> page %d (%d): FINAL\n" pbinfo.current_page_number prev.depth in  (* for debug *)
*)
        (ans, Finished(prev.solid_body, prev.solid_footnote, prev.total_height))
          (* -- discards `prev.discardable` -- *)
  in
  let (ans, rest) =
    aux {
      depth = 0;  (* -- mainly for debugging -- *)
      skip_after_break = Length.zero;
      last_breakable = {
        badness  = initial_badness;
        division = Outside;
        footnote = Alist.empty;
        height   = Length.zero;
      };
      allow_break    = Unbreakable;
      solid_body     = Alist.empty;
      solid_footnote = Alist.empty;
      discardable    = Alist.empty;
      total_height   = Length.zero;
    } pbvblst
  in
  match rest with
  | Remains ->
      let (body, remains) =
        match ans.division with
        | Inside(body, Normalized(remains)) -> (body, remains)
        | Inside(body, NormalizedEmpty)     -> (body, [])
        | Outside                           -> (Alist.empty, [])
      in
      (Alist.to_list body, Alist.to_list ans.footnote, Some(remains))

  | Finished(body_all, footnote_all, _) ->
      (Alist.to_list body_all, Alist.to_list footnote_all, None)


(* `squash_margins` receives:

   * `prev_bottom`: the upper bottom margin
   * `vblst`: the rest of block contents that remains to be traversed

   and returns `(br, pbvbskip)` where:

   * `br`: whether page-breakable between `prev_bottom` and `vblst`
   * `pbvbskip`: the vertical contents corresponding to the space originating from margins *)
let squash_margins (prev_bottom : (breakability * length) option) (vblst : vert_box list) : breakability * pb_vert_box list =
  let open EscapeMonad in
  let esc =
    begin
      match vblst with
      | []
      | VertClearPage :: _
      | VertHookPageBreak(_) :: _ ->
          escape (Unbreakable, [])

      | VertParagraph(margins, _) :: _
      | VertFrame(margins, _, _, _, _, _, _, _) :: _ ->
          continue margins.margin_top

      | VertFixedBreakable(_) :: _ ->
          continue None

    end >>= fun next_top ->
    escape @@ begin
      match (prev_bottom, next_top) with
      | (None, None)                                       -> (Breakable, [])
      | (None, Some((br2, len2)))                          -> (br2, [ (PBVertSkip(LowerOnly(br2), len2), br2) ])
      | (Some((br1, len1)), None)                          -> (br1, [ (PBVertSkip(UpperOnly(br1), len1), br1) ])
      | (Some((br1, len1) as p1), Some((br2, len2) as p2)) -> let br = br1 &-& br2 in (br, [ (PBVertSkip(Both(p1, p2), Length.max len1 len2), br) ])
    end
  in
  force esc


(* --
   `normalize_paragraph` receives

   * `parelems`: the contents of the paragraph
   * `br`: whether page breaking is allowed immediately after the paragraph

   and returns its corresponding normalized block contents.
   -- *)
let normalize_paragraph (parelems : paragraph_element list) (br : breakability) : pb_vert_box list =
  let pbmains =
    parelems |> List.map (function
    | VertParagLine(reach, hgt, dpt, imhbs) -> PBVertLine(reach, hgt, dpt, imhbs)
    | VertParagSkip(len)                    -> PBVertSkip(BetweenLines, len)
    )
  in
  match List.rev pbmains with
  | last :: others -> List.rev ((last, br) :: (others |> List.map (fun pbmain -> (pbmain, Breakable))))
  | []             -> []


type top_margin_needed =
  | TopMarginNeeded
  | TopMarginProhibited


(* --
   normalize:
     squashes bottom/top margins into spaces.
   -- *)
let normalize (vblst : vert_box list) : pb_vert_box list =

  let append_top_margin_if_needed needed margins pbvbacc =
    match (needed, margins.margin_top) with
    | (TopMarginNeeded, Some((br2, len2))) ->
        Alist.extend pbvbacc (PBVertSkip(LowerOnly(br2), len2), br2)

    | _ ->
        pbvbacc
  in

  let rec aux (prev_bottom : (breakability * length) option) (needed : top_margin_needed) pbvbacc vblst =
    match vblst with
    | [] ->
        Alist.to_list pbvbacc

    | VertParagraph(margins, parelems) :: vbtail ->
        let pbvbacc = append_top_margin_if_needed needed margins pbvbacc in
        let (br, pbvbskip) = squash_margins margins.margin_bottom vbtail in
        let pbvbpar = normalize_paragraph parelems br in
        aux margins.margin_bottom TopMarginProhibited (Alist.append (Alist.append pbvbacc pbvbpar) pbvbskip) vbtail

    | VertFixedBreakable(vskip) :: vbtail ->
        aux None TopMarginNeeded (Alist.extend pbvbacc (PBVertSkip(Fixed, vskip), Breakable)) vbtail
          (* -- appending a skip that stems from a top margin is needed only after fixed skips -- *)

    | VertFrame(margins, pads, decoS, decoH, decoM, decoT, wid, vblstsub) :: vbtail ->
        let pbvbacc = append_top_margin_if_needed needed margins pbvbacc in
        let (br, pbvbskip) = squash_margins margins.margin_bottom vbtail in
        let pbvblstsub = aux None TopMarginProhibited Alist.empty vblstsub in
        let pbvb = (PBVertFrame(Beginning, pads, decoS, decoH, decoM, decoT, wid, pbvblstsub), br) in
        aux margins.margin_bottom TopMarginProhibited (Alist.append (Alist.extend pbvbacc pbvb) pbvbskip) vbtail

    | VertClearPage :: vbtail ->
        aux None TopMarginProhibited (Alist.extend pbvbacc (PBClearPage, Breakable)) vbtail

    | VertHookPageBreak(hookf) :: vbtail ->
        let (br, _) = squash_margins prev_bottom vbtail in
        aux prev_bottom needed (Alist.extend pbvbacc (PBHookPageBreak(hookf), br)) vbtail

  in
  aux None TopMarginProhibited Alist.empty vblst


let solidify (vblst : vert_box list) : intermediate_vert_box list =
  let rec aux pbvblst =
    pbvblst |> List.map (fun (pbvbmain, _) ->
      match pbvbmain with
      | PBVertLine(reach, hgt, dpt, imhbs) -> ImVertLine(reach, hgt, dpt, imhbs)
      | PBVertSkip(debug_margins, len)     -> ImVertFixedEmpty(debug_margins, len)
      | PBClearPage                        -> ImVertFixedEmpty(Fixed, Length.zero)

      | PBVertFrame(_, pads, decoS, _decoH, _decoM, _decoT, wid, pbvblstsub) ->
          let imvblstsub = aux pbvblstsub in
          ImVertFrame(pads, decoS, wid, imvblstsub)

      | PBHookPageBreak(hookf) ->
          ImVertHookPageBreak(hookf)
    )
  in
  let pbvblst = normalize vblst in
  aux pbvblst


let chop_single_column_with_insertion (pbinfo : page_break_info) (content_height : length) (columnhookf : unit -> vert_box list) (pbvblst : pb_vert_box list) =
  let vblst_inserted = columnhookf () in  (* Invokes the column hook function. *)
  let pbvblst_inserted = normalize vblst_inserted in
  chop_single_column pbinfo content_height (List.append pbvblst_inserted pbvblst)


let main (absname_out : abs_path) ~(paper_size : length * length) (columnhookf : column_hook_func) (pagecontf : page_content_scheme_func) (pagepartsf : page_parts_scheme_func) (vblst : vert_box list) : HandlePdf.t =

  let pdfinit = HandlePdf.create_empty_pdf absname_out in

  let rec aux pageno (pdfacc : HandlePdf.t) pbvblst =
    let pbinfo = { current_page_number = pageno; } in
    let pagecontsch = pagecontf pbinfo in  (* -- invokes the page scheme function -- *)
    let (evvblstpage, footnote, restopt) =
      chop_single_column_with_insertion pbinfo pagecontsch.page_content_height columnhookf pbvblst
    in
    let page = HandlePdf.make_empty_page ~paper_size pbinfo pagecontsch in
    let page = HandlePdf.add_column_to_page page Length.zero evvblstpage footnote in
    let pdfaccnew = pdfacc |> HandlePdf.write_page page pagepartsf in
    match restopt with
    | None       -> pdfaccnew
    | Some(rest) -> aux (pageno + 1) pdfaccnew rest
  in
  let pbvblst = normalize vblst in
  aux 1 pdfinit pbvblst


let main_multicolumn (absname_out : abs_path) ~(paper_size : length * length) (origin_shifts : length list) (columnhookf : column_hook_func) (columnendhookf : column_hook_func) (pagecontf : page_content_scheme_func) (pagepartsf : page_parts_scheme_func) (vblst : vert_box list) : HandlePdf.t =

  let pdfinit = HandlePdf.create_empty_pdf absname_out in

  let page_number_limit = OptionState.get_page_number_limit () in

  let rec iter_on_column
      (pbinfo : page_break_info) (content_height : length)
      (page : HandlePdf.page) (pbvbs : pb_vert_box list) (origin_shifts : length list) =
    match origin_shifts with
    | [] ->
        let vbs_inserted = columnendhookf () in  (* Invokes the column end hook function. *)
        let pbvbs_inserted = normalize vbs_inserted in
        (page, List.append pbvbs_inserted pbvbs)

    | origin_shift :: origin_shift_tail ->
        (* Makes a column. *)
        let (body, footnote, restopt) =
          chop_single_column_with_insertion pbinfo content_height columnhookf pbvbs
        in

        (* Adds the column to the page and invokes hook functions. *)
        let page = HandlePdf.add_column_to_page page origin_shift body footnote in
        begin
          match restopt with
          | None ->
              let vbs_inserted = columnendhookf () in  (* Invokes the column end hook function. *)
              let pbvbs_inserted = normalize vbs_inserted in
              (page, pbvbs_inserted)

          | Some(pbvbs) ->
              iter_on_column pbinfo content_height page pbvbs origin_shift_tail
        end

  in

  let origin_shifts = Length.zero :: origin_shifts in

  let rec iter_on_page (pageno : int) (pdfacc : HandlePdf.t) (pbvbs : pb_vert_box list) =
    if pageno > page_number_limit then
      raise (PageNumberLimitExceeded(page_number_limit))
    else
      let pbinfo = { current_page_number = pageno; } in
      let pagecontsch = pagecontf pbinfo in  (* Invokes the page scheme function. *)
      let content_height = pagecontsch.page_content_height in

      (* Creates an empty page and iteratively adds columns to it. *)
      let page = HandlePdf.make_empty_page ~paper_size pbinfo pagecontsch in
      let (page, rest) = iter_on_column pbinfo content_height page pbvbs origin_shifts in
      let pdfacc = pdfacc |> HandlePdf.write_page page pagepartsf in
      match rest with
      | []     -> pdfacc
      | _ :: _ -> iter_on_page (pageno + 1) pdfacc rest
  in
  let pbvblst = normalize vblst in
  iter_on_page 1 pdfinit pbvblst


let adjust_to_first_line (imvblst : intermediate_vert_box list) : length * length =
  let rec aux optinit totalhgtinit imvblst =
    imvblst |> List.fold_left (fun (opt, totalhgt) imvb ->
      match (imvb, opt) with
      | (ImVertLine(_, hgt, dpt, _), None)  -> (Some(totalhgt +% hgt), totalhgt +% hgt +% (Length.negate dpt))
      | (ImVertLine(_, hgt, dpt, _), _)     -> (opt, totalhgt +% hgt +% (Length.negate dpt))
      | (ImVertFixedEmpty(_, vskip), _)     -> (opt, totalhgt +% vskip)

      | (ImVertFrame(pads, _, _, imvblstsub), _) ->
          let totalhgtbefore = totalhgt +% pads.paddingT in
          let (optsub, totalhgtsub) = aux opt totalhgtbefore imvblstsub in
          let totalhgtafter = totalhgtsub +% pads.paddingB in
            (optsub, totalhgtafter)

      | (ImVertHookPageBreak(_), _) ->
          (opt, totalhgt)

    ) (optinit, totalhgtinit)
  in
  match aux None Length.zero imvblst with
  | (Some(hgt), totalhgt) -> (hgt, Length.negate (totalhgt -% hgt))
  | (None, totalhgt)      -> (Length.zero, Length.negate totalhgt)


let adjust_to_last_line (imvblst : intermediate_vert_box list) : length * length =
  let rec aux optinit totalhgtinit evvblst =
    let evvblstrev = List.rev evvblst in
      evvblstrev |> List.fold_left (fun (opt, totalhgt) imvblast ->
        match (imvblast, opt) with
        | (ImVertLine(_, hgt, dpt, _), None) -> (Some((Length.negate totalhgt) +% dpt), totalhgt +% (Length.negate dpt) +% hgt)
        | (ImVertLine(_, hgt, dpt, _), _)    -> (opt, totalhgt +% (Length.negate dpt) +% hgt)
        | (ImVertFixedEmpty(_, vskip), _)    -> (opt, totalhgt +% vskip)

        | (ImVertFrame(pads, _, _, evvblstsub), _) ->
            let totalhgtbefore = totalhgt +% pads.paddingB in
            let (optsub, totalhgtsub) = aux opt totalhgtbefore evvblstsub in
            let totalhgtafter = totalhgtsub +% pads.paddingT in
              (optsub, totalhgtafter)

        | (ImVertHookPageBreak(_), _) ->
            (opt, totalhgt)

      ) (optinit, totalhgtinit)
  in
  match aux None Length.zero imvblst with
  | (Some(dpt), totalhgt) -> (totalhgt +% dpt, dpt)
  | (None, totalhgt)      -> (totalhgt, Length.zero)


(*
let penalty_break_space = 100
let penalty_soft_hyphen = 1000

let () =
  let ( ~% ) = Length.of_pdf_point in
  begin
    FontInfo.initialize () ;
    let font0 = ("Arno", ~% 16.) in
    let font1 = (* ("Hlv", ~% 16.) *) font0 in
    let fontL = (* ("Hlv", ~% 32.) *) ("Arno", ~% 32.) in

    let fontK = (* ("KozMin", ~% 12.) *) ("Osaka", ~% 12.) in

    let word s = HorzPure(PHFixedString(font0, InternalText.of_utf_8 s)) in
    let word1 s = HorzPure(PHFixedString(font1, InternalText.of_utf_8 s)) in
    let wordL s = HorzPure(PHFixedString(fontL, InternalText.of_utf_8 s)) in

    let wordK s = HorzPure(PHFixedString(fontK, InternalText.of_utf_8 s)) in

    let margin = ~% 2. in
    let pads = {
      paddingL = ~% 2. +% margin;
      paddingR = ~% 2. +% margin;
      paddingT = ~% 2. +% margin;
      paddingB = ~% 2. +% margin;
    } in
    let decostd =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos +% margin in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin *% 2. in
        [
          Rectangle((xposb, ypos +% dptb), (widb, hgtb -% dptb));
        ]
      )
    in
    let decoH =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos +% margin in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin in
        [
          GeneralPath((xposb +% widb, ypos +% hgtb), [
            LineTo(xposb, ypos +% hgtb);
            LineTo(xposb, ypos +% dptb);
            LineTo(xposb +% widb, ypos +% dptb);
          ]);
        ]
      )
    in
    let decoM =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid in
        [
          GeneralPath((xposb, ypos +% hgtb), [LineTo(xposb +% widb, ypos +% hgtb)]);
          GeneralPath((xposb, ypos +% dptb), [LineTo(xposb +% widb, ypos +% dptb)]);
        ]
      )
    in
    let decoT =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin in
        [
          GeneralPath((xposb, ypos +% hgtb), [
            LineTo(xposb +% widb, ypos +% hgtb);
            LineTo(xposb +% widb, ypos +% dptb);
            LineTo(xposb, ypos +% dptb);
          ]);
        ]
      )
    in
    let framed hblst = HorzPure(PHOuterFrame(pads, decostd, hblst)) in
    let iframed hblst = HorzPure(PHInnerFrame(pads, decostd, hblst)) in
    let fframed wid hblst = HorzPure(PHFixedFrame(pads, wid, decostd, hblst)) in
    let bframed hblst = HorzFrameBreakable(pads, ~% 5., ~% 5., decostd, decoH, decoM, decoT, hblst) in
    let space = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 6., ~% 2., ~% 3.)), None, None) in
    let space1 = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 7.5, ~% 3., ~% 3.)), None, None) in
    let spaceL = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 16., ~% 2., ~% 6.)), None, None) in
    let indentation = HorzPure(PHFixedEmpty(~% 64.)) in
    let fill = HorzPure(PHOuterFil) in
    let leading = ~% 24. in
    let paragraph_skip = ~% 16. in
    let soft_hyphen = HorzDiscretionary(penalty_soft_hyphen, None, Some(PHFixedString(font0, InternalText.of_utf_8 "-")), None) in
    let soft_hyphen1 = HorzDiscretionary(penalty_soft_hyphen, None, Some(PHFixedString(font1, InternalText.of_utf_8 "-")), None) in
    let rec repeat n lst = if n <= 0 then [] else lst @ (repeat (n - 1) lst) in
    let vblst =
      [
        VertParagraph(~% 24., [
          fill; wordL "Sample"; spaceL; wordL "Text"; fill;
        ]);
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 24., [
          framed [fill; wordL "Sample"; spaceL; wordL "Text"; fill;];
        ]);
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
          word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog.";
          space;
          word "My"; space; word "quiz"; space; word "above"; space; word "the"; space; word "kiwi"; space; word "juice"; space;
          word "needs"; space; word "price"; soft_hyphen ; word "less"; space; word "fixing."; space;
          word "fluffy"; space; word "soufflés"; space; word "office"; space; word "Té"; fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          word "Now"; space; word "we"; space; word "deal"; space; word "with"; space;
          framed [word1 "kerning"; space; word1 "pair";]; space; word "information!"; fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
(*
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          space;
          word1 "A"; space1;
*)
          framed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; framed [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
(*
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
*)
          word1 "A"; space1;
          iframed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; iframed [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);
(*
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 20., [
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          fframed (~% 300.) [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; fframed (~% 120.) [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);
*)
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          indentation;
          bframed [
            word1 "Lorem"; space1; word1 "ipsum"; space1; word1 "dolor"; space1; word1 "sit"; space1; word1 "amet,"; space1;
            word1 "consectetur"; space1; word1 "adipiscing"; space1; word1 "elit,"; space1;
            word1 "sed"; space1; word1 "do"; space1; word1 "eiusmod"; space1; word1 "tempor"; space1; word1 "incididunt"; space1;
            word1 "ut"; space1; word1 "labore"; space1; word1 "et"; space1; word1 "dolore"; space1; word1 "magna"; space1; word1 "aliqua."; space1;
            bframed [
              word1 "Ut"; space1; word1 "enim"; space1; word1 "ad"; space1; word1 "minim"; space1; word1 "veniam,";
            ]; space1;
            word1 "quis"; space1; word1 "nostrud"; space1; word1 "exercitation"; space1; word1 "ullamco"; space1;
            word1 "laboris"; space1; word1 "nisi"; space1; word1 "ut"; space1; word1 "aliquip"; space1;
            word1 "ex"; space1; word1 "ea"; space1; word1 "commodo"; space1; word1 "consequat.";

          ]; fill;
        ]);

      ] @ repeat 2 [
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          indentation;
          word1 "Lorem"; space; word1 "ipsum"; space; word "dolor"; space; word "sit"; space; word "amet,"; space;
          word "consectetur"; space; word "adipiscing"; space; word "elit,"; space;
          word "sed"; space; word "do"; space; word "eiusmod"; space; word "tempor"; space; word "incididunt"; space;
          word "ut"; space; word "labore"; space; word "et"; space; word "dolore"; space; word "magna"; space; word "aliqua."; space;
          word "Ut"; space; word "enim"; space; word "ad"; space; word "minim"; space; word "veniam,"; space;
          word " quis"; space; word "nostrud"; space; word "exercitation"; space; word "ullamco"; space;
          word "laboris"; space; word "nisi"; space; word "ut"; space; word "aliquip"; space;
          word "ex"; space; word "ea"; space; word "commodo"; space; word "consequat."; space;
          word "Duis"; space; word "aute"; space; word "irure"; space; word "dolor"; space;
          word "in"; space; word "reprehenderit"; space; word "in"; space; word "voluptate"; space;
          word "velit"; space; word "esse"; space; word "cillum"; space; word "dolore"; space;
          word "eu"; space; word "fugiat"; space; word "nulla"; space; word "pariatur."; space;
          word "Excepteur"; space; word "sint"; space; word "occaecat"; space; word "cupidatat"; space;
          word "non"; space; word "proident,"; space; word "sunt"; space; word "in"; space; word "culpa"; space;
          word "qui"; space; word "officia"; space; word "deserunt"; space; word "mollit"; space; word "anim"; space;
          word "id"; space; word "est"; space; word "laborum."; fill;
        ]);
      ]
    in
    let pdfscheme = HandlePdf.create_empty_pdf "hello5.pdf" in
    try
      begin
        main_for_unit_test pdfscheme vblst ;
      end
    with
    | FontFormat.FontFormatBroken(e) -> Otfm.pp_error Format.std_formatter e
  end
*)
