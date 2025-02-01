
open LengthInterface
open HorzBox
open CharBasis
open LineBreakBox


let to_chunks (ctx : context_main) (uchs : Uchar.t list) (alw_last : break_opportunity) : break_opportunity * line_break_chunk list =
  let (alw_first, tris) = LineBreakDataMap.append_break_opportunity uchs alw_last in
  let scrs = ScriptDataMap.divide_by_script ctx tris in
  let chunks = scrs |> List.map (fun chunkmain -> (ctx, chunkmain)) in
  (alw_first, chunks)


let pure_space_between_scripts ctx1 ctx2 size (script1 : script) (lbc1 : line_break_class) (script2 : script) (lbc2 : line_break_class) =
  if is_open_punctuation lbc1 || is_close_punctuation lbc2 then
    None
  else
    match
      (ctx1.script_space_map |> ScriptSpaceMap.find_opt (script1, script2),
       ctx2.script_space_map |> ScriptSpaceMap.find_opt (script1, script2))
    with
    | (None, None) ->
        None

    | (Some(tuple), None)
    | (None, Some(tuple)) ->
        let (r0, r1, r2) = tuple in
        let metrics = (natural (size *% r0), size *% r1, size *% r2) in
        Some(LBAtom{ metrics; main = EvHorzEmpty })

    | (Some(tuple1), Some(tuple2)) ->
        let (r10, r11, r12) = tuple1 in
        let (r20, r21, r22) = tuple2 in
        let (r0, r1, r2) = (max r10 r20, max r11 r21, max r12 r22) in
        let metrics = (natural (size *% r0), size *% r1, size *% r2) in
        Some(LBAtom{ metrics; main = EvHorzEmpty })


let space_width_info (ctx : context_main) : length_info =
  let size = ctx.font_size in
    (* Uses font size directly, not multiplied by the ratio of the dominant script. *)
  let widnatural = size *% ctx.space_natural in
  let widshrink  = size *% ctx.space_shrink in
  let widstretch = size *% ctx.space_stretch in
  make_width_info widnatural widshrink widstretch


let line_break_space (metrics : metrics) : lb_pure_box =
  LBAtom{ metrics; main = EvHorzEmpty }


let pure_space ctx : lb_pure_box =
  let widinfo = space_width_info ctx in
  line_break_space (widinfo, Length.zero, Length.zero)


let get_corrected_font_size ctx script =
  let (_, font_ratio, _) = get_font_with_ratio ctx script in
  ctx.font_size *% font_ratio


(* Inserts a shrinkable CJK halfwidth space. *)
let pure_halfwidth_space_soft (fontsize : length) : lb_pure_box =
  let widinfo = make_width_info (fontsize *% 0.5) (fontsize *% 0.25) (fontsize *% 0.25) in
  line_break_space (widinfo, Length.zero, Length.zero)


(* Inserts a non-shrinkable CJK halfwidth space. *)
let pure_halfwidth_space_hard (fontsize : length) : lb_pure_box =
  let widinfo = make_width_info (fontsize *% 0.5) Length.zero (fontsize *% 0.25) in
  line_break_space (widinfo, Length.zero, Length.zero)


(* Inserts glue between directly adjacent CJK characters. *)
let adjacent_space (ctx1 : context_main) (ctx2 : context_main) =
  let fontsize = Length.max ctx1.font_size ctx2.font_size in
  let ratio = max ctx1.adjacent_stretch ctx2.adjacent_stretch in
  let widstretch = fontsize *% ratio in
  let widinfo = make_width_info Length.zero Length.zero widstretch in
  line_break_space (widinfo, Length.zero, Length.zero)


(* Inserts a solid backward halfwidth kern for CJK characters. *)
let halfwidth_kern (ctx : context_main) (script : script) : lb_box =
  let size = get_corrected_font_size ctx script in
  LBPure(line_break_space (natural (Length.negate (size *% 0.5)), Length.zero, Length.zero))


(* Inserts a solid backward quaterwidth kern for CJK characters. *)
let quarterwidth_kern (ctx : context_main) (script : script) : lb_box =
  let size = get_corrected_font_size ctx script in
  LBPure(line_break_space (natural (Length.negate (size *% 0.25)), Length.zero, Length.zero))


let breakable_space (lphbf : horz_box list -> lb_pure_box list) (ctx : context_main) : lb_box =
  let dscrid = DiscretionaryID.fresh () in
  let lphbs1 = lphbf ctx.before_word_break in
  let lphbs2 = lphbf ctx.after_word_break in
  LBDiscretionary{
    penalty  = ctx.space_badness;
    id       = dscrid;
    no_break = [ pure_space ctx ];
    pre      = lphbs1;
    post     = lphbs2;
  }


let unbreakable_space (ctx : context_main) : lb_box =
  LBPure(pure_space ctx)


(* Perform conversion from `[A; B; C; D]` to [(A, BCD); (AB, CD); (ABC, D)]. *)
let generate_separation_list (uchsegss : (uchar_segment list) list) : (uchar_segment list * uchar_segment list) list =
  let rec aux acc revprefix suffix =
    match suffix with
    | [] ->
        Alist.to_list acc

    | _uchsegs :: [] ->
        Alist.to_list acc

    | uchsegs :: suffix_new ->
        let revprefix_new = Alist.append revprefix uchsegs in
        let acc = Alist.extend acc (Alist.to_list revprefix_new, List.concat suffix_new) in
        aux acc revprefix_new suffix_new
  in
  aux Alist.empty Alist.empty uchsegss


let make_string_atom (hsinfo : horz_string_info) (uchsegs : uchar_segment list) : lb_pure_box =
  let (otxt, width, height, depth) = FontInfo.get_metrics_of_word hsinfo uchsegs in
  let metrics = (natural width, height, depth) in
  LBAtom{ metrics; main = EvHorzString{ info = hsinfo; height; depth; output = otxt } }


(* Makes an alphabetic word or a CJK character. *)
let inner_string (ctx : context_main) (script : script) (uchsegs : uchar_segment list) : lb_box list =
  let hsinfo = get_string_info ctx script in
    match LoadHyph.lookup ctx.left_hyphen_min ctx.right_hyphen_min ctx.hyphen_dictionary uchsegs with
    | LoadHyph.Single(uchsegs) ->
        [ LBPure(make_string_atom hsinfo uchsegs) ]

    | LoadHyph.Fractions(uchsegss) ->
        let uchsegs0 = List.concat uchsegss in
        let lphb0 = make_string_atom hsinfo uchsegs0 in
        let lphb_hyphen = make_string_atom hsinfo [(Uchar.of_char '-', [])] in
          (* TODO: make hyphens changeable *)
        let seps = generate_separation_list uchsegss in
        let candidates =
          seps |> List.fold_left (fun dscracc (uchsegsP, uchsegsS) ->
            let lphbP = make_string_atom hsinfo uchsegsP in
            let lphbS = make_string_atom hsinfo uchsegsS in
            let dscrid = DiscretionaryID.fresh () in
              Alist.extend dscracc (dscrid, [ lphbP; lphb_hyphen ], [ lphbS ])
          ) Alist.empty |> Alist.to_list
        in
        [ LBDiscretionaryList{ penalty = ctx.hyphen_badness; no_break = [ lphb0 ]; candidates } ]


let discretionary_if_breakable alw (penalty : pure_badness) lphb =
  match alw with
  | AllowBreak ->
      let dscrid = DiscretionaryID.fresh () in
      LBDiscretionary{
        penalty;
        id       = dscrid;
        no_break = [ lphb ];
        pre      = [];
        post     = [];
      }

  | PreventBreak ->
      LBPure(lphb)


(* TODO: should refer to the context for spacing between two scripts *)
let pure_space_between_classes (ctx1, script1, lbc1) (ctx2, script2, lbc2) =
  let size1 = get_corrected_font_size ctx1 script1 in
  let size2 = get_corrected_font_size ctx2 script2 in
  let sizeM = Length.max size1 size2 in
  let hwhard1 = (pure_halfwidth_space_hard size1) in
  let hwsoft1 = (pure_halfwidth_space_soft size1) in
  let hwsoft2 = (pure_halfwidth_space_soft size2) in
  let hwsoftM = (pure_halfwidth_space_soft sizeM) in
  let hwhardM = (pure_halfwidth_space_hard sizeM) in
  match (lbc1, lbc2) with
  | (JLCP, JLOP) -> Some(hwsoftM)
  | (JLCM, JLOP) -> Some(hwsoftM)
  | (JLFS, JLOP) -> Some(hwhardM)
  | (_   , JLOP) -> Some(hwsoft2)
  | (JLCP, JLCM) -> None
  | (JLCP, JLFS) -> None
  | (JLCP, _   ) -> Some(hwsoft1)
  | (JLCM, _   ) -> Some(hwsoft1)
  | (JLFS, _   ) -> Some(hwhard1)
      (* TEMPORARY; SHOULD WRITE MORE based on JLreq *)
(*
  | (JLNonstarter(_, _), PreWord(_, _, _)) -> full_space
*)
  | _ -> None


let space_between_chunks info1 alw info2 : lb_box list =
  let (ctx1, script1, lbc1) = info1 in
  let (ctx2, script2, lbc2) = info2 in
  let badns = max ctx1.space_badness ctx2.space_badness in
  if not (script_equal script1 script2) then
    let size = Length.max ctx1.font_size ctx2.font_size in
      match pure_space_between_scripts ctx1 ctx2 size script1 lbc1 script2 lbc2 with
      | Some(lphb) ->
          [ discretionary_if_breakable alw badns lphb ]

      | None ->
        (* If there is no space between scripts: *)
          begin
            match pure_space_between_classes info1 info2 with
            | None       -> [ discretionary_if_breakable alw badns (adjacent_space ctx1 ctx2) ]
            | Some(lphb) -> [ discretionary_if_breakable alw badns lphb ]
          end
  else
  (* If scripts are the same: *)
    match pure_space_between_classes info1 info2 with
    | None       -> [ discretionary_if_breakable alw badns (adjacent_space ctx1 ctx2) ]
    | Some(lphb) -> [ discretionary_if_breakable alw badns lphb ]


let space_between_chunks_pure info1 info2 : lb_pure_box list =
  let (ctx1, script1, lbc1) = info1 in
  let (ctx2, script2, lbc2) = info2 in
  if not (script_equal script1 script2) then
    let size = Length.max ctx1.font_size ctx2.font_size in
      match pure_space_between_scripts ctx1 ctx2 size script1 lbc1 script2 lbc2 with
      | Some(lphb) ->
          [lphb]

      | None ->
          begin
            match pure_space_between_classes info1 info2 with
            | None       -> [adjacent_space ctx1 ctx2]
            | Some(lphb) -> [lphb]
          end
  else
  (* If scripts are the same: *)
    match pure_space_between_classes info1 info2 with
    | None       -> [adjacent_space ctx1 ctx2]
    | Some(lphb) -> [lphb]


(* Converts single CJK character, not depending on adjacent characters. *)
let ideographic_single ctx script lbc (uchseg : uchar_segment) : lb_box list =
  let lphb_raw =
    let hsinfo = get_string_info ctx script in
    LBPure(make_string_atom hsinfo [ uchseg ])
  in
  let hwkern = halfwidth_kern ctx script in
  let qwkern = quarterwidth_kern ctx script in
    match lbc with
    | JLCP    (* JLreq cl-02; fullwidth close punctuation *)
    | JLFS    (* JLreq cl-06; kuten (fullwidth full stops) *)
    | JLCM -> (* JLreq cl-07; touten (fullwidth commas) *)
        [ lphb_raw; hwkern ]

    | JLOP -> (* JLreq cl-01; fullwidth open punctuation *)
        [ hwkern; lphb_raw ]

    | JLMD -> (* JLreq cl-05; nakaten (fullwidth middle dot, fullwidth semicolon, etc.) *)
        [ qwkern; lphb_raw; qwkern ]

    | _ ->
        [ lphb_raw ]


type chunk_accumulator =
  | AccInitial
  | AccNone
  | AccSome    of (context_main * script * line_break_class) * break_opportunity


let chunks_to_boxes (lphbf : horz_box list -> lb_pure_box list) (script_before : script) (chunklst : line_break_chunk list) (script_after : script) : lb_box list =
  let rec aux lhbacc optprev chunklst =
    match chunklst with
    | [] ->
        begin
          match optprev with
          | AccInitial ->
              []
                (* temporary; it may be better to insert spaces
                   using 'script_before' and 'script_after',
                   but we do not have any input context for it *)

          | AccNone ->
              Alist.to_list lhbacc

          | AccSome(infoprev, alw) ->
              let (ctxprev, _, _) = infoprev in
              let info_after = (ctxprev, script_after, XX) in
              let autospace = space_between_chunks infoprev alw info_after in
              Alist.to_list (Alist.append lhbacc autospace)
        end

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lhblstmain) =
          match chunkmain with
          | Space ->
              (AccNone, [breakable_space lphbf ctx])

          | UnbreakableSpace ->
              (AccNone, [unbreakable_space ctx])

          | AlphabeticChunk(script, lbcfirst, lbclast, uchseglst, alwnext) ->
              let opt = AccSome(((ctx, script, lbclast), alwnext)) in
              let lhblststr = inner_string ctx script uchseglst in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks info_before PreventBreak (ctx, script, lbcfirst) in
                    (opt, List.append autospace lhblststr)

                | AccNone ->
                    (opt, lhblststr)

                | AccSome(infoprev, alw) ->
                    let autospace = space_between_chunks infoprev alw (ctx, script, lbcfirst) in
                    (opt, List.append autospace lhblststr)
              end

          | IdeographicChunk(script, lbc, uchseg, alwnext) ->
              let opt = AccSome((ctx, script, lbc), alwnext) in
              let lhblststr = ideographic_single ctx script lbc uchseg in
              begin
                match optprev with
                | AccNone ->
                    (opt, lhblststr)

                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks info_before PreventBreak (ctx, script, lbc) in
                    (opt, List.append autospace lhblststr)

                | AccSome((infoprev, alw)) ->
                    let autospace = space_between_chunks infoprev alw (ctx, script, lbc) in
                    (opt, List.append autospace lhblststr)
              end
        in
        aux (Alist.append lhbacc lhblstmain) opt chunktail
  in
  aux Alist.empty AccInitial chunklst


let chunks_to_boxes_pure (script_before : script) (chunklst : line_break_chunk list) (script_after : script) : lb_pure_box list =
  let rec aux lphbacc optprev chunklst =
    match chunklst with
    | [] ->
        begin
          match optprev with
          | AccInitial ->
              []

          | AccNone ->
              Alist.to_list lphbacc

          | AccSome(infoprev, _alw) ->
              let (ctx, _, _) = infoprev in
              let info_after = (ctx, script_after, XX) in
              let autospace = space_between_chunks_pure infoprev info_after in
              Alist.to_list (Alist.append lphbacc autospace)
        end

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lphblstmain) =
          match chunkmain with
          | Space ->
              (AccNone, [pure_space ctx])

          | UnbreakableSpace ->
              (AccNone, [pure_space ctx])

          | AlphabeticChunk(script, lbcfirst, lbclast, uchsegs, alw) ->
              let opt = AccSome(((ctx, script, lbclast), alw)) in
              let lphblstmain =
                let hsinfo = get_string_info ctx script in
                [ make_string_atom hsinfo uchsegs ]
              in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks_pure info_before (ctx, script, lbcfirst) in
                    (opt, List.append autospace lphblstmain)

                | AccNone ->
                    (opt, lphblstmain)

                | AccSome((infoprev, _alw)) ->
                    let autospace = space_between_chunks_pure infoprev (ctx, script, lbcfirst) in
                    (opt, List.append autospace lphblstmain)
              end

          | IdeographicChunk(script, lbc, uchseg, alw) ->
              let opt = AccSome(((ctx, script, lbc), alw)) in
              let lphblstmain =
                let hsinfo = get_string_info ctx script in
                [ make_string_atom hsinfo [ uchseg ] ]
              in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks_pure info_before (ctx, script, lbc) in
                    (opt, List.append autospace lphblstmain)

                | AccNone ->
                    (opt, lphblstmain)

                | AccSome((infoprev, _alw)) ->
                    let autospace = space_between_chunks_pure infoprev (ctx, script, lbc) in
                    (opt, List.append autospace lphblstmain)
              end
        in
          aux (Alist.append lphbacc lphblstmain) opt chunktail
  in
  aux Alist.empty AccInitial chunklst
