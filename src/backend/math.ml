
open HorzBox


type low_math_atom =
  | LowMathGlyph        of math_string_info * length * length * length * FontFormat.glyph_id
  | LowMathGraphics     of math_graphics
  | LowMathEmbeddedHorz of horz_box list

type left_kern =
  {
    kernTL             : FontFormat.math_kern option;
    kernBL             : FontFormat.math_kern option;
    left_math_kind     : math_kind;
    first_height       : length;
    first_depth        : length;
  }

type right_kern =
  {
    italics_correction : length;
    kernTR             : FontFormat.math_kern option;
    kernBR             : FontFormat.math_kern option;
    right_math_kind    : math_kind;
    last_height        : length;
    last_depth         : length;
  }

type low_math_pure = math_kind * length * length * length * low_math_atom * left_kern * right_kern

type low_math_main =
  | LowMathPure        of low_math_pure
  | LowMathSubscript   of length * low_math * low_math
      (* --
         (1) baseline depth of the subscript
         (2) base contents
         (3) subscript contents
         -- *)
  | LowMathSuperscript of length * low_math * low_math
      (* --
         (1) baseline height of the superscript
         (2) base contents
         (3) superscript contents
         -- *)
  | LowMathFraction    of length * length * low_math * low_math
      (* --
         (1) baseline height of the numerator
         (2) baseline depth of the denominator
         (3) numerator contents
         (4) denominator contents
         -- *)
  | LowMathRadical     of low_math
  | LowMathParen       of horz_box list * horz_box list * low_math

and low_math = low_math_main list * length * length * left_kern * right_kern
  (* -- lowmath has information about its height and depth -- *)


let no_left_kern mk =
  {
    kernTL         = None;
    kernBL         = None;
    left_math_kind = mk;
    first_height   = Length.zero;
    first_depth    = Length.zero;
  }


let no_right_kern mk =
  {
    italics_correction = Length.zero;
    kernTR             = None;
    kernBR             = None;
    right_math_kind    = mk;
    last_height        = Length.zero;
    last_depth         = Length.zero;
  }


let make_left_and_right_kern hgt dpt mk mic mkiopt : left_kern * right_kern =
  let lk =
    match mkiopt with
    | Some(mki) ->
        {
          kernTL         = Some(mki.FontFormat.kernTL);
          kernBL         = Some(mki.FontFormat.kernBL);
          left_math_kind = mk;
          first_height   = hgt;
          first_depth    = dpt;
        }

    | None -> no_left_kern mk
  in
  let rk =
    match mkiopt with
    | Some(mki) ->
        Format.printf "Math> rk Some\n";
        {
          italics_correction = mic;
          kernTR             = Some(mki.FontFormat.kernTR);
          kernBR             = Some(mki.FontFormat.kernBR);
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | None ->
        Format.printf "Math> rk None\n";
        {
          italics_correction = mic;
          kernTR             = None;
          kernBR             = None;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }
  in
    (lk, rk)


let normalize_math_kind mkprev mknext mkraw =
  match mkraw with
  | MathOrdinary
  | MathOpen
  | MathClose
  | MathOperator
  | MathInner
  | MathEnd
    -> mkraw

  | MathBinary ->
      begin
        match (mkprev, mknext) with
        | (MathOrdinary, MathOrdinary)
        | (MathInner   , MathOrdinary)
        | (MathOrdinary, MathInner   )

        | (MathClose   , MathOrdinary)
        | (MathClose   , MathInner   )

        | (MathOrdinary, MathOpen    )
        | (MathInner   , MathOpen    )

        | (MathClose   , MathOpen    )
          -> MathBinary

        | _ -> MathOrdinary
      end

  | MathRelation ->
      mkraw
(*
      begin
        match (mkprev, mknext) with
        | (MathEnd , _        )
        | (_       , MathEnd  )
        | (MathOpen, _        )
        | (_       , MathClose)
          -> MathOrdinary

        | _ -> mkraw
      end
*)

let space_ord_bin fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.25, Length.zero, Length.zero)))  (* temporary; should be variable *)


let space_ord_rel fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.375, Length.zero, Length.zero)))  (* temporary; should be variable *)


let space_ord_inner fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.125, Length.zero, Length.zero)))  (* temporary; should be variable *)


let space_between_math_atom (mathctx : math_context) (scriptlev : int) (mkprev : math_kind) (mk : math_kind) : horz_box option =
  let fontsize = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
    match (mkprev, mk) with
    | (MathOrdinary, MathInner   )
    | (MathInner   , MathInner   )
    | (MathInner   , MathOrdinary)
      -> space_ord_inner fontsize scriptlev

    | (MathBinary  , MathOrdinary)
    | (MathBinary  , MathInner   )
    | (MathOrdinary, MathBinary  )
    | (MathInner   , MathBinary  )
      -> space_ord_bin fontsize scriptlev

    | (MathRelation, MathOrdinary)
    | (MathRelation, MathInner   )
    | (MathOrdinary, MathRelation)
    | (MathInner   , MathRelation)
      -> space_ord_rel fontsize scriptlev

    | _ -> None


let convert_math_element (mkprev : math_kind) (mknext : math_kind) (scriptlev : int) ((mkraw, memain) : math_element) : low_math_pure =
  let mk = normalize_math_kind mkprev mknext mkraw in
  match memain with
  | MathGraphics(g) ->
      (mk, Length.zero (* temporary *), Length.zero (* temporary *), Length.zero (*temporary *), LowMathGraphics(g), no_left_kern MathEnd, no_right_kern MathEnd (* temporary *))

  | MathEmbeddedHorz(hblst) ->
      let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
        (mk, wid, hgt, dpt, LowMathEmbeddedHorz(hblst), no_left_kern MathEnd, no_right_kern MathEnd (* temporary *))

  | MathChar(mathctx, uch) ->
      let mathstrinfo = FontInfo.get_math_string_info scriptlev mathctx in
      let (gid, wid, hgt, dpt, mic, mkiopt) = FontInfo.get_math_char_info mathstrinfo scriptlev uch in
      let (lk, rk) = make_left_and_right_kern hgt dpt mk mic mkiopt in
        (mk, wid, hgt, dpt, LowMathGlyph(mathstrinfo, wid, hgt, dpt, gid), lk, rk)


let get_left_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, lk, _)          -> lk
  | LowMathSubscript(_, (_, _, _, lk, _), _)   -> lk
  | LowMathSuperscript(_, (_, _, _, lk, _), _) -> lk
  | LowMathFraction(_, _, _, _)                -> no_left_kern MathInner
  | LowMathRadical(_)                          -> no_left_kern MathInner
  | LowMathParen(_, _, _)                      -> no_left_kern MathOpen
      (* temporary; should extract kerning information from the left paren *)


let get_right_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, _, rk)          -> rk
  | LowMathSubscript(_, (_, _, _, _, rk), _)   -> no_right_kern rk.right_math_kind
  | LowMathSuperscript(_, (_, _, _, _, rk), _) -> no_right_kern rk.right_math_kind
  | LowMathFraction(_, _, _, _)                -> no_right_kern MathInner
  | LowMathRadical(_)                          -> no_right_kern MathInner
  | LowMathParen(_, _, _)                      -> no_right_kern MathClose
      (* temporary; should extract kerning information from the right paren *)


let get_left_math_kind mathopt =
  let rec aux = function
    | MathPure((mk, _))              -> mk
    | MathSuperscript([], _)         -> MathEnd
    | MathSuperscript(mathB :: _, _) -> aux mathB
    | MathSubscript([], _)           -> MathEnd
    | MathSubscript(mathB :: _, _)   -> aux mathB
    | MathFraction(_, _)             -> MathInner
    | MathRadical(_)                 -> MathInner
    | MathParen(_, _, _)             -> MathOpen
  in
  match mathopt with
  | None       -> MathEnd
  | Some(math) -> aux math


let get_right_math_kind mathopt =
  let rec aux = function
    | MathPure((mk, _))           -> mk
    | MathSuperscript([], _)      -> MathEnd
    | MathSuperscript(mathlst, _) -> aux (List.hd (List.rev mathlst))
    | MathSubscript([], _)        -> MathEnd
    | MathSubscript(mathlst, _)   -> aux (List.hd (List.rev mathlst))
    | MathFraction(_, _)          -> MathInner
    | MathRadical(_)              -> MathInner
    | MathParen(_, _, _)          -> MathClose
  in
  match mathopt with
  | None       -> MathEnd
  | Some(math) -> try aux math with Invalid_argument(_) -> assert false


let get_real_font_size mathctx scriptlev =
  (FontInfo.get_math_string_info scriptlev mathctx).math_font_size


let superscript_baseline_height (mathctx : math_context) (scriptlev : int) h_base d_sup =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_supbmin = fontsize *% mc.FontFormat.superscript_bottom_min in
  let h_supstd  = fontsize *% mc.FontFormat.superscript_shift_up in
  let l_supdmax = fontsize *% mc.FontFormat.superscript_baseline_drop_max in
  let cand = [h_supstd; h_base -% l_supdmax; h_supbmin +% d_sup] in
  let h_supbl = cand |> List.fold_left Length.max Length.zero in
    h_supbl


(* -- calculates the base correction height and the superscript correction height (in ratio) -- *)
let superscript_correction mathctx scriptlev h_supbl h_base d_sup =
  let l_base = h_supbl -% d_sup in
  let l_sup = h_base -% h_supbl in
  let s_base = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
  let s_sup  = (FontInfo.get_math_string_info (scriptlev + 1) mathctx).math_font_size in
  let r_base = l_base /% s_base in
  let r_sup  = l_sup /% s_sup in
    (r_base, r_sup)


let subscript_baseline_depth (mathctx : math_context) (scriptlev : int) d_base h_sub =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_subtmax = fontsize *% mc.FontFormat.subscript_top_max in
  let d_substd  = Length.negate (fontsize *% mc.FontFormat.subscript_shift_down) in
  let l_subdmin = fontsize *% mc.FontFormat.subscript_baseline_drop_min in
  let cand = [Length.negate d_substd; (Length.negate d_base) +% l_subdmin; h_sub -% h_subtmax] in
  let d_subbl = Length.negate (cand |> List.fold_left Length.max Length.zero) in
    d_subbl


let subscript_correction mathctx scriptlev d_subbl d_base h_sub =
  let l_base = d_base -% d_subbl in
  let l_sub = h_sub +% d_base in
  let s_base = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
  let s_sub  = (FontInfo.get_math_string_info (scriptlev + 1) mathctx).math_font_size in
  let r_base = l_base /% s_base in
  let r_sub  = l_sub /% s_sub in
    (r_base, r_sub)


let numerator_baseline_height mathctx scriptlev d_numer =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_numerstd    = fontsize *% mc.FontFormat.fraction_numer_d_shift_up in
  let l_numergapmin = fontsize *% mc.FontFormat.fraction_numer_d_gap_min in
  let h_numerbl = Length.max h_numerstd (h_bar +% t_bar *% 0.5 +% l_numergapmin +% (Length.negate d_numer)) in
    h_numerbl


let denominator_baseline_depth mathctx scriptlev h_denom =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let d_denomstd    = Length.negate (fontsize *% mc.FontFormat.fraction_denom_d_shift_down) in
  let l_denomgapmin = fontsize *% mc.FontFormat.fraction_denom_d_gap_min in
  let d_denombl = Length.min d_denomstd (h_bar -% t_bar *% 0.5 -% l_denomgapmin -% h_denom) in
    d_denombl


let make_paren mathctx scriptlev paren hgt dpt =
  let fontsize = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar = fontsize *% mc.FontFormat.axis_height in
  paren hgt dpt h_bar
  

let rec convert_to_low (mathctx : math_context) (scriptlev : int) (mlst : math list) : low_math =
  let optres =
    mlst |> Util.list_fold_adjacent (fun opt math mathprevopt mathnextopt ->
      let mkprev = get_left_math_kind mathprevopt in
      let mknext = get_right_math_kind mathnextopt in
      let (lmmain, hgt, dpt) = convert_to_low_single mkprev mknext mathctx scriptlev math in
      let rk = get_right_kern lmmain in
      let lk = get_left_kern lmmain in
      match opt with
      | None                                        -> Some((hgt, dpt, lk, rk, lmmain :: []))
      | Some((hgtprev, dptprev, lkfirst, _, lmacc)) -> Some((Length.max hgt hgtprev, Length.min dpt dptprev, lkfirst, rk, lmmain :: lmacc))
    ) None
  in
  match optres with
  | None                                         -> ([], Length.zero, Length.zero, no_left_kern MathEnd, no_right_kern MathEnd)
  | Some((hgt, dpt, lkfirst, rklast, lmmainacc)) -> (List.rev lmmainacc, hgt, dpt, lkfirst, rklast)


and convert_to_low_single (mkprev : math_kind) (mknext : math_kind) (mathctx : math_context) (scriptlev : int) (math : math) : low_math_main * length * length =
  match math with
  | MathPure(me) ->
      let (mk, wid, hgt, dpt, lme, lk, rk) = convert_math_element mkprev mknext scriptlev me in
        (LowMathPure(mk, wid, hgt, dpt, lme, lk, rk), hgt, dpt)

  | MathFraction(mlstN, mlstD) ->
      let lmN = convert_to_low mathctx scriptlev mlstN in
      let lmD = convert_to_low mathctx scriptlev mlstD in
      let (_, h_numer, d_numer, _, _) = lmN in
      let (_, h_denom, d_denom, _, _) = lmD in
      let h_numerbl = numerator_baseline_height mathctx scriptlev d_numer in
      let d_denombl = denominator_baseline_depth mathctx scriptlev h_denom in
      let h_frac = h_numerbl +% h_numer in
      let d_frac = d_denombl +% d_denom in
        (LowMathFraction(h_numerbl, d_denombl, lmN, lmD), h_frac, d_frac)

  | MathSubscript(mlst1, mlst2) ->
      let lmB = convert_to_low mathctx scriptlev mlst1 in
      let lmS = convert_to_low mathctx (scriptlev + 1) mlst2 in
      let (_, h_base, d_base, _, rkB) = lmB in
      let (_, h_sub, d_sub, _, _)     = lmS in
      let d_subbl = subscript_baseline_depth mathctx scriptlev rkB.last_depth h_sub in
      let h_whole = h_base in
      let d_whole = Length.min d_base (d_subbl +% d_sub) in
        (LowMathSubscript(d_subbl, lmB, lmS), h_whole, d_whole)

  | MathSuperscript(mlst1, mlst2) ->
      let lmB = convert_to_low mathctx scriptlev mlst1 in
      let lmS = convert_to_low mathctx (scriptlev + 1) mlst2 in
      let (_, h_base, d_base, _, rkB) = lmB in
      let (_, h_sup, d_sup, _, _)     = lmS in
      let h_supbl = superscript_baseline_height mathctx scriptlev h_base d_sup in
      let h_whole = Length.max h_base (h_supbl +% h_sup) in
      let d_whole = d_base in
        (LowMathSuperscript(h_supbl, lmB, lmS), h_whole, d_whole)

  | MathRadical(mlst1) ->
(*
      let lm1 = convert_to_low mathctx scriptlev mlst1 in
        LowMathRadical(lm1)
*)
      failwith "unsupported; MathRadical"  (* temporary *)

  | MathParen(parenL, parenR, mlst1) ->
      let lm1 = convert_to_low mathctx scriptlev mlst1 in
      let (_, h_enc, d_enc, _, _) = lm1 in
      let (hblstparenL, kerninfo_left)    = make_paren mathctx scriptlev parenL h_enc d_enc in
      let (hblstparenR, kerninfo_right) = make_paren mathctx scriptlev parenR h_enc d_enc in
      let (_, h_left, d_left)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, h_right, d_right) = LineBreak.get_natural_metrics hblstparenR in
      let h_whole = [h_left; h_right] |> List.fold_left Length.max h_enc in
      let d_whole = [d_left; d_right] |> List.fold_left Length.min d_enc in
        (LowMathParen(hblstparenL, hblstparenR, lm1), h_whole, d_whole)


let horz_of_low_math_element (lme : low_math_atom) : horz_box list =
  match lme with
  | LowMathGlyph(mathstrinfo, wid, hgt, dpt, gid) ->
      [HorzPure(PHFixedMathGlyph(mathstrinfo, wid, hgt, dpt, gid))]

  | LowMathGraphics(g) ->
      []  (* temporary *)

  | LowMathEmbeddedHorz(hblst) ->
      hblst


let ratioize n =
  (float_of_int n) /. 1000.


let horz_fraction_bar mathctx scriptlev wid =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_bart        = h_bar +% t_bar *% 0.5 in
  let bar_color = DeviceGray(0.) in  (* temporary; should be variable *)
  let bar_graphics (xpos, ypos) =
    Graphics.pdfops_of_fill bar_color [Rectangle((xpos, ypos +% h_bart), (wid, t_bar))]
  in
    HorzPure(PHInlineGraphics(wid, h_bart, Length.zero, bar_graphics))  


let kern_top_right mathctx scriptlev rk ratio =
  let fontsize = get_real_font_size mathctx scriptlev in
  match rk.kernTR with
  | None        -> Length.zero
  | Some(mkern) -> let r = FontFormat.find_kern_ratio mkern ratio in Length.min Length.zero (fontsize *% r)


let kern_bottom_right mathctx scriptlev rk ratio =
  let fontsize = get_real_font_size mathctx scriptlev in
  match rk.kernBR with
  | None        -> Length.zero
  | Some(mkern) -> let r = FontFormat.find_kern_ratio mkern ratio in Length.min Length.zero (fontsize *% r)


let kern_top_left mathctx scriptlev lk ratio =
  let fontsize = get_real_font_size mathctx scriptlev in
  match lk.kernTL with
  | None        -> Length.zero
  | Some(mkern) -> let r = FontFormat.find_kern_ratio mkern ratio in Length.min Length.zero (fontsize *% r)


let kern_bottom_left mathctx scriptlev rk ratio =
  let fontsize = get_real_font_size mathctx scriptlev in
  match rk.kernBL with
  | None        -> Length.zero
  | Some(mkern) -> let r = FontFormat.find_kern_ratio mkern ratio in Length.min Length.zero (fontsize *% r)


let raise_horz r hblst =
  [HorzPure(PHRising(r, hblst))]


let rec horz_of_low_math (mathctx : math_context) (scriptlev : int) (lm : low_math) =
  let (lmmainlst, _, _, _, _) = lm in
  let rec aux hbacc mkprev lmmainlst =
    match lmmainlst with
    | [] -> List.rev hbacc

    | LowMathPure(mk, wid, hgt, dpt, lma, _, _) :: lmmaintail ->
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev mk in
        let hblstpure = horz_of_low_math_element lma in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstpure hbacc
          | Some(hbspace) -> List.rev_append hblstpure (hbspace :: hbacc)
        in
          aux hbaccnew mk lmmaintail

    | LowMathSuperscript(h_supbl, lmB, lmS) :: lmmaintail ->
        let (_, h_base, d_base, lkB, rkB) = lmB in
        let (_, d_base, d_sup, lkS, _) = lmS in
        let hblstB = horz_of_low_math mathctx scriptlev lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) lmS in

        let (r_base, r_sup) = superscript_correction mathctx scriptlev h_supbl rkB.last_height d_sup in
        let l_kernbase = kern_top_right mathctx scriptlev rkB r_base in
        let l_kernsup  = kern_bottom_left mathctx (scriptlev + 1) lkS r_sup in
(*
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsup = %f, " r_kernsup;
*)
        let l_italic   = rkB.italics_correction in
        Format.printf "l_italic = %f\n" (Length.to_pdf_point l_italic);
        let kern = l_italic +% l_kernbase +% l_kernsup in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsup = List.concat [hblstB; [hbkern]; raise_horz h_supbl hblstS] in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev lkB.left_math_kind in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsup hbacc
          | Some(hbspace) -> List.rev_append hblstsup (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind lmmaintail

    | LowMathSubscript(d_subbl, lmB, lmS) :: lmmaintail ->
        let (_, h_base, d_base, lkB, rkB) = lmB in
        let (_, h_sub, d_sub, lkS, _) = lmS in
        let hblstB = horz_of_low_math mathctx scriptlev lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) lmS in
        let d_base = rkB.last_depth in
        let (_, h_sub, _) = LineBreak.get_natural_metrics hblstS in
        let d_subbl = subscript_baseline_depth mathctx scriptlev d_base h_sub in
        let (r_base, r_sub) = subscript_correction mathctx scriptlev d_subbl d_base h_sub in
        let l_kernbase = kern_bottom_right mathctx scriptlev rkB r_base in
        let l_kernsub  = kern_top_left mathctx scriptlev lkS r_sub in
(*
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsub = %f\n" r_kernsub;
*)
        let kern = l_kernbase +% l_kernsub in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsub = List.concat [hblstB; [hbkern]; raise_horz d_subbl hblstS] in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev lkB.left_math_kind in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind lmmaintail

    | LowMathFraction(h_numerbl, d_denombl, lmN, lmD) :: lmmaintail ->
        let hblstN = horz_of_low_math mathctx scriptlev lmN in
        let hblstD = horz_of_low_math mathctx scriptlev lmD in
        let (w_numer, _, _) = LineBreak.get_natural_metrics hblstN in
        let (w_denom, _, _) = LineBreak.get_natural_metrics hblstD in
        let (hblstNret, hblstDret, w_frac) =
          if w_numer <% w_denom then
          (* -- if the numerator is narrower than the denominator -- *)
            let space = (w_denom -% w_numer) *% 0.5 in
            let hblst_space = HorzPure(PHFixedEmpty(space)) in
            let hblstNnew = List.concat [[hblst_space]; hblstN; [hblst_space]] in
              (hblstNnew, hblstD, w_denom)
          else
            let space = (w_numer -% w_denom) *% 0.5 in
            let hblst_space = HorzPure(PHFixedEmpty(space)) in
            let hblstDnew = List.concat [[hblst_space]; hblstD; [hblst_space]] in
              (hblstN, hblstDnew, w_numer)
        in
        let back = HorzPure(PHFixedEmpty(Length.negate w_frac)) in
        let bar = horz_fraction_bar mathctx scriptlev w_frac in
        let hblstsub = List.concat [raise_horz h_numerbl hblstNret; [back; bar; back]; raise_horz d_denombl hblstDret] in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev MathInner in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew MathOrdinary lmmaintail

    | LowMathRadical(lm1) :: lmmaintail ->
        failwith "unsupported"  (* temporary *)

    | LowMathParen(parenL, parenR, lmE) :: lmmaintail ->
(*
        let hblstE = horz_of_low_math mathctx scriptlev lmE in
        let (wE, hE, dE) = LineBreak.get_natural_metrics hblstE in
        let (kerninfoL, hblstL) = make_paren mathctx scriptlev parenL wE hE dE in
        let (kerninfoR, hblstR) = make_paren mathctx scriptlev parenR wE hE dE in
*)
        failwith "unsupported"  (* temporary *)

  in
  aux [] MathEnd lmmainlst


(*
(* for tests *)
let () =
  let mathinfo = { math_font_abbrev = "euler"; math_font_size = Length.of_pdf_point 12.; } in
  let md = FontFormat.get_math_decoder "/usr/local/lib-satysfi/dist/fonts/euler.otf" in
  let mlst = [MathSuperscript([MathPure(MathOrdinary, MathChar(mathinfo, Uchar.of_char 'P'))], [MathPure(MathOrdinary, MathChar(mathinfo, Uchar.of_char 'A'))])] in
  let lm = convert_to_low 0 mlst in
  let hblst = horz_of_low_math 0 mathinfo md lm in
  List.iter (fun hb -> Format.printf "%a@ " pp_horz_box hb) hblst;
  print_endline "";
*)
