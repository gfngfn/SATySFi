
open HorzBox


type low_math_atom =
  | LowMathGlyph        of math_string_info * length * length * length * FontFormat.glyph_id
  | LowMathGraphics     of math_graphics
  | LowMathEmbeddedHorz of horz_box list

type left_kern =
  {
    kernTL             : FontInfo.math_kern_scheme;
    kernBL             : FontInfo.math_kern_scheme;
    left_math_kind     : math_kind;
    first_height       : length;
    first_depth        : length;
  }

type right_kern =
  {
    italics_correction : length;
    kernTR             : FontInfo.math_kern_scheme;
    kernBR             : FontInfo.math_kern_scheme;
    right_math_kind    : math_kind;
    last_height        : length;
    last_depth         : length;
  }

type low_math_pure = math_kind * length * length * length * low_math_atom * left_kern * right_kern

type low_paren = horz_box list * length * length * FontInfo.math_kern_scheme

type low_radical = horz_box list

type low_math_main =
  | LowMathPure of low_math_pure

  | LowMathSubscript of length * low_math * low_math
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
  | LowMathFraction of length * length * low_math * low_math
      (* --
         (1) baseline height of the numerator
         (2) baseline depth of the denominator
         (3) numerator contents
         (4) denominator contents
         -- *)
  | LowMathRadical of low_radical * length * length * low_math
      (* --
         (1) height of the bar
         (2) thickness of the bar
         (3) inner contents
         -- *)
  | LowMathRadicalWithDegree of length * length * length * low_math * low_math
      (* --
         (1) height of the bar
         (2) thickness of the bar
         (3) height of the bottom of the degree
         (4) degree contents
         (5) inner contents
         -- *)
  | LowMathParen       of low_paren * low_paren * low_math

and low_math = low_math_main list * length * length * left_kern * right_kern
  (* -- lowmath has information about its height and depth -- *)


let no_left_kern mk =
  {
    kernTL         = FontInfo.no_math_kern;
    kernBL         = FontInfo.no_math_kern;
    left_math_kind = mk;
    first_height   = Length.zero;
    first_depth    = Length.zero;
  }


let no_right_kern mk =
  {
    italics_correction = Length.zero;
    kernTR             = FontInfo.no_math_kern;
    kernBR             = FontInfo.no_math_kern;
    right_math_kind    = mk;
    last_height        = Length.zero;
    last_depth         = Length.zero;
  }


let make_left_and_right_kern hgt dpt mk mic mkiopt : left_kern * right_kern =
  let lk =
    match mkiopt with
    | Some(mki) ->
        {
          kernTL         = FontInfo.make_discrete_math_kern mki.FontFormat.kernTL;
          kernBL         = FontInfo.make_discrete_math_kern mki.FontFormat.kernBL;
          left_math_kind = mk;
          first_height   = hgt;
          first_depth    = dpt;
        }

    | None -> no_left_kern mk
  in
  let rk =
    match mkiopt with
    | Some(mki) ->
        Format.printf "Math> rk = Some(...)\n";
        {
          italics_correction = mic;
          kernTR             = FontInfo.make_discrete_math_kern mki.FontFormat.kernTR;
          kernBR             = FontInfo.make_discrete_math_kern mki.FontFormat.kernBR;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | None ->
        {
          italics_correction = mic;
          kernTR             = FontInfo.no_math_kern;
          kernBR             = FontInfo.no_math_kern;
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

        | _ ->
            Format.printf "Math> normalize (%a, %a)\n" pp_math_kind mkprev pp_math_kind mknext;
            MathOrdinary
      end

  | MathRelation ->
      mkraw


let space_ord_bin fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.25, Length.zero, Length.zero)))  (* temporary; should be variable *)


let space_ord_rel fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.375, Length.zero, Length.zero)))  (* temporary; should be variable *)


let space_ord_inner fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.125, Length.zero, Length.zero)))  (* temporary; should be variable *)


let get_real_font_size mathctx scriptlev =
  (FontInfo.get_math_string_info mathctx scriptlev).math_font_size


let space_between_math_atom (mathctx : math_context) (scriptlev : int) (mkprev : math_kind) (italcorropt : length option) (mk : math_kind) : horz_box option =
  let fontsize = get_real_font_size mathctx scriptlev in
    match (mkprev, mk) with
    | (MathOrdinary, MathInner   )
    | (MathInner   , MathInner   )
    | (MathInner   , MathOrdinary)
      -> space_ord_inner fontsize scriptlev

    | (MathOrdinary, MathClose   )
      ->
        begin
          match italcorropt with
          | None           -> None
          | Some(italcorr) -> Some(HorzPure(PHFixedEmpty(italcorr)))
        end

    | (MathBinary  , MathOrdinary)
    | (MathBinary  , MathInner   )
    | (MathBinary  , MathOpen    )
    | (MathOrdinary, MathBinary  )
    | (MathInner   , MathBinary  )
    | (MathClose   , MathBinary  )
      -> space_ord_bin fontsize scriptlev

    | (MathRelation, MathOrdinary)
    | (MathRelation, MathInner   )
    | (MathRelation, MathOpen    )
    | (MathOrdinary, MathRelation)
    | (MathInner   , MathRelation)
    | (MathClose   , MathRelation)
      -> space_ord_rel fontsize scriptlev

    | _ -> None


let convert_math_element (mkprev : math_kind) (mknext : math_kind) (scriptlev : int) ((mkraw, memain) : math_element) : low_math_pure =
  let mk = normalize_math_kind mkprev mknext mkraw in
  match memain with
  | MathGraphics(g) ->
      (mk, Length.zero (* temporary *), Length.zero (* temporary *), Length.zero (*temporary *), LowMathGraphics(g), no_left_kern MathEnd, no_right_kern MathEnd (* temporary *))

  | MathEmbeddedHorz(hblst) ->
      let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
        (mk, wid, hgt, dpt, LowMathEmbeddedHorz(hblst), no_left_kern mk, no_right_kern mk)

  | MathChar(mathctx, uch) ->
      let mathstrinfo = FontInfo.get_math_string_info mathctx scriptlev in
      let (gid, wid, hgt, dpt, mic, mkiopt) = FontInfo.get_math_char_info mathstrinfo scriptlev uch in
      let (lk, rk) = make_left_and_right_kern hgt dpt mk mic mkiopt in
        (mk, wid, hgt, dpt, LowMathGlyph(mathstrinfo, wid, hgt, dpt, gid), lk, rk)


let make_left_paren_kern hL dL mkernsL =
  {
    kernTL         = mkernsL;
    kernBL         = mkernsL;
    left_math_kind = MathOpen;
    first_height   = hL;
    first_depth    = dL;
  }


let make_right_paren_kern hR dR mkernsR =
  {
    italics_correction = Length.zero;
    kernTR             = mkernsR;
    kernBR             = mkernsR;
    right_math_kind    = MathClose;
    last_height        = hR;
    last_depth         = dR;
  }

let get_left_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, lk, _)          -> lk
  | LowMathSubscript(_, (_, _, _, lk, _), _)   -> lk
  | LowMathSuperscript(_, (_, _, _, lk, _), _) -> lk
  | LowMathFraction(_, _, _, _)                -> no_left_kern MathInner
  | LowMathRadical(_)                          -> no_left_kern MathInner
  | LowMathRadicalWithDegree(_, _, _, _, _)    -> no_left_kern MathInner
  | LowMathParen((_, hL, dL, mkernsL), _, _)   -> make_left_paren_kern hL dL mkernsL


let get_right_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, _, rk)          -> rk
  | LowMathSubscript(_, (_, _, _, _, rk), _)   -> no_right_kern rk.right_math_kind
  | LowMathSuperscript(_, (_, _, _, _, rk), _) -> no_right_kern rk.right_math_kind
  | LowMathFraction(_, _, _, _)                -> no_right_kern MathInner
  | LowMathRadical(_)                          -> no_right_kern MathInner
  | LowMathRadicalWithDegree(_, _, _, _, _)    -> no_right_kern MathInner
  | LowMathParen(_, (_, hR, dR, mkernsR), _)   -> make_right_paren_kern hR dR mkernsR


let rec get_left_math_kind = function
  | MathPure((mk, _))              -> mk
  | MathSuperscript([], _)         -> MathEnd
  | MathSuperscript(mathB :: _, _) -> get_left_math_kind mathB
  | MathSubscript([], _)           -> MathEnd
  | MathSubscript(mathB :: _, _)   -> get_left_math_kind mathB
  | MathFraction(_, _)             -> MathInner
  | MathRadical(_)                 -> MathInner
  | MathRadicalWithDegree(_, _)    -> MathInner
  | MathParen(_, _, _)             -> MathOpen


let rec get_right_math_kind math =
  try
    match math with
    | MathPure((mk, _))           -> mk
    | MathSuperscript([], _)      -> MathEnd
    | MathSuperscript(mathlst, _) -> get_right_math_kind (List.hd (List.rev mathlst))
    | MathSubscript([], _)        -> MathEnd
    | MathSubscript(mathlst, _)   -> get_right_math_kind (List.hd (List.rev mathlst))
    | MathFraction(_, _)          -> MathInner
    | MathRadical(_)              -> MathInner
    | MathRadicalWithDegree(_, _) -> MathInner
    | MathParen(_, _, _)          -> MathClose
  with
  | Invalid_argument(_) -> assert false


let superscript_baseline_height (mathctx : math_context) (scriptlev : int) h_base d_sup =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_supbmin = fontsize *% mc.FontFormat.superscript_bottom_min in
  let h_supstd  = fontsize *% mc.FontFormat.superscript_shift_up in
  let l_supdmax = fontsize *% mc.FontFormat.superscript_baseline_drop_max in
  let cand = [h_supstd; h_base -% l_supdmax; h_supbmin +% d_sup] in
  let h_supbl = cand |> List.fold_left Length.max Length.zero in
    h_supbl


(* -- calculates the base correction height and the superscript correction height -- *)
let superscript_correction_heights mathctx scriptlev h_supbl h_base d_sup =
  let l_base = h_supbl -% d_sup in
  let l_sup = h_base -% h_supbl in
    (l_base, l_sup)


let subscript_baseline_depth (mathctx : math_context) (scriptlev : int) d_base h_sub =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let h_subtmax = fontsize *% mc.FontFormat.subscript_top_max in
  let d_substd  = Length.negate (fontsize *% mc.FontFormat.subscript_shift_down) in
  let l_subdmin = fontsize *% mc.FontFormat.subscript_baseline_drop_min in
  let cand = [Length.negate d_substd; (Length.negate d_base) +% l_subdmin; h_sub -% h_subtmax] in
  let d_subbl = Length.negate (cand |> List.fold_left Length.max Length.zero) in
    d_subbl


let subscript_correction_heights mathctx scriptlev d_subbl d_base h_sub =
  let l_base = h_sub +% d_base in
  let l_sub = d_base -% d_subbl in
    (l_base, l_sub)


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


(* --
   radical_bar_metrics:
     takes a math context, a script level, and the height of the contents,
     and then returns the height, the thickness, and the extra ascender of the raducal rule.
   -- *)
let radical_bar_metrics mathctx scriptlev h_cont =
  let fontsize = get_real_font_size mathctx scriptlev in
  let mc = FontInfo.get_math_constants mathctx in
  let l_radgap = fontsize *% mc.FontFormat.radical_d_vertical_gap in
  let t_bar    = fontsize *% mc.FontFormat.radical_rule_thickness in
  let l_extra  = fontsize *% mc.FontFormat.radical_extra_ascender in
  let h_bar = h_cont +% l_radgap in
    (h_bar, t_bar, l_extra)


(* --
   radical_degree_baseline_height:
     takes a math context, a script level, the height of the radical sign, and the depth of the degree,
     and then returns the height of the degree baseline.
   -- *)
let radical_degree_baseline_height mathctx scriptlev h_rad d_deg =
  let mc = FontInfo.get_math_constants mathctx in
  let h_degb = h_rad *% mc.FontFormat.radical_degree_bottom in
  let h_degbl = h_degb +% d_deg in
    h_degbl
 

let make_paren mathctx scriptlev paren hgt dpt =
  let fontsize = (FontInfo.get_math_string_info mathctx scriptlev).math_font_size in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar = fontsize *% mc.FontFormat.axis_height in
  let (hblst, kernf) = paren hgt (Length.negate dpt) h_bar fontsize in
    (hblst, FontInfo.make_dense_math_kern kernf)


let make_radical mathctx scriptlev radical hgt_bar t_bar dpt =
  let fontsize = get_real_font_size mathctx scriptlev in
  let hblst = radical hgt_bar t_bar (Length.negate dpt) fontsize in
    hblst


let rec convert_to_low (mathctx : math_context) (scriptlev : int) (mkfirst : math_kind) (mklast : math_kind) (mlst : math list) : low_math =
  let optres =
    mlst |> Util.list_fold_adjacent (fun opt math mathprevopt mathnextopt ->
      let mkprev = match mathprevopt with None -> mkfirst | Some(mathprev) -> get_right_math_kind mathprev in
      let mknext = match mathnextopt with None -> mklast  | Some(mathnext) -> get_left_math_kind mathnext in
        (* -- get the rightward math class of the previous, and the leftward math class of the next -- *)
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
      let lmN = convert_to_low mathctx scriptlev MathEnd MathEnd mlstN in
      let lmD = convert_to_low mathctx scriptlev MathEnd MathEnd mlstD in
      let (_, h_numer, d_numer, _, _) = lmN in
      let (_, h_denom, d_denom, _, _) = lmD in
      let h_numerbl = numerator_baseline_height mathctx scriptlev d_numer in
      let d_denombl = denominator_baseline_depth mathctx scriptlev h_denom in
      let h_frac = h_numerbl +% h_numer in
      let d_frac = d_denombl +% d_denom in
        (LowMathFraction(h_numerbl, d_denombl, lmN, lmD), h_frac, d_frac)

  | MathSubscript(mlstB, mlstS) ->
      let lmB = convert_to_low mathctx scriptlev MathEnd MathEnd mlstB in
      let lmS = convert_to_low mathctx (scriptlev + 1) MathEnd MathEnd mlstS in
      let (_, h_base, d_base, _, rkB) = lmB in
      let (_, h_sub, d_sub, _, _)     = lmS in
      let d_subbl = subscript_baseline_depth mathctx scriptlev rkB.last_depth h_sub in
      let h_whole = h_base in
      let d_whole = Length.min d_base (d_subbl +% d_sub) in
        (LowMathSubscript(d_subbl, lmB, lmS), h_whole, d_whole)

  | MathSuperscript(mlstB, mlstS) ->
      let lmB = convert_to_low mathctx scriptlev MathEnd MathEnd mlstB in
      let lmS = convert_to_low mathctx (scriptlev + 1) MathEnd MathEnd mlstS in
      let (_, h_base, d_base, _, rkB) = lmB in
      let (_, h_sup, d_sup, _, _)     = lmS in
      let h_supbl = superscript_baseline_height mathctx scriptlev h_base d_sup in
      let h_whole = Length.max h_base (h_supbl +% h_sup) in
      let d_whole = d_base in
        (LowMathSuperscript(h_supbl, lmB, lmS), h_whole, d_whole)

  | MathRadical(radical, mlstC) ->
      let lmC = convert_to_low mathctx scriptlev MathEnd MathEnd mlstC in
      let (_, h_cont, d_cont, _, _) = lmC in
      let (h_bar, t_bar, l_extra) = radical_bar_metrics mathctx scriptlev h_cont in
      let hblstrad = make_radical mathctx scriptlev radical h_bar t_bar d_cont in
      let h_rad = h_bar +% t_bar in
      let h_whole = h_rad +% l_extra in
      let d_whole = d_cont in  (* temporary; should consider the depth of the radical sign *)
        (LowMathRadical(hblstrad, h_bar, t_bar, lmC), h_whole, d_whole)

  | MathRadicalWithDegree(mlstD, mlstC) ->
      let lmD = convert_to_low mathctx (scriptlev + 1) MathEnd MathEnd mlstD in
      let lmC = convert_to_low mathctx scriptlev MathEnd MathEnd mlstC in
      let (_, h_cont, d_cont, _, _) = lmC in
      let (h_bar, t_bar, l_extra) = radical_bar_metrics mathctx scriptlev h_cont in
      let h_rad = h_bar +% t_bar in
      let (_, h_deg, d_deg, _, _) = lmD in
      let h_degbl = radical_degree_baseline_height mathctx scriptlev h_rad d_deg in
      let h_whole = Length.max (h_rad +% l_extra) (h_degbl +% h_deg) in
      let d_whole = d_cont in  (* temporary; should consider the depth of the radical sign *)
        (LowMathRadicalWithDegree(h_bar, t_bar, h_degbl, lmD, lmC), h_whole, d_whole)

  | MathParen(parenL, parenR, mlstC) ->
      let lmC = convert_to_low mathctx scriptlev MathOpen MathClose mlstC in
      let (_, hC, dC, _, _) = lmC in
      let (hblstparenL, mkernsL) = make_paren mathctx scriptlev parenL hC dC in
      let (hblstparenR, mkernsR) = make_paren mathctx scriptlev parenR hC dC in
      let (_, hL, dL)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, hR, dR) = LineBreak.get_natural_metrics hblstparenR in
      let h_whole = [hL; hR] |> List.fold_left Length.max hC in
      let d_whole = [dL; dR] |> List.fold_left Length.min dC in
      let lpL = (hblstparenL, hL, dL, mkernsL) in
      let lpR = (hblstparenR, hR, dR, mkernsR) in
        (LowMathParen(lpL, lpR, lmC), h_whole, d_whole)


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


let calculate_kern mathctx scriptlev (mkernsch : FontInfo.math_kern_scheme) (corrhgt : length) =
  FontInfo.get_math_kern mathctx scriptlev mkernsch corrhgt


let raise_horz r hblst =
  [HorzPure(PHRising(r, hblst))]


let rec horz_of_low_math (mathctx : math_context) (scriptlev : int) (mkprevfirst : math_kind) (mklast : math_kind) (lm : low_math) =
  let (lmmainlst, _, _, _, _) = lm in
  let rec aux hbacc mkprev italcorropt lmmainlst =
    match lmmainlst with
    | [] ->
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt mklast in
        begin
          match hbspaceopt with
          | None          -> List.rev hbacc
          | Some(hbspace) -> List.rev (hbspace :: hbacc)
              (* --
                 appends italics correction for the last glyph of inner contents of a parenthesis;
                 -- *)
        end

    | LowMathPure(mk, wid, hgt, dpt, lma, _, rk) :: lmmaintail ->
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt mk in
        let hblstpure = horz_of_low_math_element lma in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstpure hbacc
          | Some(hbspace) -> List.rev_append hblstpure (hbspace :: hbacc)
        in
          aux hbaccnew mk (Some(rk.italics_correction)) lmmaintail

    | LowMathSuperscript(h_supbl, lmB, lmS) :: lmmaintail ->
        let (_, _, _, lkB, rkB) = lmB in
        let (_, _, d_sup, lkS, _) = lmS in
        let hblstB = horz_of_low_math mathctx scriptlev MathEnd MathEnd lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) MathEnd MathEnd lmS in
        let h_base = rkB.last_height in
        let (l_base, l_sup) = superscript_correction_heights mathctx scriptlev h_supbl h_base d_sup in
        let l_kernbase = calculate_kern mathctx scriptlev rkB.kernTR l_base in
        let l_kernsup  = calculate_kern mathctx (scriptlev + 1) lkS.kernBL l_sup in
(*
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsup = %f, " r_kernsup;
*)
        let l_italic   = rkB.italics_correction in
        Format.printf "Math> l_italic = %f\n" (Length.to_pdf_point l_italic);
        let kern = l_italic +% l_kernbase +% l_kernsup in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsup = List.concat [hblstB; [hbkern]; raise_horz h_supbl hblstS] in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt lkB.left_math_kind in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsup hbacc
          | Some(hbspace) -> List.rev_append hblstsup (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind None lmmaintail

    | LowMathSubscript(d_subbl, lmB, lmS) :: lmmaintail ->
        let (_, _, _, lkB, rkB) = lmB in
        let (_, h_sub, _, lkS, _) = lmS in
        let hblstB = horz_of_low_math mathctx scriptlev MathEnd MathEnd lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) MathEnd MathEnd lmS in
        let d_base = rkB.last_depth in
        let d_subbl = subscript_baseline_depth mathctx scriptlev d_base h_sub in
        let (l_base, l_sub) = subscript_correction_heights mathctx scriptlev d_subbl d_base h_sub in
        let l_kernbase = calculate_kern mathctx scriptlev rkB.kernBR l_base in
        let l_kernsub  = calculate_kern mathctx scriptlev lkS.kernTL l_sub in
(*
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsub = %f\n" r_kernsub;
*)
        let kern = l_kernbase +% l_kernsub in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let (w_sub, _, _) = LineBreak.get_natural_metrics hblstS in
        let hblstsub =
          let lensub = kern +% w_sub in
          if lensub <% Length.zero then
          (* -- if the leftward shift by the kern is larger than the width of the subscript -- *)
            let hbsupplement = HorzPure(PHFixedEmpty(Length.negate lensub)) in
            List.concat [hblstB; [hbkern]; raise_horz d_subbl hblstS; [hbsupplement]]
          else
            List.concat [hblstB; [hbkern]; raise_horz d_subbl hblstS]
        in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt lkB.left_math_kind in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind None lmmaintail

    | LowMathFraction(h_numerbl, d_denombl, lmN, lmD) :: lmmaintail ->
        let hblstN = horz_of_low_math mathctx scriptlev MathEnd MathEnd lmN in
        let hblstD = horz_of_low_math mathctx scriptlev MathEnd MathEnd lmD in
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
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt MathInner in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew MathOrdinary None lmmaintail

    | LowMathRadical(hblstrad, h_bar, t_bar, lmC) :: lmmaintail ->
        let hblstC = horz_of_low_math mathctx scriptlev MathEnd MathEnd lmC in
        let (w_cont, _, _) = LineBreak.get_natural_metrics hblstC in
        let hbbar =
          HorzPure(PHInlineGraphics(w_cont, h_bar +% t_bar, Length.zero,
            (fun (xpos, ypos) ->
              Graphics.pdfops_of_fill (DeviceGray(0.)) [Rectangle((xpos, ypos +% h_bar), (w_cont, t_bar))])))
        in
        let back = HorzPure(PHFixedEmpty(Length.negate w_cont)) in
        let hblstsub = List.append hblstrad (hbbar :: back :: hblstC) in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt MathInner in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew MathInner None lmmaintail

    | LowMathRadicalWithDegree(h_bar, t_bar, h_degbl, lmD, lmC) :: lmmaintail ->
        failwith "unsupported; LowMathRadicalWithDegree"  (* temporary *)

    | LowMathParen(lpL, lpR, lmE) :: lmmaintail ->
        let (hblstparenL, _, _, mkernsL) = lpL in
        let (hblstparenR, _, _, mkernsR) = lpR in
        let hblstE = horz_of_low_math mathctx scriptlev MathOpen MathClose lmE in
        let hblstsub = List.concat [hblstparenL; hblstE; hblstparenR] in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev italcorropt MathOpen in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew MathClose None lmmaintail

  in
  aux [] mkprevfirst None lmmainlst


let main mathctx mathlst =
  let lmlst = convert_to_low mathctx 0 MathEnd MathEnd mathlst in
  let hblst = horz_of_low_math mathctx 0 MathEnd MathEnd lmlst in
    hblst


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
