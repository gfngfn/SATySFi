
open MyUtil
open LengthInterface
open GraphicBase
open HorzBox
open Types


type low_math_atom =
  | LowMathGlyph        of math_string_info * length * length * length * OutputText.t
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

type low_paren =
  {
    lp_main             : horz_box list;
    lp_height           : length;
    lp_depth            : length;
    lp_math_kern_scheme : FontInfo.math_kern_scheme;
  }

type low_radical = horz_box list

type low_math_main =
  | LowMathPure of low_math_pure

  | LowMathList of math_context_change option * low_math
      (* --
         (1) information for updating math contexts
         (2) inner contents
         -- *)

  | LowMathGroup of math_kind * math_kind * low_math
      (* --
         (1) math class for leftward contents
         (2) math class for rightward contents
         (3) grouped contents
         -- *)

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

  | LowMathSubSuperscript of length * length * low_math * low_math * low_math
      (* --
         (1) baseline height of the superscript
         (2) baseline height of the subscript
         (3) base contents
         (4) superscript contents
         (5) subscript contents
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

  | LowMathParen of low_paren * low_paren * low_math
      (* --
         (1) graphical specification of the left parenthesis
         (2) graphical specification of the right parenthesis
         (3) inner contents
         -- *)

  | LowMathParenWithMiddle of low_paren * low_paren * low_paren * (low_math list)
      (* --
         (1) graphical specification of the left parenthesis
         (2) graphical specification of the right parenthesis
         (3) graphical specification of the middle sign
         (4) list of inner contents
         -- *)

  | LowMathUpperLimit of length * low_math * low_math
      (* --
         (1) baseline height of the upper script
         (2) base contents
         (3) upper script contents
         -- *)

  | LowMathLowerLimit of length * low_math * low_math
      (* --
         (1) baseline depth of the lower script
         (2) base contents
         (3) lower script contents
         -- *)

and low_math = low_math_main list * length * length * left_kern * right_kern
  (* -- lowmath has information about its height and depth -- *)

type space_correction =
  | NoSpace
  | ItalicsCorrection of length
  | SpaceAfterScript


let no_left_kern hgt dpt mk =
  {
    kernTL         = FontInfo.no_math_kern;
    kernBL         = FontInfo.no_math_kern;
    left_math_kind = mk;
    first_height   = hgt;
    first_depth    = dpt;
  }


let no_right_kern hgt dpt mk =
  {
    italics_correction = Length.zero;
    kernTR             = FontInfo.no_math_kern;
    kernBR             = FontInfo.no_math_kern;
    right_math_kind    = mk;
    last_height        = hgt;
    last_depth         = dpt;
  }


type math_kern_specification =
  | MathKernInfo of FontFormat.math_kern_info option
  | MathKernFunc of math_kern_func * math_kern_func


let make_left_and_right_kern hgt dpt mk mic mkspec : left_kern * right_kern =
  let lk =
    match mkspec with
    | MathKernInfo(Some(mki)) ->
        {
          kernTL         = FontInfo.make_discrete_math_kern mki.FontFormat.kernTL;
          kernBL         = FontInfo.make_discrete_math_kern mki.FontFormat.kernBL;
          left_math_kind = mk;
          first_height   = hgt;
          first_depth    = dpt;
        }

    | MathKernInfo(None) ->
        no_left_kern hgt dpt mk

    | MathKernFunc(kernfL, _) ->
        let kernL = FontInfo.make_dense_math_kern kernfL in
          {
            kernTL         = kernL;
            kernBL         = kernL;
            left_math_kind = mk;
            first_height   = hgt;
            first_depth    = dpt;
          }
  in
  let rk =
    match mkspec with
    | MathKernInfo(Some(mki)) ->
        {
          italics_correction = mic;
          kernTR             = FontInfo.make_discrete_math_kern mki.FontFormat.kernTR;
          kernBR             = FontInfo.make_discrete_math_kern mki.FontFormat.kernBR;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | MathKernInfo(None) ->
        {
          italics_correction = mic;
          kernTR             = FontInfo.no_math_kern;
          kernBR             = FontInfo.no_math_kern;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | MathKernFunc(_, kernfR) ->
        let kernR = FontInfo.make_dense_math_kern kernfR in
          {
            italics_correction = mic;
            kernTR             = kernR;
            kernBR             = kernR;
            right_math_kind    = mk;
            last_height        = hgt;
            last_depth         = dpt;
          }
  in
    (lk, rk)


let fixed_empty wid =
  HorzPure(PHSFixedEmpty(wid))


let outer_empty wid shrink stretch =
  HorzPure(PHSOuterEmpty(wid, shrink, stretch))


let normalize_math_kind mkprev mknext mkraw =
  match mkraw with
  | MathOrdinary
  | MathOpen
  | MathClose
  | MathPrefix
  | MathOperator
  | MathPunct
  | MathInner
  | MathEnd
    -> mkraw

  | MathBinary ->
      let bprev =
        match mkprev with
        | MathOperator
        | MathBinary
        | MathRelation
        | MathOpen
        | MathPunct
            -> true
        | _ -> false
      in
      let bnext =
        match mknext with
        | MathRelation
        | MathClose
        | MathPunct
            -> true
        | _ -> false
      in
        if bprev || bnext then
          MathOrdinary
        else
          MathBinary

  | MathRelation ->
      mkraw


let space_ord_bin ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_bin in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))


let space_ord_rel ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_rel in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))


let space_ord_inner ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_inner in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))


let space_punct ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_punct in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))

let space_ord_op ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_op in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))


let space_ord_prefix ctx fontsize =
  let (r_natural, r_shrink, r_stretch) = ctx.HorzBox.space_math_prefix in
    Some(outer_empty (fontsize *% r_natural) (fontsize *% r_shrink) (fontsize *% r_stretch))


let space_after_script mathctx =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  if not (MathContext.is_in_base_level mathctx) then
    None
  else
    let mc = FontInfo.get_math_constants mathctx in
    Some(outer_empty (fontsize *% mc.FontFormat.space_after_script) Length.zero Length.zero)
      (* temporary; should have variable stretchability and shrinkability *)


let space_between_math_kinds (mathctx : math_context) (mkprev : math_kind) (corr : space_correction) (mk : math_kind) : horz_box option =
  let is_in_script = not (MathContext.is_in_base_level mathctx) in
  let ctx = MathContext.context_main mathctx in
  let fontsize = FontInfo.actual_math_font_size mathctx in
    if is_in_script then
      match (mkprev, mk) with
      | (MathOperator, MathOrdinary)
      | (MathOrdinary, MathOperator)
      | (MathOperator, MathOperator)
      | (MathClose   , MathOperator)
      | (MathInner   , MathOperator)
          -> space_ord_op ctx fontsize

      | _ -> None
    else
      match (mkprev, mk) with
      | (MathPunct   , _           )
        -> space_punct ctx fontsize

      | (MathInner   , MathOrdinary)
      | (MathInner   , MathOpen    )
      | (MathInner   , MathPunct   )
      | (MathInner   , MathInner   )
      | (MathOrdinary, MathInner   )
      | (MathPrefix  , MathInner   )
      | (MathClose   , MathInner   )
        -> space_ord_inner ctx fontsize

      | (_           , MathClose   )
        ->
          begin
            match corr with
            | NoSpace                     -> None
            | SpaceAfterScript            -> None
            | ItalicsCorrection(italcorr) -> Some(fixed_empty italcorr)
          end

      | (MathOrdinary, MathOpen    )
      | (MathPrefix  , MathOpen    )
        ->
          begin
            match corr with
            | NoSpace                     -> None
            | SpaceAfterScript            -> space_after_script mathctx
            | ItalicsCorrection(italcorr) -> Some(fixed_empty italcorr)
          end

      | (MathBinary  , MathOrdinary)
      | (MathBinary  , MathPrefix  )
      | (MathBinary  , MathOperator)
      | (MathBinary  , MathOpen    )
      | (MathBinary  , MathInner   )
      | (MathOrdinary, MathBinary  )
      | (MathClose   , MathBinary  )
      | (MathInner   , MathBinary  )
        -> space_ord_bin ctx fontsize

      | (MathRelation, MathOrdinary)
      | (MathRelation, MathOperator)
      | (MathRelation, MathInner   )
      | (MathRelation, MathOpen    )
      | (MathRelation, MathPrefix  )
      | (MathOrdinary, MathRelation)
      | (MathOperator, MathRelation)
      | (MathInner   , MathRelation)
      | (MathClose   , MathRelation)
        -> space_ord_rel ctx fontsize

      | (MathOperator, MathOrdinary)
      | (MathOperator, MathOperator)
      | (MathOperator, MathInner   )
      | (MathOperator, MathPrefix  )
      | (MathOrdinary, MathOperator)
      | (MathClose   , MathOperator)
      | (MathInner   , MathOperator)
        -> space_ord_op ctx fontsize

      | (MathOrdinary, MathPrefix  )
      | (MathInner   , MathPrefix  )
        -> space_ord_prefix ctx fontsize

      | (_           , MathEnd     )
      | (MathEnd     , _           )
        -> None

      | _ ->
          begin
            match corr with
            | NoSpace              -> None
            | SpaceAfterScript     -> space_after_script mathctx
            | ItalicsCorrection(_) -> None
          end


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

let get_left_kern lmmain hgt dpt =
  let nokernf = no_left_kern hgt dpt in
  match lmmain with
  | LowMathPure(_, _, _, _, _, lk, _)          -> lk
  | LowMathList(_, (_, _, _, lk, _))           -> lk
  | LowMathGroup(mkL, _, _)                    -> nokernf mkL
  | LowMathSubscript(_, (_, _, _, lk, _), _)   -> lk
  | LowMathSuperscript(_, (_, _, _, lk, _), _) -> lk
  | LowMathSubSuperscript(_, _, (_, _, _, lk, _), _, _) -> lk
  | LowMathFraction(_, _, _, _)                -> nokernf MathInner
  | LowMathRadical(_)                          -> nokernf MathInner
  | LowMathRadicalWithDegree(_, _, _, _, _)    -> nokernf MathInner

  | LowMathParen(lpL, _, _)
  | LowMathParenWithMiddle(lpL, _, _, _)
      -> make_left_paren_kern lpL.lp_height lpL.lp_depth lpL.lp_math_kern_scheme

  | LowMathUpperLimit(_, (_, _, _, lk, _), _)  -> lk
  | LowMathLowerLimit(_, (_, _, _, lk, _), _)  -> lk


let get_right_kern lmmain hgt dpt =
  let nokernf = no_right_kern hgt dpt in
  match lmmain with
  | LowMathPure(_, _, _, _, _, _, rk)          -> rk
  | LowMathList(_, (_, _, _, _, rk))           -> rk
  | LowMathGroup(_, mkR, _)                    -> nokernf mkR
  | LowMathSubscript(_, (_, _, _, _, rk), _)   -> nokernf rk.right_math_kind
  | LowMathSuperscript(_, (_, _, _, _, rk), _) -> nokernf rk.right_math_kind
  | LowMathSubSuperscript(_, _, (_, _, _, _, rk), _, _) -> nokernf rk.right_math_kind
  | LowMathFraction(_, _, _, _)                -> nokernf MathInner
  | LowMathRadical(_)                          -> nokernf MathInner
  | LowMathRadicalWithDegree(_, _, _, _, _)    -> nokernf MathInner

  | LowMathParen(_, lpR, _)
  | LowMathParenWithMiddle(_, lpR, _, _)
      -> make_right_paren_kern lpR.lp_height lpR.lp_depth lpR.lp_math_kern_scheme

  | LowMathUpperLimit(_, (_, _, _, _, rk), _)  -> rk
  | LowMathLowerLimit(_, (_, _, _, _, rk), _)  -> rk


let get_math_kind_of_math_element (ctx : input_context) = function
  | MathElement(mk, _)              -> mk
  | MathVariantChar(uch)            -> let (mk, _) = MathContext.convert_math_variant_char ctx uch in mk
  | MathVariantCharDirect(mk, _, _) -> mk


let rec get_left_math_kind (ctx : input_context) = function
  | MathPure(me)                   -> get_math_kind_of_math_element ctx me
  | MathPullInScripts(mkL, _, _)   -> mkL
  | MathGroup(mkL, _, _)           -> mkL
  | MathSuperscript([], _)         -> MathEnd
  | MathSuperscript(mathB :: _, _) -> get_left_math_kind ctx mathB
  | MathSubscript([], _)           -> MathEnd
  | MathSubscript(mathB :: _, _)   -> get_left_math_kind ctx mathB
  | MathFraction(_, _)             -> MathInner
  | MathRadical(_)                 -> MathInner
  | MathRadicalWithDegree(_, _)    -> MathInner
  | MathParen(_, _, _)             -> MathOpen
  | MathParenWithMiddle(_, _, _, _) -> MathOpen
  | MathLowerLimit(mathB :: _, _)  -> get_left_math_kind ctx mathB
  | MathLowerLimit([], _)          -> MathEnd
  | MathUpperLimit(mathB :: _, _)  -> get_left_math_kind ctx mathB
  | MathUpperLimit([], _)          -> MathEnd
  | MathChangeContext(_, [])       -> MathEnd
  | MathChangeContext(_, m :: _)   -> get_left_math_kind ctx m


let rec get_right_math_kind (ctx : input_context) math =
  try
    match math with
    | MathPure(me)                 -> get_math_kind_of_math_element ctx me
    | MathPullInScripts(_, mkR, _) -> mkR
    | MathGroup(_, mkR, _)         -> mkR
    | MathSuperscript([], _)       -> MathEnd
    | MathSuperscript(mathlst, _)  -> get_right_math_kind ctx (List.hd (List.rev mathlst))
    | MathSubscript([], _)         -> MathEnd
    | MathSubscript(mathlst, _)    -> get_right_math_kind ctx (List.hd (List.rev mathlst))
    | MathFraction(_, _)           -> MathInner
    | MathRadical(_)               -> MathInner
    | MathRadicalWithDegree(_, _)  -> MathInner
    | MathParen(_, _, _)           -> MathClose
    | MathParenWithMiddle(_, _, _, _) -> MathClose
    | MathLowerLimit([], _)        -> MathEnd
    | MathLowerLimit(mathlst, _)   -> get_right_math_kind ctx (List.hd (List.rev mathlst))
    | MathUpperLimit([], _)        -> MathEnd
    | MathUpperLimit(mathlst, _)   -> get_right_math_kind ctx (List.hd (List.rev mathlst))
    | MathChangeContext(_, [])     -> MathEnd
    | MathChangeContext(_, mlst)   -> get_right_math_kind ctx (List.hd (List.rev mlst))
  with
  | Invalid_argument(_) -> assert false


let superscript_baseline_height mathctx h_base d_sup =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_supbmin = fontsize *% mc.FontFormat.superscript_bottom_min in
  let h_supstd  = fontsize *% mc.FontFormat.superscript_shift_up in
  let l_supdmax = fontsize *% mc.FontFormat.superscript_baseline_drop_max in
  let cand = [h_supstd; h_base -% l_supdmax; h_supbmin +% (Length.negate d_sup)] in
  let h_supbl = cand |> List.fold_left Length.max Length.zero in
    h_supbl


(* -- calculates the base correction height and the superscript correction height -- *)
let superscript_correction_heights mathctx h_supbl h_base d_sup =
  let l_base = h_supbl +% d_sup in
  let l_sup = h_base -% h_supbl in
    (l_base, l_sup)


let subscript_baseline_depth mathctx d_base h_sub =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_subtmax = fontsize *% mc.FontFormat.subscript_top_max in
  let d_substd  = Length.negate (fontsize *% mc.FontFormat.subscript_shift_down) in
  let l_subdmin = fontsize *% mc.FontFormat.subscript_baseline_drop_min in
  let cand = [Length.negate d_substd; (Length.negate d_base) +% l_subdmin; h_sub -% h_subtmax] in
  let d_subbl = Length.negate (cand |> List.fold_left Length.max Length.zero) in
    d_subbl


let subscript_correction_heights mathctx d_subbl d_base h_sub =
  let l_base = h_sub +% d_base in
  let l_sub = d_base -% d_subbl in
    (l_base, l_sub)


let correct_script_baseline_heights mathctx d_sup h_sub h_supbl_raw d_subbl_raw =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let l_gapmin = fontsize *% mc.FontFormat.sub_superscript_gap_min in
  let l_gap = (h_supbl_raw +% d_sup) -% (d_subbl_raw +% h_sub) in
  if l_gap <% l_gapmin then
    let corr = (l_gapmin -% l_gap) *% 0.5 in
    (h_supbl_raw +% corr, d_subbl_raw -% corr)
  else
    (h_supbl_raw, d_subbl_raw)


let numerator_baseline_height mathctx d_numer =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_numerstd    = fontsize *% mc.FontFormat.fraction_numer_d_shift_up in
  let l_numergapmin = fontsize *% mc.FontFormat.fraction_numer_d_gap_min in
  let h_numerbl = Length.max h_numerstd (h_bar +% t_bar *% 0.5 +% l_numergapmin +% (Length.negate d_numer)) in
    h_numerbl


let denominator_baseline_depth mathctx h_denom =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let d_denomstd    = Length.negate (fontsize *% mc.FontFormat.fraction_denom_d_shift_down) in
  let l_denomgapmin = fontsize *% mc.FontFormat.fraction_denom_d_gap_min in
  let d_denombl = Length.min d_denomstd (h_bar -% t_bar *% 0.5 -% l_denomgapmin -% h_denom) in
    d_denombl


let upper_limit_baseline_height mathctx h_base d_up =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let l_upmingap = fontsize *% mc.FontFormat.upper_limit_gap_min in
  let l_upblstd = fontsize *% mc.FontFormat.upper_limit_baseline_rise_min in
  let h_upbl = Length.max (h_base +% l_upblstd) (h_base +% l_upmingap +% (Length.negate d_up)) in
    h_upbl


let lower_limit_baseline_depth mathctx d_base h_low =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let l_lowmingap = fontsize *% mc.FontFormat.lower_limit_gap_min in
  let l_lowblstd = fontsize *% mc.FontFormat.lower_limit_baseline_drop_min in
  let d_lowbl = Length.min (d_base -% l_lowblstd) (d_base -% l_lowmingap -% h_low) in
    d_lowbl



(* --
   radical_bar_metrics:
     takes a math context, a script level, and the height of the contents,
     and then returns the height, the thickness, and the extra ascender of the raducal rule.
   -- *)
let radical_bar_metrics mathctx h_cont =
  let fontsize = FontInfo.actual_math_font_size mathctx in
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
(*
let radical_degree_baseline_height mathctx scriptlev h_rad d_deg =
  let mc = FontInfo.get_math_constants mathctx in
  let h_degb = h_rad *% mc.FontFormat.radical_degree_bottom in
  let h_degbl = h_degb +% d_deg in
    h_degbl
*)


let make_paren mathctx paren hgt dpt =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar = fontsize *% mc.FontFormat.axis_height in
  let (hblst, kernf) = paren hgt dpt h_bar fontsize (MathContext.color mathctx) in
    (hblst, FontInfo.make_dense_math_kern kernf)


let make_radical mathctx radical hgt_bar t_bar dpt =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let hblst = radical hgt_bar t_bar dpt fontsize (MathContext.color mathctx) in
    hblst


let convert_math_char mathctx is_big (uchlst : Uchar.t list) mk =
  let mathstrinfo = FontInfo.get_math_string_info mathctx in
  let is_in_display = true (* temporary *) in
  let (otxt, wid, hgt, dpt, mic, mkiopt) = FontInfo.get_math_char_info mathctx is_in_display is_big uchlst in
  let (lk, rk) = make_left_and_right_kern hgt dpt mk mic (MathKernInfo(mkiopt)) in
    (mk, wid, hgt, dpt, LowMathGlyph(mathstrinfo, wid, hgt, dpt, otxt), lk, rk)


let convert_math_char_with_kern mathctx is_big (uchlst : Uchar.t list) mk (kernfL : math_char_kern_func) (kernfR : math_char_kern_func) =
  let mathstrinfo = FontInfo.get_math_string_info mathctx in
  let is_in_display = true (* temporary *) in
  let (otxt, wid, hgt, dpt, mic, _) = FontInfo.get_math_char_info mathctx is_in_display is_big uchlst in
  let fontsize = mathstrinfo.math_font_size in
  let mkspec = MathKernFunc(kernfL fontsize, kernfR fontsize) in  (* temporary *)
  let (lk, rk) = make_left_and_right_kern hgt dpt mk mic mkspec in
    (mk, wid, hgt, dpt, LowMathGlyph(mathstrinfo, wid, hgt, dpt, otxt), lk, rk)


let change_math_context chg mathctx =
  match chg with
  | MathChangeColor(color)         -> mathctx |> MathContext.set_color color
  | MathChangeMathCharClass(mccls) -> mathctx |> MathContext.set_math_char_class mccls


let rec check_subscript mlstB =
  match List.rev mlstB with
  | MathSubscript(mlstBB, mlstT) :: mtailrev ->
    (* -- if the last element of the base contents has a subscript -- *)
      let mlstBnew = List.rev_append mtailrev mlstBB in
      Some((mlstT, mlstBnew))

  | MathChangeContext(chg, mlstBsub) :: mtailrev ->
      begin
        match check_subscript mlstBsub with
        | Some((mlstT, mlstBsubnew)) ->
            let mlstBnew = List.rev (MathChangeContext(chg, mlstBsubnew) :: mtailrev) in
              Some(([MathChangeContext(chg, mlstT)], mlstBnew))

        | None ->
            None
      end

  | _ -> None


let check_pull_in mlstB =
  match List.rev mlstB with
  | MathPullInScripts(mkL, mkR, mlstf) :: mtailrev ->
    (* -- if the last element of the base contents is a pull-in-scripts -- *)
      Some(((mkL, mkR, mlstf), List.rev mtailrev))

  | _ ->
      None


let rec convert_math_element (mathctx : math_context) (mkprev : math_kind) (mknext : math_kind) (me : math_element) : low_math_pure =
  match me with
  | MathElement(mkraw, MathEmbeddedText(hblstf)) ->
      let hblst = hblstf (MathContext.context_for_text mathctx) in
      let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
      let mk = normalize_math_kind mkprev mknext mkraw in
        (mk, wid, hgt, dpt, LowMathEmbeddedHorz(hblst), no_left_kern hgt dpt mk, no_right_kern hgt dpt mk)

  | MathVariantChar(uch_from) ->
      let (mkrawv, uch_to) = MathContext.convert_math_variant_char (MathContext.context_for_text mathctx) uch_from in
      let mk = normalize_math_kind mkprev mknext mkrawv in
      let is_big = false in
      convert_math_char mathctx is_big [uch_to] mk

  | MathVariantCharDirect(mkraw, is_big, mvsty) ->
      let mk = normalize_math_kind mkprev mknext mkraw in
      let mccls = MathContext.math_char_class mathctx in
      let uchlst =
        match mccls with
        | MathItalic       -> mvsty.math_italic
        | MathBoldItalic   -> mvsty.math_bold_italic
        | MathRoman        -> mvsty.math_roman
        | MathBoldRoman    -> mvsty.math_bold_roman
        | MathScript       -> mvsty.math_script
        | MathBoldScript   -> mvsty.math_bold_script
        | MathFraktur      -> mvsty.math_fraktur
        | MathBoldFraktur  -> mvsty.math_bold_fraktur
        | MathDoubleStruck -> mvsty.math_double_struck
      in
        convert_math_char mathctx is_big uchlst mk

  | MathElement(mkraw, MathChar(is_big, uchlst)) ->
      let mk = normalize_math_kind mkprev mknext mkraw in
        convert_math_char mathctx is_big uchlst mk

  | MathElement(mkraw, MathCharWithKern(is_big, uchlst, kernfL, kernfR)) ->
      let mk = normalize_math_kind mkprev mknext mkraw in
        convert_math_char_with_kern mathctx is_big uchlst mk kernfL kernfR


and convert_to_low (mathctx : math_context) (mkfirst : math_kind) (mklast : math_kind) (mlst : math list) : low_math =
  let optres =
    mlst |> list_fold_adjacent (fun opt math mathprevopt mathnextopt ->
      let mkprev = match mathprevopt with None -> mkfirst | Some(mathprev) -> get_right_math_kind (MathContext.context_for_text mathctx) mathprev in
      let mknext = match mathnextopt with None -> mklast  | Some(mathnext) -> get_left_math_kind (MathContext.context_for_text mathctx) mathnext in
        (* -- get the rightward math class of the previous, and the leftward math class of the next -- *)
      let (lmmain, hgt, dpt) = convert_to_low_single mkprev mknext mathctx math in
      let rk = get_right_kern lmmain hgt dpt in
      let lk = get_left_kern lmmain hgt dpt in
        match opt with
        | None                                        -> Some((hgt, dpt, lk, rk, Alist.extend Alist.empty lmmain))
        | Some((hgtprev, dptprev, lkfirst, _, lmacc)) -> Some((Length.max hgt hgtprev, Length.min dpt dptprev, lkfirst, rk, Alist.extend lmacc lmmain))
    ) None
  in
  match optres with
  | None ->
      let lk = no_left_kern Length.zero Length.zero MathEnd in
      let rk = no_right_kern Length.zero Length.zero MathEnd in
        ([], Length.zero, Length.zero, lk, rk)

  | Some((hgt, dpt, lkfirst, rklast, lmmainacc)) ->
      (Alist.to_list lmmainacc, hgt, dpt, lkfirst, rklast)


and convert_to_low_single (mkprev : math_kind) (mknext : math_kind) (mathctx : math_context) (math : math) : low_math_main * length * length =
  match math with
  | MathPure(me) ->
      let (mk, wid, hgt, dpt, lme, lk, rk) = convert_math_element mathctx mkprev mknext me in
        (LowMathPure(mk, wid, hgt, dpt, lme, lk, rk), hgt, dpt)

  | MathPullInScripts(mkL, mkR, mlstf) ->
      let mlstI = mlstf None None in
        (* -- invokes the math-generating function -- *)
      let lmI = convert_to_low mathctx mkprev mknext mlstI in
      let (_, h_inner, d_inner, _, rk) = lmI in
        (LowMathGroup(mkL, mkR, lmI), h_inner, d_inner)

  | MathChangeContext(chg, mlstI) ->
      let mathctxnew = mathctx |> change_math_context chg in
      let lmI = convert_to_low mathctxnew mkprev mknext mlstI in
      let (_, h_inner, d_inner, _, rk) = lmI in
        (LowMathList(Some(chg), lmI), h_inner, d_inner)

  | MathGroup(mkL, mkR, mlstC) ->
      let lmC = convert_to_low mathctx MathEnd MathClose mlstC in
      let (_, h_cont, d_cont, _, _) = lmC in
        (LowMathGroup(mkL, mkR, lmC), h_cont, d_cont)

  | MathFraction(mlstN, mlstD) ->
      let lmN = convert_to_low mathctx MathEnd MathEnd mlstN in
      let lmD = convert_to_low mathctx MathEnd MathEnd mlstD in
      let (_, h_numer, d_numer, _, _) = lmN in
      let (_, h_denom, d_denom, _, _) = lmD in
      let h_numerbl = numerator_baseline_height mathctx d_numer in
      let d_denombl = denominator_baseline_depth mathctx h_denom in
      let h_frac = h_numerbl +% h_numer in
      let d_frac = d_denombl +% d_denom in
        (LowMathFraction(h_numerbl, d_denombl, lmN, lmD), h_frac, d_frac)

  | MathSubscript(mlstB, mlstS) ->
      begin
        match check_pull_in mlstB with
        | Some((pullin, mlstB)) ->
          (* -- if the last element of the base contents is a pull-in-scripts -- *)
            invoke_pull_in_scripts (mathctx, mkprev, mknext) pullin (Some(mlstS)) None mlstB

        | None ->
            let lmB = convert_to_low mathctx mkprev MathEnd mlstB in
            let lmS = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstS in
            let (_, h_base, d_base, _, rkB) = lmB in
            let (_, h_sub, d_sub, _, _)     = lmS in
            let d_subbl = subscript_baseline_depth mathctx rkB.last_depth h_sub in
            let h_whole = h_base in
            let d_whole = Length.min d_base (d_subbl +% d_sub) in
              (LowMathSubscript(d_subbl, lmB, lmS), h_whole, d_whole)
      end

  | MathSuperscript(mlstB, mlstS) ->
      begin
        match check_subscript mlstB with
        | Some((mlstT, mlstB)) ->
          (* -- if the last element of the base contents has a subscript -- *)
            begin
              match check_pull_in mlstB with
              | Some((pullin, mlstB)) ->
                (* -- if the last element of the base contents is a pull-in-scipts -- *)
                  invoke_pull_in_scripts (mathctx, mkprev, mknext) pullin (Some(mlstT)) (Some(mlstS)) mlstB

              | None ->
                  let lmB = convert_to_low mathctx mkprev MathEnd mlstB in
                  let lmS = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstS in
                  let lmT = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstT in
                  let (_, h_base, d_base, _, rkB) = lmB in
                  let (_, h_sup, d_sup, _, _) = lmS in
                  let (_, h_sub, d_sub, _, _) = lmT in
                  let h_supbl_raw = superscript_baseline_height mathctx h_base d_sup in
                  let d_subbl_raw = subscript_baseline_depth mathctx rkB.last_depth h_sub in
                  let (h_supbl, d_subbl) = correct_script_baseline_heights mathctx d_sup h_sub h_supbl_raw d_subbl_raw in
                  let h_whole = Length.max h_base (h_supbl +% h_sup) in
                  let d_whole = Length.min d_base (d_subbl +% d_sub) in
                    (LowMathSubSuperscript(h_supbl, d_subbl, lmB, lmS, lmT), h_whole, d_whole)
            end

        | None ->
          (* -- if the last element of the base contents does NOT have a subscript -- *)
            begin
              match check_pull_in mlstB with
              | Some((pullin, mlstB)) ->
                (* -- if the last element of the base contents is a pull-in-scipts -- *)
                  invoke_pull_in_scripts (mathctx, mkprev, mknext) pullin None (Some(mlstS)) mlstB

              | None ->
                  let lmB = convert_to_low mathctx mkprev MathEnd mlstB in
                  let lmS = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstS in
                  let (_, h_base, d_base, _, rkB) = lmB in
                  let (_, h_sup, d_sup, _, _)     = lmS in
                  let h_supbl = superscript_baseline_height mathctx h_base d_sup in
                  let h_whole = Length.max h_base (h_supbl +% h_sup) in
                  let d_whole = d_base in
                    (LowMathSuperscript(h_supbl, lmB, lmS), h_whole, d_whole)
            end
      end

  | MathRadical(radical, mlstC) ->
      let lmC = convert_to_low mathctx MathEnd MathEnd mlstC in
      let (_, h_cont, d_cont, _, _) = lmC in
      let (h_bar, t_bar, l_extra) = radical_bar_metrics mathctx h_cont in
      let hblstrad = make_radical mathctx radical h_bar t_bar d_cont in
      let h_rad = h_bar +% t_bar in
      let h_whole = h_rad +% l_extra in
      let d_whole = d_cont in  (* temporary; should consider the depth of the radical sign *)
        (LowMathRadical(hblstrad, h_bar, t_bar, lmC), h_whole, d_whole)

  | MathRadicalWithDegree(mlstD, mlstC) ->
(*
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
*)
      failwith "unsupported; MathRadicalWithDegree"

  | MathParen(parenL, parenR, mlstC) ->
      let lmC = convert_to_low mathctx MathOpen MathClose mlstC in
      let (_, hC, dC, _, _) = lmC in
      let (hblstparenL, mkernsL) = make_paren mathctx parenL hC dC in
      let (hblstparenR, mkernsR) = make_paren mathctx parenR hC dC in
      let (_, hL, dL)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, hR, dR) = LineBreak.get_natural_metrics hblstparenR in
      let h_whole = [hL; hR] |> List.fold_left Length.max hC in
      let d_whole = [dL; dR] |> List.fold_left Length.min dC in
      let lpL = { lp_main = hblstparenL; lp_height = hL; lp_depth = dL; lp_math_kern_scheme = mkernsL; } in
      let lpR = { lp_main = hblstparenR; lp_height = hR; lp_depth = dR; lp_math_kern_scheme = mkernsR; } in
        (LowMathParen(lpL, lpR, lmC), h_whole, d_whole)

  | MathParenWithMiddle(parenL, parenR, middle, mlstlst) ->
      let lmlst = List.map (convert_to_low mathctx MathOpen MathClose) mlstlst in
      let (hC, dC) =
        lmlst |> List.fold_left (fun (hacc, dacc) lm ->
          let (_, h, d, _, _) = lm in (Length.max hacc h, Length.min dacc d)
        ) (Length.zero, Length.zero)
      in
      let (hblstparenL, mkernsL) = make_paren mathctx parenL hC dC in
      let (hblstparenR, mkernsR) = make_paren mathctx parenR hC dC in
      let (hblstmiddle, _) = make_paren mathctx middle hC dC in
      let (_, hL, dL)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, hR, dR) = LineBreak.get_natural_metrics hblstparenR in
      let (_, hM, dM) = LineBreak.get_natural_metrics hblstmiddle in
      let h_whole = [hL; hR; hM] |> List.fold_left Length.max hC in
      let d_whole = [dL; dR; dM] |> List.fold_left Length.min dC in
      let lpL = { lp_main = hblstparenL; lp_height = hL; lp_depth = dL; lp_math_kern_scheme = mkernsL; } in
      let lpR = { lp_main = hblstparenR; lp_height = hR; lp_depth = dR; lp_math_kern_scheme = mkernsR; } in
      let lpM = { lp_main = hblstmiddle; lp_height = hM; lp_depth = dM; lp_math_kern_scheme = FontInfo.no_math_kern; } in
        (LowMathParenWithMiddle(lpL, lpR, lpM, lmlst), h_whole, d_whole)

  | MathUpperLimit(mlstB, mlstU) ->
      let lmB = convert_to_low mathctx mkprev mknext mlstB in
      let lmU = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstU in
      let (_, h_base, d_base, _, _) = lmB in
      let (_, h_up, d_up, _, _) = lmU in
      let h_upbl = upper_limit_baseline_height mathctx h_base d_up in
      let h_whole = h_upbl +% h_up in
      let d_whole = d_base in
        (LowMathUpperLimit(h_upbl, lmB, lmU), h_whole, d_whole)

  | MathLowerLimit(mlstB, mlstL) ->
      let lmB = convert_to_low mathctx mkprev mknext mlstB in
      let lmL = convert_to_low (MathContext.enter_script mathctx) MathEnd MathEnd mlstL in
      let (_, h_base, d_base, _, _) = lmB in
      let (_, h_low, d_low, _, _) = lmL in
      let d_lowbl = lower_limit_baseline_depth mathctx d_base h_low in
      let h_whole = h_base in
      let d_whole = d_lowbl +% d_low in
        (LowMathLowerLimit(d_lowbl, lmB, lmL), h_whole, d_whole)


(* --
   'mlstSopt': 'None' or subscript enveloped in 'Some(_)'
   'mlstTopt': 'None' or superscript enveloped in 'Some(_)'
   'mlstB': the math list attached before the math list generated by pull-in-scripts
   -- *)
and invoke_pull_in_scripts (mathctx, mkprev, mknext) (mkL, mkR, mlstf) mlstSopt mlstTopt mlstB =
  let mlstIsub = mlstf mlstSopt mlstTopt in
    (* -- invokes the math-generating function -- *)
  let mlstI = List.append mlstB [MathGroup(mkL, mkR, mlstIsub)] in
  let lmI = convert_to_low mathctx mkprev mknext mlstI in
  let (_, h_inner, d_inner, _, rk) = lmI in
    (LowMathList(None, lmI), h_inner, d_inner)


let horz_of_low_math_element (lme : low_math_atom) : horz_box list =
  match lme with
  | LowMathGlyph(mathstrinfo, wid, hgt, dpt, otxt) ->
      [HorzPure(PHCInnerMathGlyph(mathstrinfo, wid, hgt, dpt, otxt))]

  | LowMathEmbeddedHorz(hblst) ->
      hblst


let ratioize n =
  (float_of_int n) /. 1000.


let horz_fraction_bar mathctx wid =
  let fontsize = FontInfo.actual_math_font_size mathctx in
  let mc = FontInfo.get_math_constants mathctx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_bart        = h_bar +% t_bar *% 0.5 in
  let bar_color = MathContext.color mathctx in
  let bar_graphics (xpos, ypos) =
    GraphicD.singleton (GraphicD.make_fill bar_color [Rectangle((xpos, ypos +% h_bart), (wid, t_bar))])
  in
    HorzPure(PHGFixedGraphics(wid, h_bart, Length.zero, bar_graphics))


let calculate_kern mathctx (mkernsch : FontInfo.math_kern_scheme) (corrhgt : length) : length =
  FontInfo.get_math_kern mathctx mkernsch corrhgt


let raise_horz r hblst =
  [HorzPure(PHGRising(r, hblst))]


let get_space_correction = function
  | LowMathList(_, (_, _, _, _, rk))
  | LowMathPure(_, _, _, _, _, _, rk)
      -> ItalicsCorrection(rk.italics_correction)

  | LowMathSubscript(_, _, _)
  | LowMathSuperscript(_, _, _)
  | LowMathSubSuperscript(_, _, _, _, _)
      -> SpaceAfterScript

  | _ -> NoSpace


let rec horz_of_low_math (mathctx : math_context) (mkprevfirst : math_kind) (mklast : math_kind) (lm : low_math) =
  let (lmmainlst, _, _, _, _) = lm in
  let rec aux (hbacc : horz_box Alist.t) mkprev (corr : space_correction) lmmainlst =
    match lmmainlst with
    | [] ->
        let hbspaceopt = space_between_math_kinds mathctx mkprev corr mklast in
        begin
          match hbspaceopt with
          | None          -> Alist.to_list hbacc
          | Some(hbspace) -> Alist.to_list (Alist.extend hbacc hbspace)
              (* --
                 appends italics correction for the last glyph of inner contents of a parenthesis;
                 -- *)
        end

    | lmmain :: lmmaintail ->
        let corrnext = get_space_correction lmmain in
        let (hblst, hbspaceopt, mk) =
          match lmmain with
          | LowMathList(chgopt, lmI) ->
              let (_, _, _, lkI, rkI) = lmI in
              let mathctxnew =
                match chgopt with
                | None      -> mathctx
                | Some(chg) -> mathctx |> change_math_context chg
              in
              let hblst = horz_of_low_math mathctxnew mkprevfirst MathEnd lmI in
              let hbspaceopt = space_between_math_kinds mathctxnew mkprev corr lkI.left_math_kind in
                (hblst, hbspaceopt, rkI.right_math_kind)

          | LowMathPure(mk, wid, hgt, dpt, lma, _, rk) ->
              let hblstpure = horz_of_low_math_element lma in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr mk in
                (hblstpure, hbspaceopt, mk)

          | LowMathGroup(mkL, mkR, lmC) ->
              let hblstC = horz_of_low_math mathctx MathEnd MathClose lmC in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr mkL in
                (hblstC, hbspaceopt, mkR)

          | LowMathSuperscript(h_supbl, lmB, lmS) ->
              let (_, _, _, lkB, rkB) = lmB in
              let (_, _, d_sup, lkS, _) = lmS in
              let hblstB = horz_of_low_math mathctx MathEnd MathEnd lmB in
              let hblstS = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmS in
              let h_base = rkB.last_height in
              let (l_base, l_sup) = superscript_correction_heights mathctx h_supbl h_base d_sup in
              let l_kernbase = calculate_kern mathctx rkB.kernTR l_base in
              let l_kernsup  = calculate_kern (MathContext.enter_script mathctx) lkS.kernBL l_sup in
              let l_italic   = rkB.italics_correction in
              let kern = l_italic +% l_kernbase +% l_kernsup in
              let hbkern = fixed_empty kern in
              let hblstsup =
                List.concat [hblstB; [hbkern]; raise_horz h_supbl hblstS]
              in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr lkB.left_math_kind in
                (hblstsup, hbspaceopt, rkB.right_math_kind)

          | LowMathSubscript(d_subbl, lmB, lmS) ->
              let (_, _, _, lkB, rkB) = lmB in
              let (_, h_sub, _, lkS, _) = lmS in
              let hblstB = horz_of_low_math mathctx MathEnd MathEnd lmB in
              let hblstS = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmS in
              let d_base = rkB.last_depth in
              let (l_base, l_sub) = subscript_correction_heights mathctx d_subbl d_base h_sub in
              let l_kernbase = calculate_kern mathctx rkB.kernBR l_base in
              let l_kernsub  = calculate_kern mathctx lkS.kernTL l_sub in
              let kern = l_kernbase +% l_kernsub in
              let hbkern = fixed_empty kern in
              let (w_sub, _, _) = LineBreak.get_natural_metrics hblstS in
              let hblstsub =
                let lensub = kern +% w_sub in
                if lensub <% Length.zero then
                (* -- if the leftward shift by the kern is larger than the width of the subscript -- *)
                  let hbsupplement = fixed_empty (Length.negate lensub) in
                  List.concat [hblstB; [hbkern]; raise_horz d_subbl hblstS; [hbsupplement]]
                else
                  List.concat [hblstB; [hbkern]; raise_horz d_subbl hblstS]
              in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr lkB.left_math_kind in
                (hblstsub, hbspaceopt, rkB.right_math_kind)

          | LowMathSubSuperscript(h_supbl, d_subbl, lmB, lmS, lmT) ->
              let (_, _, _, lkB, rkB) = lmB in
              let (_, _, d_sup, lkS, _) = lmS in
              let (_, h_sub, _, lkT, _) = lmT in
              let hblstB = horz_of_low_math mathctx MathEnd MathEnd lmB in
              let hblstS = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmS in
              let hblstT = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmT in
              let h_base = rkB.last_height in
              let d_base = rkB.last_depth in

              let (l_base_sup, l_sup) = superscript_correction_heights mathctx h_supbl h_base d_sup in
              let l_kernbase_sup = calculate_kern mathctx rkB.kernTR l_base_sup in
              let l_kernsup      = calculate_kern (MathContext.enter_script mathctx) lkS.kernBL l_sup in
              let l_italic       = rkB.italics_correction in
              let kernsup = l_italic +% l_kernbase_sup +% l_kernsup in
              let hbkernsup = fixed_empty kernsup in

              let (l_base_sub, l_sub) = subscript_correction_heights mathctx d_subbl d_base h_sub in
              let l_kernbase_sub = calculate_kern mathctx rkB.kernBR l_base_sub in
              let l_kernsub      = calculate_kern mathctx lkS.kernTL l_sub in
              let kernsub = (l_kernbase_sub +% l_kernsub) in
              let hbkernsub = fixed_empty kernsub in

              let (w_sup, _, _) = LineBreak.get_natural_metrics hblstS in
              let (w_sub, _, _) = LineBreak.get_natural_metrics hblstT in
              let foresup = kernsup +% w_sup in
              let foresub = kernsub +% w_sub in
              let hbbacksub = fixed_empty (Length.negate foresub) in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr lkB.left_math_kind in
              let hblstappended =
                let hblstlst =
                  [hblstB; [hbkernsub]; raise_horz d_subbl hblstT; [hbbacksub; hbkernsup]; raise_horz h_supbl hblstS]
                in
                if Length.zero <% foresup && foresub <% foresup then
                (* -- if the superscript is wider than the subscript and leftward kern value -- *)
                  List.concat hblstlst
                else
                  let hbsupplement = fixed_empty ((Length.max Length.zero foresub) -% foresup) in
                  List.concat (List.append hblstlst [[hbsupplement]])
              in
                (hblstappended, hbspaceopt, rkB.right_math_kind)

          | LowMathFraction(h_numerbl, d_denombl, lmN, lmD) ->
              let hblstN = horz_of_low_math mathctx MathEnd MathEnd lmN in
              let hblstD = horz_of_low_math mathctx MathEnd MathEnd lmD in
              let (w_numer, _, _) = LineBreak.get_natural_metrics hblstN in
              let (w_denom, _, _) = LineBreak.get_natural_metrics hblstD in
              let (hblstNret, hblstDret, w_frac) =
                if w_numer <% w_denom then
                (* -- if the numerator is narrower than the denominator -- *)
                  let space = (w_denom -% w_numer) *% 0.5 in
                  let hblst_space = fixed_empty space in
                  let hblstNnew = List.concat [[hblst_space]; hblstN; [hblst_space]] in
                    (hblstNnew, hblstD, w_denom)
                else
                  let space = (w_numer -% w_denom) *% 0.5 in
                  let hblst_space = fixed_empty space in
                  let hblstDnew = List.concat [[hblst_space]; hblstD; [hblst_space]] in
                    (hblstN, hblstDnew, w_numer)
              in
              let hbback = fixed_empty (Length.negate w_frac) in
              let hbbar = horz_fraction_bar mathctx w_frac in
              let hblstsub = List.concat [raise_horz h_numerbl hblstNret; [hbback; hbbar; hbback]; raise_horz d_denombl hblstDret] in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr MathInner in
                (hblstsub, hbspaceopt, MathInner)

          | LowMathRadical(hblstrad, h_bar, t_bar, lmC) ->
              let hblstC = horz_of_low_math mathctx MathEnd MathEnd lmC in
              let (w_cont, _, _) = LineBreak.get_natural_metrics hblstC in
              let hbbar =
                HorzPure(PHGFixedGraphics(w_cont, h_bar +% t_bar, Length.zero,
                  (fun (xpos, ypos) ->
                    let grelem =
                      GraphicD.make_fill (MathContext.color mathctx) [Rectangle((xpos, ypos +% h_bar), (w_cont, t_bar))]
                    in
                      GraphicD.singleton grelem)))
              in
              let hbback = fixed_empty (Length.negate w_cont) in
              let hblstsub = List.append hblstrad (hbbar :: hbback :: hblstC) in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr MathInner in
                (hblstsub, hbspaceopt, MathInner)

          | LowMathRadicalWithDegree(h_bar, t_bar, h_degbl, lmD, lmC) ->
              failwith "unsupported; LowMathRadicalWithDegree"  (* temporary *)

          | LowMathParen(lpL, lpR, lmE) ->
              let hblstparenL = lpL.lp_main in
              let hblstparenR = lpR.lp_main in
              let hblstE = horz_of_low_math mathctx MathOpen MathClose lmE in
              let hblstsub = List.concat [hblstparenL; hblstE; hblstparenR] in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr MathOpen in
                (hblstsub, hbspaceopt, MathClose)

          | LowMathParenWithMiddle(lpL, lpR, lpM, lmlst) ->
              let hblstparenL = lpL.lp_main in
              let hblstparenR = lpR.lp_main in
              let hblstmiddle = lpM.lp_main in
              let hblstlst = List.map (horz_of_low_math mathctx MathOpen MathClose) lmlst in
              let hblstC =
                let opt =
                  hblstlst |> List.fold_left (fun hblstaccopt hblst ->
                    match hblstaccopt with
                    | None           -> Some(Alist.extend Alist.empty hblst)
                    | Some(hblstacc) -> Some(Alist.extend (Alist.extend hblstacc hblstmiddle) hblst)
                  ) None
                in
                  match opt with
                  | None           -> []
                  | Some(hblstacc) -> hblstacc |> Alist.to_list |> List.concat
              in
              let hblstsub = List.concat [hblstparenL; hblstC; hblstparenR] in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr MathOpen in
                (hblstsub, hbspaceopt, MathClose)

          | LowMathUpperLimit(h_upbl, lmB, lmU) ->
              let (_, _, _, lkB, rkB) = lmB in
              let hblstB = horz_of_low_math mathctx MathEnd MathEnd lmB in
                (* needs reconsideration *)
              let hblstU = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmU in
              let (w_base, _, _) = LineBreak.get_natural_metrics hblstB in
              let (w_up, _, _) = LineBreak.get_natural_metrics hblstU in
              let hblstsub =
                if w_base <% w_up then
                (* -- if the upper script is wider than the base -- *)
                  let space = (w_up -% w_base) *% 0.5 in
                  let hbspace = fixed_empty space in
                  let hbback = fixed_empty (Length.negate w_up) in
                    List.concat [raise_horz h_upbl hblstU; [hbback; hbspace]; hblstB; [hbspace]]
                else
                  let space = (w_base -% w_up) *% 0.5 in
                  let hbspace = fixed_empty space in
                  let hbback = fixed_empty (Length.negate w_base) in
                    List.concat [[hbspace]; raise_horz h_upbl hblstU; [hbspace; hbback]; hblstB]
              in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr lkB.left_math_kind in
                (hblstsub, hbspaceopt, rkB.right_math_kind)

          | LowMathLowerLimit(d_lowbl, lmB, lmL) ->
              let (_, _, _, lkB, rkB) = lmB in
              let hblstB = horz_of_low_math mathctx MathEnd MathEnd lmB in
                (* needs reconsideration *)
              let hblstL = horz_of_low_math (MathContext.enter_script mathctx) MathEnd MathEnd lmL in
              let (w_base, _, _) = LineBreak.get_natural_metrics hblstB in
              let (w_low, _, _) = LineBreak.get_natural_metrics hblstL in
              let hblstsub =
                if w_base <% w_low then
                (* -- if the lower script is wider than the base -- *)
                  let space = (w_low -% w_base) *% 0.5 in
                  let hbspace = fixed_empty space in
                  let hbback = fixed_empty (Length.negate w_low) in
                    List.concat [raise_horz d_lowbl hblstL; [hbback; hbspace]; hblstB; [hbspace]]
                else
                  let space = (w_base -% w_low) *% 0.5 in
                  let hbspace = fixed_empty space in
                  let hbback = fixed_empty (Length.negate w_base) in
                    List.concat [[hbspace]; raise_horz d_lowbl hblstL; [hbspace; hbback]; hblstB]
              in
              let hbspaceopt = space_between_math_kinds mathctx mkprev corr lkB.left_math_kind in
                (hblstsub, hbspaceopt, rkB.right_math_kind)
        in
        let hbaccnew =
          match hbspaceopt with
          | None          -> Alist.append hbacc hblst
          | Some(hbspace) -> Alist.append (Alist.extend hbacc hbspace) hblst
        in
          aux hbaccnew mk corrnext lmmaintail

  in
  aux Alist.empty mkprevfirst NoSpace lmmainlst


let main (mathctx : math_context) (mathlst : math list) : horz_box list =
  let lmlst = convert_to_low mathctx MathEnd MathEnd mathlst in
  let hblst = horz_of_low_math mathctx MathEnd MathEnd lmlst in
    hblst


let space_between_maths (mathctx : math_context) (mathlst1 : math list) (mathlst2 : math list) : horz_box option =
  let ctx = MathContext.context_for_text mathctx in
  match (List.rev mathlst1, mathlst2) with
  | (math1R :: _, math2L :: _) ->
      let mk1R = get_right_math_kind ctx math1R in
      let mk2L = get_left_math_kind ctx math2L in
      let lmlst1 = convert_to_low mathctx MathEnd mk2L mathlst1 in
      let corr =
        match lmlst1 with
        | (lmmain :: _, _, _, _, _) -> get_space_correction lmmain
        | ([], _, _, _, _)          -> NoSpace
      in
        space_between_math_kinds mathctx mk1R corr mk2L

  | _ ->
      None
