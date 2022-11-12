
open MyUtil
open LengthInterface
open GraphicBase
open HorzBox
open Types


type left_kern = {
  kernTL             : MathKernScheme.t;
  kernBL             : MathKernScheme.t;
  left_math_kind     : math_kind;
  first_height       : length;
  first_depth        : length;
}

type right_kern = {
  italics_correction : length;
  kernTR             : MathKernScheme.t;
  kernBR             : MathKernScheme.t;
  right_math_kind    : math_kind;
  last_height        : length;
  last_depth         : length;
}

type low_math_atom_main =
  | LowMathAtomGlyph of {
      info   : HorzBox.math_string_info;
      width  : length;
      height : length;
      depth  : length;
      output : OutputText.t;
    }
  | LowMathAtomEmbeddedInline of {
      content : HorzBox.horz_box list;
      height  : length; (* Can be gained from `content` *)
      depth   : length; (* Can be gained from `content` *)
    }

type low_math_atom = {
  atom_main       : low_math_atom_main;
  atom_left_kern  : left_kern;
  atom_right_kern : right_kern;
}

type low_paren = {
  lp_main             : horz_box list;
  lp_height           : length;
  lp_depth            : length;
  lp_math_kern_scheme : MathKernScheme.t;
}

type low_radical = horz_box list

type low_math_main =
  | LowMathAtom of {
      kind : math_kind;
      atom : low_math_atom;
    }
  | LowMathGroup of {
      left  : math_kind;
      right : math_kind;
      inner : low_math;
    }
  | LowMathSubscript of {
      sub_baseline_depth : length;
      base               : low_math;
      sub                : low_math;
    }
  | LowMathSuperscript of {
      sup_baseline_height : length;
      base                : low_math;
      sup                 : low_math;
    }
  | LowMathSubSuperscript of {
      sup_baseline_height : length;
      sub_baseline_depth  : length;
      base                : low_math;
      sup                 : low_math;
      sub                 : low_math;
    }
  | LowMathFraction of {
      numerator_baseline_height  : length;
      denominator_baseline_depth : length;
      numerator                  : low_math;
      denominator                : low_math;
    }
  | LowMathRadical of {
      radical       : low_radical;
      bar_height    : length;
      bar_thickness : length;
      inner         : low_math;
    }
  | LowMathParen of {
      left  : low_paren;
      right : low_paren;
      inner : low_math;
    }
  | LowMathParenWithMiddle of {
      left   : low_paren;
      right  : low_paren;
      middle : low_paren;
      inner  : low_math list;
    }
  | LowMathUpperLimit of {
      upper_baseline_height : length;
      base                  : low_math;
      upper                 : low_math;
    }
  | LowMathLowerLimit of {
      lower_baseline_depth : length;
      base                 : low_math;
      lower                : low_math;
    }

and low_math = {
  low_main       : low_math_main list;
  low_height     : length;
  low_depth      : length;
  low_left_kern  : left_kern;
  low_right_kern : right_kern;
}

type space_correction =
  | NoSpace
  | ItalicsCorrection of length
  | SpaceAfterScript


let no_left_kern hgt dpt mk =
  {
    kernTL         = MathKernScheme.zero;
    kernBL         = MathKernScheme.zero;
    left_math_kind = mk;
    first_height   = hgt;
    first_depth    = dpt;
  }


let no_right_kern hgt dpt mk =
  {
    italics_correction = Length.zero;
    kernTR             = MathKernScheme.zero;
    kernBR             = MathKernScheme.zero;
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
          kernTL         = MathKernScheme.make_discrete mki.FontFormat.kernTL;
          kernBL         = MathKernScheme.make_discrete mki.FontFormat.kernBL;
          left_math_kind = mk;
          first_height   = hgt;
          first_depth    = dpt;
        }

    | MathKernInfo(None) ->
        no_left_kern hgt dpt mk

    | MathKernFunc(kernfL, _) ->
        let kernL = MathKernScheme.make_dense kernfL in
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
          kernTR             = MathKernScheme.make_discrete mki.FontFormat.kernTR;
          kernBR             = MathKernScheme.make_discrete mki.FontFormat.kernBR;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | MathKernInfo(None) ->
        {
          italics_correction = mic;
          kernTR             = MathKernScheme.zero;
          kernBR             = MathKernScheme.zero;
          right_math_kind    = mk;
          last_height        = hgt;
          last_depth         = dpt;
        }

    | MathKernFunc(_, kernfR) ->
        let kernR = MathKernScheme.make_dense kernfR in
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
  HorzPure(PHSFixedEmpty{ width = wid })


let outer_empty natural shrinkable stretchable =
  HorzPure(PHSOuterEmpty{ natural; shrinkable; stretchable })


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
        | MathEnd
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


let space_after_script ictx =
  let fontsize = Context.font_size ictx in
  if not (Context.is_in_base_level ictx) then
    None
  else
    let mc = Context.get_math_constants ictx in
    Some(outer_empty (fontsize *% mc.FontFormat.space_after_script) Length.zero Length.zero)
      (* TODO: should have variable stretchability and shrinkability *)


let space_between_math_kinds (ictx : input_context) ~prev:(mk_prev : math_kind) (corr : space_correction) (mk : math_kind) : horz_box option =
  let is_in_script = not (Context.is_in_base_level ictx) in
  let (ctx, _) = ictx in
  let fontsize = Context.font_size ictx in
    if is_in_script then
      match (mk_prev, mk) with
      | (MathOperator, MathOrdinary)
      | (MathOrdinary, MathOperator)
      | (MathOperator, MathOperator)
      | (MathClose   , MathOperator)
      | (MathInner   , MathOperator) ->
          space_ord_op ctx fontsize

      | _ ->
          None
    else
      match (mk_prev, mk) with
      | (MathPunct   , _           ) ->
          space_punct ctx fontsize

      | (MathInner   , MathOrdinary)
      | (MathInner   , MathOpen    )
      | (MathInner   , MathPunct   )
      | (MathInner   , MathInner   )
      | (MathOrdinary, MathInner   )
      | (MathPrefix  , MathInner   )
      | (MathClose   , MathInner   ) ->
          space_ord_inner ctx fontsize

      | (_           , MathClose   ) ->
          begin
            match corr with
            | NoSpace                     -> None
            | SpaceAfterScript            -> None
            | ItalicsCorrection(italcorr) -> Some(fixed_empty italcorr)
          end

      | (MathOrdinary, MathOpen    )
      | (MathPrefix  , MathOpen    ) ->
          begin
            match corr with
            | NoSpace                     -> None
            | SpaceAfterScript            -> space_after_script ictx
            | ItalicsCorrection(italcorr) -> Some(fixed_empty italcorr)
          end

      | (MathBinary  , MathOrdinary)
      | (MathBinary  , MathPrefix  )
      | (MathBinary  , MathOperator)
      | (MathBinary  , MathOpen    )
      | (MathBinary  , MathInner   )
      | (MathOrdinary, MathBinary  )
      | (MathClose   , MathBinary  )
      | (MathInner   , MathBinary  ) ->
          space_ord_bin ctx fontsize

      | (MathRelation, MathOrdinary)
      | (MathRelation, MathOperator)
      | (MathRelation, MathInner   )
      | (MathRelation, MathOpen    )
      | (MathRelation, MathPrefix  )
      | (MathOrdinary, MathRelation)
      | (MathOperator, MathRelation)
      | (MathInner   , MathRelation)
      | (MathClose   , MathRelation) ->
          space_ord_rel ctx fontsize

      | (MathOperator, MathOrdinary)
      | (MathOperator, MathOperator)
      | (MathOperator, MathInner   )
      | (MathOperator, MathPrefix  )
      | (MathOrdinary, MathOperator)
      | (MathClose   , MathOperator)
      | (MathInner   , MathOperator) ->
          space_ord_op ctx fontsize

      | (MathOrdinary, MathPrefix  )
      | (MathInner   , MathPrefix  ) ->
          space_ord_prefix ctx fontsize

      | (_           , MathEnd     )
      | (MathEnd     , _           ) ->
          None

      | _ ->
          begin
            match corr with
            | NoSpace              -> None
            | SpaceAfterScript     -> space_after_script ictx
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
  | LowMathAtom{ atom; _ } ->
      atom.atom_left_kern

  | LowMathGroup{ left; _ } ->
      nokernf left

  | LowMathSubscript{ base = lm; _ }
  | LowMathSuperscript{ base = lm; _ }
  | LowMathSubSuperscript{ base = lm; _ }
  | LowMathUpperLimit{ base = lm; _ }
  | LowMathLowerLimit{ base = lm; _ } ->
      lm.low_left_kern

  | LowMathFraction(_)
  | LowMathRadical(_) ->
      nokernf MathInner

  | LowMathParen{ left; _ }
  | LowMathParenWithMiddle{ left; _ } ->
      make_left_paren_kern left.lp_height left.lp_depth left.lp_math_kern_scheme


let get_right_kern lmmain hgt dpt =
  let nokernf = no_right_kern hgt dpt in
  match lmmain with
  | LowMathAtom{ atom; _ } ->
      atom.atom_right_kern

  | LowMathGroup{ right; _ } ->
      nokernf right

  | LowMathSubscript{ base = lm; _ }
  | LowMathSuperscript{ base = lm; _ }
  | LowMathSubSuperscript{ base = lm; _ } ->
      nokernf lm.low_right_kern.right_math_kind

  | LowMathUpperLimit{ base = lm; _ }
  | LowMathLowerLimit{ base = lm; _ } ->
      lm.low_right_kern

  | LowMathFraction(_)
  | LowMathRadical(_) ->
      nokernf MathInner

  | LowMathParen{ right; _ }
  | LowMathParenWithMiddle{ right; _ } ->
      make_right_paren_kern right.lp_height right.lp_depth right.lp_math_kern_scheme


let rec get_left_math_kind : math_box -> math_kind = function
  | MathBoxAtom{ kind; _ } ->
      kind

  | MathBoxGroup{ left; _ } ->
      left

  | MathBoxSuperscript{ base; _ }
  | MathBoxSubscript{ base; _ }
  | MathBoxLowerLimit{ base; _ }
  | MathBoxUpperLimit{ base; _ } ->
      begin
        match base with
        | []         -> MathEnd
        | mathB :: _ -> get_left_math_kind mathB
      end

  | MathBoxFraction(_)
  | MathBoxRadical(_) ->
      MathInner

  | MathBoxParen(_)
  | MathBoxParenWithMiddle(_) ->
      MathOpen


let rec get_right_math_kind : math_box -> math_kind = function
  | MathBoxAtom{ kind; _ } ->
      kind

  | MathBoxGroup{ right; _ } ->
      right

  | MathBoxSuperscript{ base; _ }
  | MathBoxSubscript{ base; _ }
  | MathBoxLowerLimit{ base; _ }
  | MathBoxUpperLimit{ base; _ } ->
      begin
        match List.rev base with
        | []         -> MathEnd
        | mathB :: _ -> get_right_math_kind mathB
      end

  | MathBoxFraction(_)
  | MathBoxRadical(_) ->
      MathInner

  | MathBoxParen(_)
  | MathBoxParenWithMiddle(_) ->
      MathClose


let superscript_baseline_height ictx h_base d_sup =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let h_supbmin = fontsize *% mc.FontFormat.superscript_bottom_min in
  let h_supstd  = fontsize *% mc.FontFormat.superscript_shift_up in
  let l_supdmax = fontsize *% mc.FontFormat.superscript_baseline_drop_max in
  let cand = [h_supstd; h_base -% l_supdmax; h_supbmin +% (Length.negate d_sup)] in
  let h_supbl = cand |> List.fold_left Length.max Length.zero in
    h_supbl


(* Calculates the base correction height and the superscript correction height. *)
let superscript_correction_heights h_supbl h_base d_sup =
  let l_base = h_supbl +% d_sup in
  let l_sup = h_base -% h_supbl in
  (l_base, l_sup)


let subscript_baseline_depth ictx d_base h_sub =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let h_subtmax = fontsize *% mc.FontFormat.subscript_top_max in
  let d_substd  = Length.negate (fontsize *% mc.FontFormat.subscript_shift_down) in
  let l_subdmin = fontsize *% mc.FontFormat.subscript_baseline_drop_min in
  let cand = [Length.negate d_substd; (Length.negate d_base) +% l_subdmin; h_sub -% h_subtmax] in
  let d_subbl = Length.negate (cand |> List.fold_left Length.max Length.zero) in
    d_subbl


let subscript_correction_heights d_subbl d_base h_sub =
  let l_base = h_sub +% d_base in
  let l_sub = d_base -% d_subbl in
  (l_base, l_sub)


let correct_script_baseline_heights ictx d_sup h_sub h_supbl_raw d_subbl_raw =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let l_gapmin = fontsize *% mc.FontFormat.sub_superscript_gap_min in
  let l_gap = (h_supbl_raw +% d_sup) -% (d_subbl_raw +% h_sub) in
  if l_gap <% l_gapmin then
    let corr = (l_gapmin -% l_gap) *% 0.5 in
    (h_supbl_raw +% corr, d_subbl_raw -% corr)
  else
    (h_supbl_raw, d_subbl_raw)


let numerator_baseline_height ictx d_numer =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_numerstd    = fontsize *% mc.FontFormat.fraction_numer_d_shift_up in
  let l_numergapmin = fontsize *% mc.FontFormat.fraction_numer_d_gap_min in
  let h_numerbl = Length.max h_numerstd (h_bar +% t_bar *% 0.5 +% l_numergapmin +% (Length.negate d_numer)) in
    h_numerbl


let denominator_baseline_depth ictx h_denom =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let h_bar         = fontsize *% mc.FontFormat.axis_height in
  let t_bar         = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let d_denomstd    = Length.negate (fontsize *% mc.FontFormat.fraction_denom_d_shift_down) in
  let l_denomgapmin = fontsize *% mc.FontFormat.fraction_denom_d_gap_min in
  let d_denombl = Length.min d_denomstd (h_bar -% t_bar *% 0.5 -% l_denomgapmin -% h_denom) in
    d_denombl


let upper_limit_baseline_height ictx h_base d_up =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let l_upmingap = fontsize *% mc.FontFormat.upper_limit_gap_min in
  let l_upblstd = fontsize *% mc.FontFormat.upper_limit_baseline_rise_min in
  let h_upbl = Length.max (h_base +% l_upblstd) (h_base +% l_upmingap +% (Length.negate d_up)) in
    h_upbl


let lower_limit_baseline_depth ictx d_base h_low =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let l_lowmingap = fontsize *% mc.FontFormat.lower_limit_gap_min in
  let l_lowblstd = fontsize *% mc.FontFormat.lower_limit_baseline_drop_min in
  let d_lowbl = Length.min (d_base -% l_lowblstd) (d_base -% l_lowmingap -% h_low) in
    d_lowbl



(* --
   radical_bar_metrics:
     takes a math context, a script level, and the height of the contents,
     and then returns the height, the thickness, and the extra ascender of the raducal rule.
   -- *)
let radical_bar_metrics ictx h_cont =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
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
let radical_degree_baseline_height ictx scriptlev h_rad d_deg =
  let mc = FontInfo.get_math_constants ictx in
  let h_degb = h_rad *% mc.FontFormat.radical_degree_bottom in
  let h_degbl = h_degb +% d_deg in
    h_degbl
*)


let make_paren ictx (paren : paren) (height : length) (depth : length) =
  let (hbs, kernf) = paren height depth ictx in
  (hbs, MathKernScheme.make_dense kernf)


let make_radical ictx radical hgt_bar t_bar dpt =
  let fontsize = Context.font_size ictx in
  let hblst = radical hgt_bar t_bar dpt fontsize (Context.color ictx) in
    hblst


let convert_math_char (ictx : input_context) ~(kern : (math_char_kern_func * math_char_kern_func) option) ~(is_big : bool) (uchlst : Uchar.t list) (mk : math_kind) : low_math_atom =
  let mathstrinfo = Context.get_math_string_info ictx in
  let font_size = Context.font_size ictx in
  let is_in_base_level = Context.is_in_base_level ictx in
  let is_in_display = true (* temporary *) in
  let (otxt, wid, hgt, dpt, mic, mkiopt) =
    let mathkey = Context.math_font_key_exn ictx in
    FontInfo.get_math_char_info mathkey ~is_in_base_level ~is_in_display ~is_big ~font_size uchlst
  in
  let mkspec =
    match kern with
    | Some((kernfL, kernfR)) -> MathKernFunc(kernfL font_size, kernfR font_size) (* temporary *)
    | None                   -> (MathKernInfo(mkiopt))
  in
  let (lk, rk) = make_left_and_right_kern hgt dpt mk mic mkspec in
  let lma =
    LowMathAtomGlyph{
      info   = mathstrinfo;
      width  = wid;
      height = hgt;
      depth  = dpt;
      output = otxt;
    }
  in
  { atom_main = lma; atom_left_kern = lk; atom_right_kern = rk }


let get_height_and_depth_of_low_math_atom (lma : low_math_atom) : length * length =
  match lma.atom_main with
  | LowMathAtomGlyph{ height; depth; _ }          -> (height, depth)
  | LowMathAtomEmbeddedInline{ height; depth; _ } -> (height, depth)


let check_subscript (mlstB : math_box list) =
  match List.rev mlstB with
  | MathBoxSubscript{ base = mlstBB; sub = mlstT; _ } :: mtailrev ->
    (* If the last element of the base contents has a subscript *)
      let mlstBnew = List.rev_append mtailrev mlstBB in
      Some((mlstT, mlstBnew))

  | _ ->
      None


let convert_math_element (mk : math_kind) (ma : math_box_atom) : low_math_atom =
  match ma with
  | MathEmbeddedInline(ibs) ->
      let (_wid, hgt, dpt) = LineBreak.get_natural_metrics ibs in
      let lma = LowMathAtomEmbeddedInline{ content = ibs; height = hgt; depth = dpt } in
      {
        atom_main       = lma;
        atom_left_kern  = no_left_kern hgt dpt mk;
        atom_right_kern = no_right_kern hgt dpt mk;
      }

  | MathChar{ context = ictx; is_big; chars = uchs } ->
      convert_math_char ictx ~kern:None ~is_big uchs mk

  | MathCharWithKern{ context = ictx; is_big; chars = uchs; left_kern; right_kern } ->
      convert_math_char ictx ~kern:(Some((left_kern, right_kern))) ~is_big uchs mk


let rec convert_to_low ~prev:(mk_first : math_kind) ~next:(mk_last : math_kind) (ms : math_box list) : low_math =
  let optres =
    ms |> list_fold_adjacent (fun opt math math_prev_opt math_next_opt ->
      let mk_prev =
        match math_prev_opt with
        | None            -> mk_first
        | Some(math_prev) -> get_right_math_kind math_prev
      in
      let mk_next =
        match math_next_opt with
        | None            -> mk_last
        | Some(math_next) -> get_left_math_kind math_next
      in
        (* Get the rightward math class of the previous, and the leftward math class of the next *)
      let (lmmain, hgt, dpt) = convert_to_low_single ~prev:mk_prev ~next:mk_next math in
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
      {
        low_main       = [];
        low_height     = Length.zero;
        low_depth      = Length.zero;
        low_left_kern  = lk;
        low_right_kern = rk;
      }

  | Some((hgt, dpt, lkfirst, rklast, lmmainacc)) ->
      {
        low_main       = Alist.to_list lmmainacc;
        low_height     = hgt;
        low_depth      = dpt;
        low_left_kern  = lkfirst;
        low_right_kern = rklast;
      }


and convert_to_low_single ~prev:(mk_prev : math_kind) ~next:(mk_next : math_kind) (math : math_box) : low_math_main * length * length =
  match math with
  | MathBoxAtom{ kind = mk_raw; main = ma } ->
      let mk = normalize_math_kind mk_prev mk_next mk_raw in
      let lmatom = convert_math_element mk ma in
      let (hgt, dpt) = get_height_and_depth_of_low_math_atom lmatom in
      (LowMathAtom{ kind = mk; atom = lmatom }, hgt, dpt)

  | MathBoxGroup{ left = mkL; right = mkR; inner = mlstC } ->
      let lmC = convert_to_low ~prev:MathEnd ~next:MathClose mlstC in
      (LowMathGroup{ left = mkL; right = mkR; inner = lmC }, lmC.low_height, lmC.low_depth)

  | MathBoxFraction{ context = ictx; numerator = mlstN; denominator = mlstD } ->
      let lmN = convert_to_low ~prev:MathEnd ~next:MathEnd mlstN in
      let lmD = convert_to_low ~prev:MathEnd ~next:MathEnd mlstD in
      let h_numer = lmN.low_height in
      let d_numer = lmN.low_depth in
      let h_denom = lmD.low_height in
      let d_denom = lmD.low_depth in
      let ictx = ictx in
      let h_numerbl = numerator_baseline_height ictx d_numer in
      let d_denombl = denominator_baseline_depth ictx h_denom in
      let h_frac = h_numerbl +% h_numer in
      let d_frac = d_denombl +% d_denom in
      let lm =
        LowMathFraction{
          numerator_baseline_height  = h_numerbl;
          denominator_baseline_depth = d_denombl;
          numerator                  = lmN;
          denominator                = lmD;
        }
      in
      (lm, h_frac, d_frac)

  | MathBoxSubscript{ context = ictx; base = mlstB; sub = mlstS } ->
      let lmB = convert_to_low ~prev:mk_prev ~next:MathEnd mlstB in
      let lmS = convert_to_low ~prev:MathEnd ~next:MathEnd mlstS in
      let h_base = lmB.low_height in
      let d_base = lmB.low_depth in
      let rkB = lmB.low_right_kern in
      let h_sub = lmS.low_height in
      let d_sub = lmS.low_depth in
      let ictx = ictx in
      let d_subbl = subscript_baseline_depth ictx rkB.last_depth h_sub in
      let h_whole = h_base in (*TODO: take the height of subscripts into account *)
      let d_whole = Length.min d_base (d_subbl +% d_sub) in
      (LowMathSubscript{ sub_baseline_depth = d_subbl; base = lmB; sub = lmS }, h_whole, d_whole)

  | MathBoxSuperscript{ context = ictx; base = mlstB; sup = mlstS } ->
      begin
        match check_subscript mlstB with
        | Some((mlstT, mlstB)) ->
          (* If the last element of the base contents has a subscript *)
            let lmB = convert_to_low ~prev:mk_prev ~next:MathEnd mlstB in
            let lmS = convert_to_low ~prev:MathEnd ~next:MathEnd mlstS in
            let lmT = convert_to_low ~prev:MathEnd ~next:MathEnd mlstT in
            let h_base = lmB.low_height in
            let d_base = lmB.low_depth in
            let rkB = lmB.low_right_kern in
            let h_sup = lmS.low_height in
            let d_sup = lmS.low_depth in
            let h_sub = lmT.low_height in
            let d_sub = lmT.low_depth in
            let ictx = ictx in
            let h_supbl_raw = superscript_baseline_height ictx h_base d_sup in
            let d_subbl_raw = subscript_baseline_depth ictx rkB.last_depth h_sub in
            let (h_supbl, d_subbl) = correct_script_baseline_heights ictx d_sup h_sub h_supbl_raw d_subbl_raw in
            let h_whole = Length.max h_base (h_supbl +% h_sup) in
            let d_whole = Length.min d_base (d_subbl +% d_sub) in
            let lm =
              LowMathSubSuperscript{
                sup_baseline_height = h_supbl;
                sub_baseline_depth  = d_subbl;
                base                = lmB;
                sub                 = lmS;
                sup                 = lmT;
              }
            in
            (lm, h_whole, d_whole)

        | None ->
          (* If the last element of the base contents does NOT have a subscript *)
            let lmB = convert_to_low ~prev:mk_prev ~next:MathEnd mlstB in
            let lmS = convert_to_low ~prev:MathEnd ~next:MathEnd mlstS in
            let h_base = lmB.low_height in
            let d_base = lmB.low_depth in
            let h_sup = lmS.low_height in
            let d_sup = lmS.low_depth in
            let ictx = ictx in
            let h_supbl = superscript_baseline_height ictx h_base d_sup in
            let h_whole = Length.max h_base (h_supbl +% h_sup) in
            let d_whole = d_base in (*TODO: take the depth of superscripts into account *)
            (LowMathSuperscript{ sup_baseline_height = h_supbl; base = lmB; sup = lmS }, h_whole, d_whole)
      end

  | MathBoxRadical{ context = ictx; radical; degree = mlstD_opt; inner = mlstC } ->
      let ictx = ictx in
      let lmC = convert_to_low ~prev:MathEnd ~next:MathEnd mlstC in
      let h_inner = lmC.low_height in
      let d_inner = lmC.low_depth in
      let (h_bar, t_bar, l_extra) = radical_bar_metrics ictx h_inner in
      let hblstrad = make_radical ictx radical h_bar t_bar d_inner in
      let h_rad = h_bar +% t_bar in
      begin
        match mlstD_opt with
        | Some(_mlstD) ->
            failwith "TODO: unsupported; MathRadicalWithDegree"

        | None ->
            let h_whole = h_rad +% l_extra in
            let d_whole = d_inner in  (* TODO: consider the depth of the radical sign *)
            let lm =
              LowMathRadical{
                radical       = hblstrad;
                bar_height    = h_bar;
                bar_thickness = t_bar;
                inner         = lmC;
              }
            in
            (lm, h_whole, d_whole)
      end

  | MathBoxParen{ context = ictx; left = parenL; right = parenR; inner = mlstC } ->
      let lmC = convert_to_low ~prev:MathOpen ~next:MathClose mlstC in
      let h_inner = lmC.low_height in
      let d_inner = lmC.low_depth in
      let ictx = ictx in
      let (hblstparenL, mkernsL) = make_paren ictx parenL h_inner d_inner in
      let (hblstparenR, mkernsR) = make_paren ictx parenR h_inner d_inner in
      let (_, hL, dL)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, hR, dR) = LineBreak.get_natural_metrics hblstparenR in
      let h_whole = [hL; hR] |> List.fold_left Length.max h_inner in
      let d_whole = [dL; dR] |> List.fold_left Length.min d_inner in
      let lpL = { lp_main = hblstparenL; lp_height = hL; lp_depth = dL; lp_math_kern_scheme = mkernsL; } in
      let lpR = { lp_main = hblstparenR; lp_height = hR; lp_depth = dR; lp_math_kern_scheme = mkernsR; } in
      (LowMathParen{ left = lpL; right = lpR; inner = lmC }, h_whole, d_whole)

  | MathBoxParenWithMiddle{ context = ictx; left = parenL; right = parenR; middle; inner = mss } ->
      let lms = mss |> List.map (convert_to_low ~prev:MathOpen ~next:MathClose) in
      let (h_inner, d_inner) =
        lms |> List.fold_left (fun (hacc, dacc) lm ->
          (Length.max hacc lm.low_height, Length.min dacc lm.low_depth)
        ) (Length.zero, Length.zero)
      in
      let ictx = ictx in
      let (hblstparenL, mkernsL) = make_paren ictx parenL h_inner d_inner in
      let (hblstparenR, mkernsR) = make_paren ictx parenR h_inner d_inner in
      let (hblstmiddle, _) = make_paren ictx middle h_inner d_inner in
      let (_, hL, dL)   = LineBreak.get_natural_metrics hblstparenL in
      let (_, hR, dR) = LineBreak.get_natural_metrics hblstparenR in
      let (_, hM, dM) = LineBreak.get_natural_metrics hblstmiddle in
      let h_whole = [hL; hR; hM] |> List.fold_left Length.max h_inner in
      let d_whole = [dL; dR; dM] |> List.fold_left Length.min d_inner in
      let lpL = { lp_main = hblstparenL; lp_height = hL; lp_depth = dL; lp_math_kern_scheme = mkernsL; } in
      let lpR = { lp_main = hblstparenR; lp_height = hR; lp_depth = dR; lp_math_kern_scheme = mkernsR; } in
      let lpM = { lp_main = hblstmiddle; lp_height = hM; lp_depth = dM; lp_math_kern_scheme = MathKernScheme.zero; } in
      let lm =
        LowMathParenWithMiddle{
          left   = lpL;
          right  = lpR;
          middle = lpM;
          inner  = lms;
        }
      in
      (lm, h_whole, d_whole)

  | MathBoxUpperLimit{ context = ictx; base = mlstB; upper = mlstU } ->
      let lmB = convert_to_low ~prev:mk_prev ~next:mk_next mlstB in
      let lmU = convert_to_low ~prev:MathEnd ~next:MathEnd mlstU in
      let h_base = lmB.low_height in
      let d_base = lmB.low_depth in
      let h_upper = lmU.low_height in
      let d_upper = lmU.low_depth in
      let ictx = ictx in
      let h_upperbl = upper_limit_baseline_height ictx h_base d_upper in
      let h_whole = h_upperbl +% h_upper in
      let d_whole = d_base in
      (LowMathUpperLimit{ upper_baseline_height = h_upperbl; base = lmB; upper = lmU }, h_whole, d_whole)

  | MathBoxLowerLimit{ context = ictx; base = mlstB; lower = mlstL } ->
      let lmB = convert_to_low ~prev:mk_prev ~next:mk_next mlstB in
      let lmL = convert_to_low ~prev:MathEnd ~next:MathEnd mlstL in
      let h_base = lmB.low_height in
      let d_base = lmB.low_depth in
      let h_lower = lmL.low_height in
      let d_lower = lmL.low_depth in
      let ictx = ictx in
      let d_lowerbl = lower_limit_baseline_depth ictx d_base h_lower in
      let h_whole = h_base in
      let d_whole = d_lowerbl +% d_lower in
      (LowMathLowerLimit{ lower_baseline_depth = d_lowerbl; base = lmB; lower = lmL }, h_whole, d_whole)


let horz_of_low_math_element (lme : low_math_atom_main) : horz_box list =
  match lme with
  | LowMathAtomGlyph{ info; width; height; depth; output } ->
      [ HorzPure(PHCInnerMathGlyph{ info; width; height; depth; output }) ]

  | LowMathAtomEmbeddedInline{ content = hbs; _ } ->
      hbs


let horz_fraction_bar ictx wid =
  let fontsize = Context.font_size ictx in
  let mc = Context.get_math_constants ictx in
  let h_bar = fontsize *% mc.FontFormat.axis_height in
  let t_bar = fontsize *% mc.FontFormat.fraction_rule_thickness in
  let h_bart = h_bar +% t_bar *% 0.5 in
  let bar_color = Context.color ictx in
  let bar_graphics (xpos, ypos) =
    GraphicD.make_fill bar_color [ make_rectangle (xpos, ypos +% h_bart) wid t_bar ]
  in
  HorzPure(PHGFixedGraphics{ width = wid; height = h_bart; depth = Length.zero; graphics = bar_graphics })


let raise_horz r hbs =
  [ HorzPure(PHGRising{ rising = r; contents = hbs }) ]


let get_space_correction = function
  | LowMathAtom{ atom; _ } ->
      ItalicsCorrection(atom.atom_right_kern.italics_correction)

  | LowMathSubscript(_)
  | LowMathSuperscript(_)
  | LowMathSubSuperscript(_) ->
      SpaceAfterScript

  | _ ->
      NoSpace


let rec horz_of_low_math (ictx : input_context) ~prev:(mk_first : math_kind) ~next:(mk_last : math_kind) (lm : low_math) =
  let rec aux (hbacc : horz_box Alist.t) (mk_prev : math_kind) (corr : space_correction) lmmainlst =
    match lmmainlst with
    | [] ->
        let hb_space_opt = space_between_math_kinds ictx ~prev:mk_prev corr mk_last in
        begin
          match hb_space_opt with
          | None           -> Alist.to_list hbacc
          | Some(hb_space) -> Alist.to_list (Alist.extend hbacc hb_space)
              (* Appends italics correction for the last glyph of inner contents of a parenthesis *)
        end

    | lmmain :: lmmaintail ->
        let corrnext = get_space_correction lmmain in
        let (hblst, hbspaceopt, mk) =
          match lmmain with
          | LowMathAtom{ kind = mk; atom } ->
              let hblstpure = horz_of_low_math_element atom.atom_main in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr mk in
              (hblstpure, hbspaceopt, mk)

          | LowMathGroup{ left = mkL; right = mkR; inner = lmC } ->
              let hblstC = horz_of_low_math ictx ~prev:MathEnd ~next:MathClose lmC in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr mkL in
              (hblstC, hbspaceopt, mkR)

          | LowMathSuperscript{
              sup_baseline_height = h_supbl;
              base                = lmB;
              sup                 = lmS;
            } ->
              let lkB = lmB.low_left_kern in
              let rkB = lmB.low_right_kern in
              let d_sup = lmS.low_depth in
              let lkS = lmS.low_left_kern in
              let hblstB = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmB in
              let hblstS = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmS in
              let h_base = rkB.last_height in
              let (l_base, l_sup) = superscript_correction_heights h_supbl h_base d_sup in
              let l_kernbase = MathKernScheme.calculate ictx rkB.kernTR l_base in
              let l_kernsup  = MathKernScheme.calculate (Context.enter_script ictx) lkS.kernBL l_sup in
              let l_italic   = rkB.italics_correction in
              let kern = l_italic +% l_kernbase +% l_kernsup in
              let hbkern = fixed_empty kern in
              let hblstsup =
                List.concat [hblstB; [hbkern]; raise_horz h_supbl hblstS]
              in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr lkB.left_math_kind in
              (hblstsup, hbspaceopt, rkB.right_math_kind)

          | LowMathSubscript{
              sub_baseline_depth = d_subbl;
              base               = lmB;
              sub                = lmS;
            } ->
              let lkB = lmB.low_left_kern in
              let rkB = lmB.low_right_kern in
              let h_sub = lmS.low_height in
              let lkS = lmS.low_left_kern in
              let hblstB = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmB in
              let hblstS = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmS in
              let d_base = rkB.last_depth in
              let (l_base, l_sub) = subscript_correction_heights d_subbl d_base h_sub in
              let l_kernbase = MathKernScheme.calculate ictx rkB.kernBR l_base in
              let l_kernsub  = MathKernScheme.calculate ictx lkS.kernTL l_sub in
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
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr lkB.left_math_kind in
              (hblstsub, hbspaceopt, rkB.right_math_kind)

          | LowMathSubSuperscript{
              sup_baseline_height = h_supbl;
              sub_baseline_depth  = d_subbl;
              base                = lmB;
              sub                 = lmS;
              sup                 = lmT;
            } ->
              let lkB = lmB.low_left_kern in
              let rkB = lmB.low_right_kern in
              let d_sup = lmS.low_depth in
              let lkS = lmS.low_left_kern in
              let h_sub = lmT.low_height in
              let hblstB = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmB in
              let hblstS = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmS in
              let hblstT = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmT in
              let h_base = rkB.last_height in
              let d_base = rkB.last_depth in

              let (l_base_sup, l_sup) = superscript_correction_heights h_supbl h_base d_sup in
              let l_kernbase_sup = MathKernScheme.calculate ictx rkB.kernTR l_base_sup in
              let l_kernsup      = MathKernScheme.calculate (Context.enter_script ictx) lkS.kernBL l_sup in
              let l_italic       = rkB.italics_correction in
              let kernsup = l_italic +% l_kernbase_sup +% l_kernsup in
              let hbkernsup = fixed_empty kernsup in

              let (l_base_sub, l_sub) = subscript_correction_heights d_subbl d_base h_sub in
              let l_kernbase_sub = MathKernScheme.calculate ictx rkB.kernBR l_base_sub in
              let l_kernsub      = MathKernScheme.calculate ictx lkS.kernTL l_sub in
              let kernsub = (l_kernbase_sub +% l_kernsub) in
              let hbkernsub = fixed_empty kernsub in

              let (w_sup, _, _) = LineBreak.get_natural_metrics hblstS in
              let (w_sub, _, _) = LineBreak.get_natural_metrics hblstT in
              let foresup = kernsup +% w_sup in
              let foresub = kernsub +% w_sub in
              let hbbacksub = fixed_empty (Length.negate foresub) in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr lkB.left_math_kind in
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

          | LowMathFraction{
              numerator_baseline_height  = h_numerbl;
              denominator_baseline_depth = d_denombl;
              numerator                  = lmN;
              denominator                = lmD;
            } ->
              let hblstN = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmN in
              let hblstD = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmD in
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
              let hbbar = horz_fraction_bar ictx w_frac in
              let hblstsub = List.concat [raise_horz h_numerbl hblstNret; [hbback; hbbar; hbback]; raise_horz d_denombl hblstDret] in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr MathInner in
              (hblstsub, hbspaceopt, MathInner)

          | LowMathRadical{
              radical       = hblstrad;
              bar_height    = h_bar;
              bar_thickness = t_bar;
              inner         = lmC;
            } ->
              let hblstC = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmC in
              let (w_cont, _, _) = LineBreak.get_natural_metrics hblstC in
              let graphics (xpos, ypos) =
                GraphicD.make_fill (Context.color ictx)
                  [ make_rectangle (xpos, ypos +% h_bar) w_cont t_bar ]
              in
              let hbbar =
                HorzPure(PHGFixedGraphics{
                  width  = w_cont;
                  height = h_bar +% t_bar;
                  depth  = Length.zero;
                  graphics;
                })
              in
              let hbback = fixed_empty (Length.negate w_cont) in
              let hblstsub = List.append hblstrad (hbbar :: hbback :: hblstC) in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr MathInner in
              (hblstsub, hbspaceopt, MathInner)

          | LowMathParen{ left = lpL; right = lpR; inner = lmE } ->
              let hblstparenL = lpL.lp_main in
              let hblstparenR = lpR.lp_main in
              let hblstE = horz_of_low_math ictx ~prev:MathOpen ~next:MathClose lmE in
              let hblstsub = List.concat [hblstparenL; hblstE; hblstparenR] in
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr MathOpen in
              (hblstsub, hbspaceopt, MathClose)

          | LowMathParenWithMiddle{ left = lpL; right = lpR; middle = lpM; inner = lmlst } ->
              let hblstparenL = lpL.lp_main in
              let hblstparenR = lpR.lp_main in
              let hblstmiddle = lpM.lp_main in
              let hblstlst = List.map (horz_of_low_math ictx ~prev:MathOpen ~next:MathClose) lmlst in
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
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr MathOpen in
              (hblstsub, hbspaceopt, MathClose)

          | LowMathUpperLimit{ upper_baseline_height = h_upbl; base = lmB; upper = lmU } ->
              let lkB = lmB.low_left_kern in
              let rkB = lmB.low_right_kern in
              let hblstB = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmB in
                (* needs reconsideration *)
              let hblstU = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmU in
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
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr lkB.left_math_kind in
              (hblstsub, hbspaceopt, rkB.right_math_kind)

          | LowMathLowerLimit{ lower_baseline_depth = d_lowbl; base = lmB; lower = lmL } ->
              let lkB = lmB.low_left_kern in
              let rkB = lmB.low_right_kern in
              let hblstB = horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lmB in
                (* needs reconsideration *)
              let hblstL = horz_of_low_math (Context.enter_script ictx) ~prev:MathEnd ~next:MathEnd lmL in
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
              let hbspaceopt = space_between_math_kinds ictx ~prev:mk_prev corr lkB.left_math_kind in
              (hblstsub, hbspaceopt, rkB.right_math_kind)
        in
        let hbaccnew =
          match hbspaceopt with
          | None          -> Alist.append hbacc hblst
          | Some(hbspace) -> Alist.append (Alist.extend hbacc hbspace) hblst
        in
        aux hbaccnew mk corrnext lmmaintail

  in
  aux Alist.empty mk_first NoSpace lm.low_main


let main (ictx : input_context) (ms : math_box list) : horz_box list =
  let lms = convert_to_low ~prev:MathEnd ~next:MathEnd ms in
  horz_of_low_math ictx ~prev:MathEnd ~next:MathEnd lms


let space_between_maths (ictx : input_context) (mathlst1 : math_box list) (mathlst2 : math_box list) : horz_box option =
  match (List.rev mathlst1, mathlst2) with
  | (math1R :: _, math2L :: _) ->
      let mk1R = get_right_math_kind math1R in
      let mk2L = get_left_math_kind math2L in
      let lm1 = convert_to_low ~prev:MathEnd ~next:mk2L mathlst1 in
      let corr =
        match lm1.low_main with
        | lmmain :: _ -> get_space_correction lmmain
        | []          -> NoSpace
      in
      space_between_math_kinds ictx ~prev:mk1R corr mk2L

  | _ ->
      None
