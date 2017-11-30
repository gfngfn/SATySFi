
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

type low_math_element = math_kind * length * length * length * low_math_atom * left_kern * right_kern

type low_math_main =
  | LowMathPure        of low_math_element
  | LowMathFraction    of low_math * low_math
  | LowMathRadical     of low_math
  | LowMathSubscript   of low_math * low_math
  | LowMathSuperscript of low_math * low_math

and low_math = low_math_main list * left_kern * right_kern


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
  | MathEnd
    -> mkraw

  | MathRelation
  | MathBinary
    ->
      match (mkprev, mknext) with
      | (MathEnd , _        )
      | (_       , MathEnd  )
      | (MathOpen, _        )
      | (_       , MathClose)
        -> MathOrdinary

      | _ -> mkraw


let convert_math_element (mkprev : math_kind) (mknext : math_kind) (scriptlev : int) ((mkraw, memain) : math_element) : low_math_element =
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


let get_right_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, _, rk) -> rk
  | LowMathSubscript((_, _, rk), _)   -> no_right_kern rk.right_math_kind
  | LowMathSuperscript((_, _, rk), _) -> no_right_kern rk.right_math_kind
  | _                                 -> no_right_kern MathEnd


let get_left_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, lk, _) -> lk
  | LowMathSubscript((_, lk, _), _)   -> lk
  | LowMathSuperscript((_, lk, _), _) -> lk
  | _                                 -> no_left_kern MathEnd


let get_left_math_kind mathopt =
  let rec aux = function
    | MathPure((mk, _))              -> mk
    | MathSuperscript([], _)         -> MathEnd
    | MathSuperscript(mathB :: _, _) -> aux mathB
    | MathSubscript([], _)           -> MathEnd
    | MathSubscript(mathB :: _, _)   -> aux mathB
    | MathFraction(_, _)             -> MathEnd
    | MathRadical(_)                 -> MathEnd
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
    | MathFraction(_, _)          -> MathEnd
    | MathRadical(_)              -> MathEnd
  in
  match mathopt with
  | None       -> MathEnd
  | Some(math) -> try aux math with Invalid_argument(_) -> assert false


let rec convert_to_low (scriptlev : int) (mlst : math list) : low_math =
  let optres =
    mlst |> Util.list_fold_adjacent (fun opt math mathprevopt mathnextopt ->
      let mkprev = get_left_math_kind mathprevopt in
      let mknext = get_right_math_kind mathnextopt in
      let lmmain = convert_to_low_single mkprev mknext scriptlev math in
      let rk = get_right_kern lmmain in
      let lk = get_left_kern lmmain in
      match opt with
      | None                      -> Some((lk, rk, lmmain :: []))
      | Some((lkfirst, _, lmacc)) -> Some((lkfirst, rk, lmmain :: lmacc))
    ) None
  in
  match optres with
  | None                               -> ([], no_left_kern MathEnd, no_right_kern MathEnd)
  | Some((lkfirst, rklast, lmmainacc)) -> (List.rev lmmainacc, lkfirst, rklast)


and convert_to_low_single (mkprev : math_kind) (mknext : math_kind) (scriptlev : int) (math : math) : low_math_main =
  match math with
  | MathPure(me) ->
      let (mk, wid, hgt, dpt, lme, lk, rk) = convert_math_element mkprev mknext scriptlev me in
        LowMathPure(mk, wid, hgt, dpt, lme, lk, rk)

  | MathFraction(mlstN, mlstD) ->
      let lmN = convert_to_low scriptlev mlstN in
      let lmD = convert_to_low scriptlev mlstD in
        LowMathFraction(lmN, lmD)

  | MathSubscript(mlst1, mlst2) ->
      let lmB = convert_to_low scriptlev mlst1 in
      let lmS = convert_to_low (scriptlev + 1) mlst2 in
        LowMathSubscript(lmB, lmS)

  | MathSuperscript(mlst1, mlst2) ->
      let lmB = convert_to_low scriptlev mlst1 in
      let lmS = convert_to_low (scriptlev + 1) mlst2 in
        LowMathSuperscript(lmB, lmS)

  | MathRadical(mlst1) ->
      let lm1 = convert_to_low scriptlev mlst1 in
        LowMathRadical(lm1)


let horz_of_low_math_element (lme : low_math_atom) : horz_box list =
  match lme with
  | LowMathGlyph(mathstrinfo, wid, hgt, dpt, gid) ->
      [HorzPure(PHFixedMathGlyph(mathstrinfo, wid, hgt, dpt, gid))]

  | LowMathGraphics(g) ->
      []  (* temporary *)

  | LowMathEmbeddedHorz(hblst) ->
      hblst


let space_ord_bin fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.25 (* temporary *), Length.zero, Length.zero)))


let space_ord_rel fontsize scriptlev =
  if scriptlev > 0 then None else
    Some(HorzPure(PHOuterEmpty(fontsize *% 0.5 (* temporary *), Length.zero, Length.zero)))


let space_between_math_atom (mathctx : math_context) (scriptlev : int) (mkprev : math_kind) (mk : math_kind) : horz_box option =
  let fontsize = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
    match (mkprev, mk) with
    | (MathBinary  , MathOrdinary) -> space_ord_bin fontsize scriptlev
    | (MathRelation, MathOrdinary) -> space_ord_rel fontsize scriptlev
    | (MathOrdinary, MathBinary  ) -> space_ord_bin fontsize scriptlev
    | (MathOrdinary, MathRelation) -> space_ord_rel fontsize scriptlev
    | (_,            _           ) -> None


let ratioize n =
  (float_of_int n) /. 1000.


let superscript_baseline_height (mathctx : math_context) (scriptlev : int) h_base d_sup =
  let fontsize = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
  let mc = FontInfo.get_math_constants mathctx in
  let h_supbmin = fontsize *% mc.FontFormat.superscript_bottom_min in
  let h_supstd  = fontsize *% mc.FontFormat.superscript_shift_up in
  let l_supdmax = fontsize *% mc.FontFormat.superscript_baseline_drop_max in
  let cand = [h_supstd; h_base -% l_supdmax; h_supbmin +% d_sup] in
  let h_supbl = cand |> List.fold_left Length.max Length.zero in
    h_supbl


(* -- calculates the base correction height and the superscript correction height-- *)
let superscript_correction_heights h_supbl h_base d_sup =
  let l_base = h_supbl -% d_sup in
  let l_sup = h_base -% h_supbl in
    (l_base, l_sup)


let subscript_baseline_depth (mathctx : math_context) (scriptlev : int) d_base h_sub =
  let fontsize = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
  let mc = FontInfo.get_math_constants mathctx in
  let h_subtmax = fontsize *% mc.FontFormat.subscript_top_max in
  let d_substd  = Length.negate (fontsize *% mc.FontFormat.subscript_shift_down) in
  let l_subdmin = fontsize *% mc.FontFormat.subscript_baseline_drop_min in
  let cand = [Length.negate d_substd; (Length.negate d_base) +% l_subdmin; h_sub -% h_subtmax] in
  let d_subbl = Length.negate (cand |> List.fold_left Length.max Length.zero) in
    d_subbl


let subscript_correction_heights d_subbl d_base h_sub =
  let l_base = d_base -% d_subbl in
  let l_sub = h_sub +% d_base in
    (l_base, l_sub)


let kern_top_right rk ratio =
  match rk.kernTR with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let kern_bottom_right rk ratio =
  match rk.kernBR with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let kern_top_left lk ratio =
  match lk.kernTL with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let kern_bottom_left rk ratio =
  match rk.kernBL with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let raise_as_script r hblst =
  [HorzPure(PHRising(r, hblst))]


let rec horz_of_low_math (mathctx : math_context) (scriptlev : int) (lm : low_math) =
  let (lmmainlst, _, _) = lm in
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

    | LowMathSuperscript(lmB, lmS) :: lmmaintail ->
        let (_, lkB, rkB) = lmB in
        let (_, lkS, _) = lmS in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev lkB.left_math_kind in
        let hblstB = horz_of_low_math mathctx scriptlev lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) lmS in
        let h_base = rkB.last_height in
        let (_, _, d_sup) = LineBreak.get_natural_metrics hblstS in
        let h_supbl = superscript_baseline_height mathctx scriptlev h_base d_sup in
        let (l_base, l_sup) = superscript_correction_heights h_supbl h_base d_sup in
        let s_base = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
        let s_sup  = (FontInfo.get_math_string_info (scriptlev + 1) mathctx).math_font_size in
        let r_base = l_base /% s_base in
        let r_sup  = l_sup /% s_sup in
        let r_kernbase = kern_top_right rkB r_base in
        let r_kernsup  = kern_bottom_left lkS r_sup in
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsup = %f, " r_kernsup;
        let l_italic   = rkB.italics_correction in
        Format.printf "l_italic = %f\n" (Length.to_pdf_point l_italic);
        let l_kernbase = Length.min Length.zero (s_base *% r_kernbase) in
        let l_kernsup  = Length.min Length.zero (s_sup *% r_kernsup) in
        let kern = l_italic +% l_kernbase +% l_kernsup in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsup = List.concat [hblstB; [hbkern]; raise_as_script h_supbl hblstS] in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsup hbacc
          | Some(hbspace) -> List.rev_append hblstsup (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind lmmaintail

    | LowMathSubscript(lmB, lmS) :: lmmaintail ->
        let (_, lkB, rkB) = lmB in
        let (_, lkS, _) = lmS in
        let hbspaceopt = space_between_math_atom mathctx scriptlev mkprev lkB.left_math_kind in
        let hblstB = horz_of_low_math mathctx scriptlev lmB in
        let hblstS = horz_of_low_math mathctx (scriptlev + 1) lmS in
        let d_base = rkB.last_depth in
        let (_, h_sub, _) = LineBreak.get_natural_metrics hblstS in
        let d_subbl = subscript_baseline_depth mathctx scriptlev d_base h_sub in
        let (l_base, l_sub) = subscript_correction_heights d_subbl d_base h_sub in
        let s_base = (FontInfo.get_math_string_info scriptlev mathctx).math_font_size in
        let s_sub  = (FontInfo.get_math_string_info (scriptlev + 1) mathctx).math_font_size in
        let r_base = l_base /% s_base in
        let r_sub  = l_sub /% s_sub in
        let r_kernbase = kern_bottom_right rkB r_base in
        let r_kernsub  = kern_top_left lkS r_sub in
        Format.printf "Math> r_kernbase = %f, " r_kernbase;
        Format.printf "r_kernsub = %f\n" r_kernsub;
        let l_kernbase = Length.min Length.zero (s_base *% r_kernbase) in
        let l_kernsub  = Length.min Length.zero (s_base *% r_kernsub) in
        let kern = l_kernbase +% l_kernsub in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsub = List.concat [hblstB; [hbkern]; raise_as_script d_subbl hblstS] in
        let hbaccnew =
          match hbspaceopt with
          | None          -> List.rev_append hblstsub hbacc
          | Some(hbspace) -> List.rev_append hblstsub (hbspace :: hbacc)
        in
          aux hbaccnew rkB.right_math_kind lmmaintail

    | LowMathRadical(lm1) :: lmmaintail ->
        failwith "unsupported"  (* temporary *)

    | LowMathFraction(lmN, lmD) :: lmmaintail ->
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
