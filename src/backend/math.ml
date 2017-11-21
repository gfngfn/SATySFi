
open HorzBox


type low_math_atom =
  | LowMathGlyph        of math_info * length * length * length * FontFormat.glyph_id
  | LowMathGraphics     of math_graphics
  | LowMathEmbeddedHorz of horz_box list

type left_kern =
  {
    kernTL             : FontFormat.math_kern option;
    kernBL             : FontFormat.math_kern option;
    left_math_kind     : math_kind option;
  }

type right_kern =
  {
    italics_correction : length;
    kernTR             : FontFormat.math_kern option;
    kernBR             : FontFormat.math_kern option;
    right_math_kind    : math_kind option;
  }

type low_math_element = math_kind * length * length * length * low_math_atom * left_kern * right_kern

type low_math_main =
  | LowMathPure        of low_math_element
  | LowMathFraction    of low_math * low_math
  | LowMathRadical     of low_math
  | LowMathSubscript   of low_math * low_math
  | LowMathSuperscript of low_math * low_math

and low_math = low_math_main list * left_kern * right_kern


let no_left_kern = { kernTL = None; kernBL = None; left_math_kind = None; }

let no_right_kern = { italics_correction = Length.zero; kernTR = None; kernBR = None; right_math_kind = None; }


let make_left_and_right_kern mkopt mic mkiopt : left_kern * right_kern =
  let lk =
    match mkiopt with
    | Some(mki) ->
        {
          kernTL         = Some(mki.FontFormat.kernTL);
          kernBL         = Some(mki.FontFormat.kernBL);
          left_math_kind = mkopt;
        }

    | None -> no_left_kern
  in
  let rk =
    match mkiopt with
    | Some(mki) ->
        {
          italics_correction = mic;
          kernTR             = Some(mki.FontFormat.kernTR);
          kernBR             = Some(mki.FontFormat.kernBR);
          right_math_kind    = mkopt;
        }

    | None -> no_right_kern
  in
    (lk, rk)


let convert_math_element (scriptlev : int) ((mk, memain) : math_element) : low_math_element =
  match memain with
  | MathGraphics(g) ->
      (mk, Length.zero (* temporary *), Length.zero (* temporary *), Length.zero (*temporary *), LowMathGraphics(g), no_left_kern, no_right_kern)

  | MathEmbeddedHorz(hblst) ->
      let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
        (mk, wid, hgt, dpt, LowMathEmbeddedHorz(hblst), no_left_kern, no_right_kern)

  | MathChar(mathinfo, uch) ->
      let (gid, wid, hgt, dpt, mic, mkiopt) = FontInfo.get_math_char_info mathinfo uch in
        (* temporary; choosing glyph ID should depends on the script level. *)
      let (lk, rk) = make_left_and_right_kern (Some(mk)) mic mkiopt in
        (mk, wid, hgt, dpt, LowMathGlyph(mathinfo, wid, hgt, dpt, gid), lk, rk)


let get_right_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, _, rk) -> rk
  | _                                 -> no_right_kern


let get_left_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, _, _, lk, _) -> lk
  | LowMathSubscript((_, lk, _), _)   -> lk
  | LowMathSuperscript((_, lk, _), _) -> lk
  | _                                 -> no_left_kern


let rec convert_to_low (scriptlev : int) (mlst : math list) : low_math =
  let optres =
    mlst |> List.fold_left (fun opt math ->
      let lmmain = convert_to_low_single scriptlev math in
      let rk = get_right_kern lmmain in
      let lk = get_left_kern lmmain in
      match opt with
      | None                      -> Some((lk, rk, lmmain :: []))
      | Some((lkfirst, _, lmacc)) -> Some((lkfirst, rk, lmmain :: lmacc))
    ) None
  in
  match optres with
  | None                               -> ([], no_left_kern, no_right_kern)
  | Some((lkfirst, rklast, lmmainacc)) -> (List.rev lmmainacc, lkfirst, rklast)


and convert_to_low_single (scriptlev : int) (math : math) : low_math_main =
  match math with
  | MathPure(me) ->
      let (mk, wid, hgt, dpt, lme, lk, rk) = convert_math_element scriptlev me in
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


let horz_of_low_math_element (mathinfo : math_info) (lme : low_math_atom) : horz_box list =
  match lme with
  | LowMathGlyph(mathinfo, wid, hgt, dpt, gid) -> [HorzPure(PHFixedMathGlyph(mathinfo, wid, hgt, dpt, gid))]
  | LowMathGraphics(g)                         -> []  (* temporary *)
  | LowMathEmbeddedHorz(hblst)                 -> hblst



let space_between_math_atom (prevmkopt : math_kind option) (mkopt : math_kind option) : horz_box =
  HorzPure(PHOuterEmpty(Length.zero, Length.zero, Length.zero))
    (* temporary; should decide width of a space based on 'prevmkopt' and 'mk' *)


let ratioize n =
  (float_of_int n) /. 1000.


(* -- calculates the base correction height and the superscript correction height-- *)
let superscript_baseline_height fontsize md d_sup =
  let mc = FontFormat.get_math_constants md in
  let h_supbmin = fontsize *% (ratioize mc.FontFormat.superscript_bottom_min) in
  let h_supstd  = fontsize *% (ratioize mc.FontFormat.superscript_shift_up) in
  let h_supb = Length.max h_supstd (h_supbmin +% d_sup) in
    h_supb


let correction_heights h_supb h_base d_sup =
  let l_base = h_supb -% d_sup in
  let l_sup  = h_base -% h_supb in
    (l_base, l_sup)


let get_real_font_size scriptlev fontsize md =
  let mc = FontFormat.get_math_constants md in
  match scriptlev with
  | 0              -> fontsize
  | 1              -> fontsize *% mc.FontFormat.script_scale_down
  | t  when t >= 2 -> fontsize *% mc.FontFormat.script_script_scale_down
  | _              -> assert false


let kern_top_right rk ratio =
  match rk.kernTR with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let kern_bottom_left rk ratio =
  match rk.kernBL with
  | None        -> 0.
  | Some(mkern) -> FontFormat.find_kern_ratio mkern ratio


let rec horz_of_low_math (scriptlev : int) (mathinfo : math_info) (md : FontFormat.math_decoder) (lm : low_math) =
  let fontsize = mathinfo.math_font_size in
  let (lmmainlst, _, _) = lm in
  let rec aux hbacc prevmkopt lmmainlst =
    match lmmainlst with
    | [] -> List.rev hbacc

    | LowMathPure(mk, wid, hgt, dpt, lma, _, _) :: lmmaintail ->
        let hbspace = space_between_math_atom prevmkopt (Some(mk)) in
        let hbaccnew = List.rev_append (horz_of_low_math_element mathinfo lma) (hbspace :: hbacc) in
          aux hbaccnew (Some(mk)) lmmaintail

    | LowMathSuperscript(lmB, lmS) :: lmmaintail ->
        let (_, lkB, rkB) = lmB in
        let (_, lkS, _) = lmS in
        let mkopt = lkB.left_math_kind in
        let hbspace = space_between_math_atom prevmkopt mkopt in
        let hblstB = horz_of_low_math scriptlev mathinfo md lmB in
        let hblstS = horz_of_low_math (scriptlev + 1) mathinfo md lmS in
        let (_, h_base, d_base) = LineBreak.get_natural_metrics hblstB in
        let (_, h_sup, d_sup) = LineBreak.get_natural_metrics hblstS in
        let h_supb = superscript_baseline_height fontsize md d_sup in
        let (l_base, l_sup) = correction_heights h_supb h_base d_sup in
        let s_base = get_real_font_size scriptlev fontsize md in
        let s_sup  = get_real_font_size (scriptlev + 1) fontsize md in
        let r_base = l_base /% s_base in
        let r_sup  = l_sup /% s_sup in
        let r_kernbase = kern_top_right rkB r_base in
        let r_kernsup  = kern_bottom_left lkS r_sup in
        let l_italic   = rkB.italics_correction in
        let l_kernbase = Length.min Length.zero (s_base *% r_kernbase) in
        let l_kernsup  = Length.min Length.zero (s_sup *% r_kernsup) in
        let kern = l_italic +% l_kernbase +% l_kernsup in
        let hbkern = HorzPure(PHFixedEmpty(kern)) in
        let hblstsup = List.concat [hblstB; [hbkern]; hblstS  (* temporary; should raise boxes *)] in
        let hbaccnew = List.rev_append hblstsup (hbspace :: hbacc) in
          aux hbaccnew mkopt lmmaintail

    | LowMathSubscript(lmB, lmS) :: lmmaintail ->
        failwith "unsupported"  (* temporary *)

    | LowMathRadical(lm1) :: lmmaintail ->
        failwith "unsupported"  (* temporary *)

    | LowMathFraction(lmN, lmD) :: lmmaintail ->
        failwith "unsupported"  (* temporary *)
  in
  aux [] None lmmainlst


(* for tests *)
let () =
  let mathinfo = { math_font_abbrev = "euler"; math_font_size = Length.of_pdf_point 12.; } in
  let md = FontFormat.get_math_decoder "/usr/local/lib-satysfi/dist/fonts/euler.otf" in
  let mlst = [MathPure(MathOrdinary, MathChar(mathinfo, Uchar.of_char 'a'))] in
  let lm = convert_to_low 0 mlst in
  let hblst = horz_of_low_math 0 mathinfo md lm in
  List.iter (pp_horz_box Format.std_formatter) hblst
