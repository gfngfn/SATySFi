
open HorzBox


type low_math_element_main =
  | LowMathGlyph        of math_info * FontFormat.glyph_id
  | LowMathGraphics     of math_graphics
  | LowMathEmbeddedHorz of horz_box list

type left_kern =
  {
    kernTL             : FontFormat.math_kern option;
    kernBL             : FontFormat.math_kern option;
  }

type right_kern =
  {
    italics_correction : length option;
    kernTR             : FontFormat.math_kern option;
    kernBR             : FontFormat.math_kern option;
  }

type low_math_element = math_kind * low_math_element_main

type low_math_main =
  | LowMathPure        of low_math_element
  | LowMathFraction    of low_math * low_math
  | LowMathRadical     of low_math
  | LowMathSubscript   of low_math * low_math
  | LowMathSuperscript of low_math * low_math

and low_math = low_math_main list * left_kern * right_kern


let no_left_kern = { kernTL = None; kernBL = None; }

let no_right_kern = { italics_correction = None; kernTR = None; kernBR = None; }


let get_left_and_right_kern micopt mkiopt : left_kern * right_kern =
  let lk =
    match mkiopt with
    | Some(mki) ->
        {
          kernTL = Some(mki.FontFormat.kernTL);
          kernBL = Some(mki.FontFormat.kernBL);
        }

    | None -> no_left_kern
  in
  let rk =
    match mkiopt with
    | Some(mki) ->
        {
          italics_correction = micopt;
          kernTR             = Some(mki.FontFormat.kernTR);
          kernBR             = Some(mki.FontFormat.kernBR);
        }

    | None -> no_right_kern
  in
    (lk, rk)


let convert_math_element (scriptlev : int) ((mk, memain) : math_element) : low_math_element * left_kern * right_kern =
  match memain with
  | MathGraphics(g)         -> ((mk, LowMathGraphics(g)), no_left_kern, no_right_kern)
  | MathEmbeddedHorz(hblst) -> ((mk, LowMathEmbeddedHorz(hblst)), no_left_kern, no_right_kern)
  | MathChar(mathinfo, uch) ->
      let (gid, wid, hgt, dpt, micopt, mkiopt) = FontInfo.get_math_char_info mathinfo uch in
      let (lk, rk) = get_left_and_right_kern micopt mkiopt in
        ((mk, LowMathGlyph(mathinfo, gid)), lk, rk)

(*
let convert_math_element_list (scriptlev : int) (melst : math_element list)
    : (correction_info * low_math_element list) =
  let lmelst = melst |> List.map convert_math_element in
  let (micopt, kernoptTR, kernoptBR) =
    match List.rev lmelst with
    | []                                     -> (None, None, None)
    | (_, _, micoptlast, None) :: _          -> (micoptlast, None, None)
    | (_, _, micoptlast, Some(mkilast)) :: _ -> (micoptlast, Some(mkilast.FontFormat.kernTR), Some(mkilast.FontFormat.kernBR))
  in
  let (kernoptTL, kernoptBL) =
    match lmelst with
    | []                             -> (None, None)
    | (_, _, _, None) :: _           -> (None, None)
    | (_, _, _, Some(mkifirst)) :: _ -> (Some(mkifirst.FontFormat.kernTL), Some(mkifirst.FontFormat.kernBL))
  in
  let corrinfo =
    {
      italics_correction = micopt;
      kernTL             = kernoptTL;
      kernBL             = kernoptBL;
      kernTR             = kernoptTR;
      kernBR             = kernoptBR;
    }
  in
    (corrinfo, lmelst)
*)

(*
let update_left_kern lkernopt kernoptTL kernoptBL =
  match lkernopt with
  | None    -> Some({ kernTL = kernoptTL; kernBL = kernoptBL; })
  | Some(_) -> lkernopt
*)


let rec convert (scriptlev : int) (mlst : math list) : low_math =
  let optres =
    mlst |> List.fold_left (fun opt math ->
      let (lmmain, lk, rk) = convert_single scriptlev math in
      match opt with
      | None                      -> Some((lk, rk, lmmain :: []))
      | Some((lkfirst, _, lmacc)) -> Some((lkfirst, rk, lmmain :: lmacc))
    ) None
  in
  match optres with
  | None                               -> ([], no_left_kern, no_right_kern)
  | Some((lkfirst, rklast, lmmainacc)) -> (List.rev lmmainacc, lkfirst, rklast)


and convert_single (scriptlev : int) (math : math) =
  match math with
  | MathPure(me) ->
      let ((mk, lme), lk, rk) = convert_math_element scriptlev me in
      (LowMathPure(mk, lme), lk, rk)

  | MathFraction(mlstN, mlstD) ->
      let lmN = convert scriptlev mlstN in
      let lmD = convert scriptlev mlstD in
        (LowMathFraction(lmN, lmD), no_left_kern, no_right_kern)

  | MathSubscript(mlst1, mlst2) ->
      let lmB = convert scriptlev mlst1 in
      let (_, lkB, _) = lmB in
      let lmS = convert (scriptlev + 1) mlst2 in
        (LowMathSubscript(lmB, lmS), lkB, no_right_kern)

  | MathSuperscript(mlst1, mlst2) ->
      let lmB = convert scriptlev mlst1 in
      let (_, lkB, _) = lmB in
      let lmS = convert (scriptlev + 1) mlst2 in
        (LowMathSuperscript(lmB, lmS), lkB, no_right_kern)

  | MathRadical(mlst1) ->
      let lm1 = convert scriptlev mlst1 in
        (LowMathRadical(lm1), no_left_kern, no_right_kern)
