
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

type low_math_element = math_kind * low_math_element_main * left_kern * right_kern

type low_math_main =
  | LowMathPure        of low_math_element
  | LowMathFraction    of low_math * low_math
  | LowMathRadical     of low_math
  | LowMathSubscript   of low_math * low_math
  | LowMathSuperscript of low_math * low_math

and low_math = low_math_main list * left_kern * right_kern


let no_left_kern = { kernTL = None; kernBL = None; }

let no_right_kern = { italics_correction = None; kernTR = None; kernBR = None; }


let make_left_and_right_kern micopt mkiopt : left_kern * right_kern =
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


let convert_math_element (scriptlev : int) ((mk, memain) : math_element) : low_math_element =
  match memain with
  | MathGraphics(g)         -> (mk, LowMathGraphics(g), no_left_kern, no_right_kern)
  | MathEmbeddedHorz(hblst) -> (mk, LowMathEmbeddedHorz(hblst), no_left_kern, no_right_kern)
  | MathChar(mathinfo, uch) ->
      let (gid, wid, hgt, dpt, micopt, mkiopt) = FontInfo.get_math_char_info mathinfo uch in
        (* temporary; choosing glyph ID should depends on the script level. *)
      let (lk, rk) = make_left_and_right_kern micopt mkiopt in
        (mk, LowMathGlyph(mathinfo, gid), lk, rk)


let get_right_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, _, rk) -> rk
  | _                        -> no_right_kern


let get_left_kern lmmain =
  match lmmain with
  | LowMathPure(_, _, lk, _)          -> lk
  | LowMathSubscript((_, lk, _), _)   -> lk
  | LowMathSuperscript((_, lk, _), _) -> lk
  | _                                 -> no_left_kern


let rec convert (scriptlev : int) (mlst : math list) : low_math =
  let optres =
    mlst |> List.fold_left (fun opt math ->
      let lmmain = convert_single scriptlev math in
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


and convert_single (scriptlev : int) (math : math) : low_math_main =
  match math with
  | MathPure(me) ->
      let (mk, lme, lk, rk) = convert_math_element scriptlev me in
        LowMathPure(mk, lme, lk, rk)

  | MathFraction(mlstN, mlstD) ->
      let lmN = convert scriptlev mlstN in
      let lmD = convert scriptlev mlstD in
        LowMathFraction(lmN, lmD)

  | MathSubscript(mlst1, mlst2) ->
      let lmB = convert scriptlev mlst1 in
      let lmS = convert (scriptlev + 1) mlst2 in
        LowMathSubscript(lmB, lmS)

  | MathSuperscript(mlst1, mlst2) ->
      let lmB = convert scriptlev mlst1 in
      let lmS = convert (scriptlev + 1) mlst2 in
        LowMathSuperscript(lmB, lmS)

  | MathRadical(mlst1) ->
      let lm1 = convert scriptlev mlst1 in
        LowMathRadical(lm1)
