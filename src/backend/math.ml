
open HorzBox


let convert_math_element_main (memain : math_element_main) : horz_box list =
  match memain with
  | MathGraphics(g)          -> []  (* temporary *)
  | MathEmbeddedHorz(hblst)  -> hblst
  | MathChar(mathinfo, uch) ->
      let (gid, wid, hgt, dpt, micopt, mkiopt) = FontInfo.get_math_char_info mathinfo.math_font_abbrev uch in
      []


let convert_math_element_list (scriptlev : int) (melst : math_element list) : horz_box list =
  let (_, hbacc) =
    melst |> List.fold_left (fun (prevmkopt, hbacc) (mk, memain) ->
      let hblst = convert_math_element_main memain in
        (Some(mk), List.rev_append hblst hbacc)
          (* temporary; should consider spaces based on prevmkopt and mk *)
    ) (None, [])
  in
    List.rev hbacc


let rec convert (scriptlev : int) (mlst : math list) : horz_box list =
  mlst |> List.map (convert_single scriptlev) |> List.concat


and convert_single (scriptlev : int) (math : math) : horz_box list =
  match math with
  | MathPure(melst) ->
      convert_math_element_list scriptlev melst

  | MathFraction(mlstN, mlstD) ->
      let hblstN = convert scriptlev mlstN in
      let hblstD = convert scriptlev mlstD in
        []  (* temporary *)

  | MathSubscript(mlst1, mlst2) ->
      let hblst1 = convert scriptlev mlst1 in
      let hblst2 = convert (scriptlev + 1) mlst2 in
        []  (* temporary *)

  | MathSuperscript(mlst1, mlst2) ->
      let hblst1 = convert scriptlev mlst1 in
      let hblst2 = convert (scriptlev + 1) mlst2 in
        []  (* temporary *)

  | MathRadical(mlst1) ->
      let hblst1 = convert scriptlev mlst1 in
        []  (* temporary *)
