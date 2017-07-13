open HorzBox

let op_cm (xdiff, ydiff) = Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (SkipLength.to_pdf_point xdiff, SkipLength.to_pdf_point ydiff)])
let op_Tm_translate (xpos, ypos) = Pdfops.Op_Tm(Pdftransform.matrix_of_transform [Pdftransform.Translate (SkipLength.to_pdf_point xpos, SkipLength.to_pdf_point ypos)])
let op_Tf tag sl = Pdfops.Op_Tf(tag, SkipLength.to_pdf_point sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET


let write_vert_lines (evvblst : evaled_vert_box list) : unit =
  let left_margin = SkipLength.of_pdf_point 50. in
  let top_margin = SkipLength.of_pdf_point (700.) in
  let (_, opaccend) =
    evvblst |> List.fold_left (fun ((xpos, ypos), opacc) (EvVertLine(evhblst)) ->
      let (xposend, opaccend) =
        evhblst |> List.fold_left (fun (xpos, opacc) evhb ->
          let (widdiff, ops) =
            match evhb with
            | EvHorzFixedBoxAtom(wid, FixedString((fntabrv, size), word)) ->
                let tag = FontInfo.get_tag fntabrv in
                  (wid, [
                    op_Tm_translate (xpos, ypos);
                    op_Tf tag size;
                    op_Tj word;
                  ])
            | EvHorzOuterBoxAtom(wid, _) -> (wid, [])
          in
          let opaccnew = List.rev_append ops opacc in
            (xpos +% widdiff, opaccnew)
        ) (xpos, opacc)
      in
        ((left_margin, ypos -% SkipLength.of_pdf_point 32. (* temporary; leading *)), opaccend)
    ) ((left_margin, top_margin), [])
  in

  let oplst = op_cm (SkipLength.zero, SkipLength.zero) :: op_BT :: (List.rev (op_ET :: opaccend)) in

  let page =
    {(Pdfpage.blankpage Pdfpaper.a4) with
        Pdfpage.content = [Pdfops.stream_of_ops oplst];
        Pdfpage.resources = Pdf.Dictionary [("/Font", Pdf.Dictionary (FontInfo.get_font_dictionary ()))]}
  in
  let (pdfsub, pageroot) = Pdfpage.add_pagetree [page] (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdf "hello.pdf"
