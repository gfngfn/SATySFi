open HorzBox


let font =
  Pdf.Dictionary [
    ("/Type", Pdf.Name "/Font");
    ("/Subtype", Pdf.Name "/Type1");
    ("/BaseFont", Pdf.Name "/Times-Italic");
  ]

let write_vert_lines (evvblst : evaled_vert_box list) : unit =
  let left_margin = SkipLength.of_pdf_point 50. in
  let top_margin = SkipLength.of_pdf_point (700.) in
  let (_, opaccend) =
    evvblst |> List.fold_left (fun ((xpos, ypos), opacc) (EvVertLine(evhblst)) ->
      let (xposend, opaccend) =
        evhblst |> List.fold_left (fun (xpos, opacc) evhb ->
          let (widdiff, ops) =
            match evhb with
            | EvHorzFixedBoxAtom(wid, FixedString(_ (* temporary; should use font_info *), str)) ->
                (wid, [
                  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (0., 0.)]);
                  Pdfops.Op_BT;
                  Pdfops.Op_Tm(Pdftransform.matrix_of_transform [Pdftransform.Translate (SkipLength.to_pdf_point xpos, SkipLength.to_pdf_point ypos)]);
                  Pdfops.Op_Tf("/F0", 16.); (* temporary; should use font_info *)
                  Pdfops.Op_Tj(str);
                  Pdfops.Op_ET;
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

  let oplst = List.rev opaccend in

  let page =
    {(Pdfpage.blankpage Pdfpaper.a4) with
        Pdfpage.content = [Pdfops.stream_of_ops oplst];
        Pdfpage.resources = Pdf.Dictionary [("/Font", Pdf.Dictionary [("/F0", font)])]}
  in
  let (pdfsub, pageroot) = Pdfpage.add_pagetree [page] (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdf "hello.pdf"

(*
let ops =
  [Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (50., 770.)]);
   Pdfops.Op_BT;
   Pdfops.Op_Tf ("/F0", 32.);
   Pdfops.Op_Tj "Hello, World!";
   Pdfops.Op_ET]
*)
