open HorzBox

let op_cm (xdiff, ydiff) =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate
                     (SkipLength.to_pdf_point xdiff, SkipLength.to_pdf_point ydiff)])

let op_Tm_translate (xpos, ypos) =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate
                     (SkipLength.to_pdf_point xpos, SkipLength.to_pdf_point ypos)])

let op_Tf tag sl = Pdfops.Op_Tf(tag, SkipLength.to_pdf_point sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET

let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)


let write_vert_lines (evvblst : evaled_vert_box list) : unit =
  let left_margin = SkipLength.of_pdf_point 50. in  (* temporary; should be variable *)
  let top_margin = SkipLength.of_pdf_point 700. in  (* temporary; should be variable *)
  let leading = SkipLength.of_pdf_point 32. in  (* temporary; should be variable *)
  let (_, opaccend) =
    evvblst @|> ((left_margin, top_margin), []) @|> List.fold_left (fun ((xpos, ypos), opacc) evvb ->
      match evvb with
      | EvVertLine(evhblst) ->
          let (xposend, opaccend) =
            evhblst @|> (xpos, opacc) @|> List.fold_left (fun (xpos, opacc) evhb ->
              let (widdiff, ops) =
                match evhb with
                | EvHorzOuterBoxAtom(wid, _) -> (wid, [])
                | EvHorzFixedBoxAtom(wid, FixedString((fontabrv, size), word)) ->
                    let tag = FontInfo.get_tag fontabrv in
                      (wid, [
                        op_Tm_translate (xpos, ypos);
                        op_Tf tag size;
                        op_Tj word;
                      ])
              in
              let opaccnew = List.rev_append ops opacc in
                (xpos +% widdiff, opaccnew)
            )
      in
        ((left_margin, ypos -% leading), opaccend)
    )
  in

  let oplst = op_cm (SkipLength.zero, SkipLength.zero) :: op_BT :: (List.rev (op_ET :: opaccend)) in

  let pdfinit = Pdf.empty () in

  let fontdict = FontInfo.get_font_dictionary pdfinit () in

  let page =
    {(Pdfpage.blankpage Pdfpaper.a4) with
        Pdfpage.content = [Pdfops.stream_of_ops oplst];
        Pdfpage.resources = Pdf.Dictionary [("/Font", Pdf.Dictionary fontdict)]}
  in
  let (pdfsub, pageroot) = Pdfpage.add_pagetree [page] pdfinit in
  let pdf = Pdfpage.add_root pageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdf "hello.pdf"
