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


type t = Pdf.t * Pdfpage.t list * file_path * (string * Pdf.pdfobject) list


let left_margin = SkipLength.of_pdf_point 75.  (* temporary; should be variable *)
let top_margin = SkipLength.of_pdf_point 70.   (* temporary; should be variable *)
let leading = SkipLength.of_pdf_point 32.      (* temporary; should be variable *)


let get_paper_height (paper : Pdfpaper.t) : skip_height =
  let dpi = 300. in  (* temporary; should be variable *)
  let pdfpt = Pdfunits.convert dpi (Pdfpaper.unit paper) Pdfunits.PdfPoint (Pdfpaper.height paper) in
    SkipLength.of_pdf_point pdfpt


let write_page (paper : Pdfpaper.t) (evvblst : evaled_vert_box list) ((pdf, pageacc, flnm, fontdict) : t) : t =
  let xinit = left_margin in
  let yinit = (get_paper_height paper) -% top_margin in
  let (_, opaccend) =
    evvblst @|> ((xinit, yinit), []) @|> List.fold_left (fun ((xpos, ypos), opacc) evvb ->
      match evvb with
      | EvVertFixedEmpty(vskip)       -> ((left_margin, ypos -% vskip), opacc)
      | EvVertLine(hgt, dpt, evhblst) ->
          let yposbaseline = ypos -% hgt in
          let (xposend, opaccend) =
            evhblst @|> (xpos, opacc) @|> List.fold_left (fun (xpos, opacc) evhb ->
              let (widdiff, ops) =
                match evhb with
                | EvHorzOuterBoxAtom(wid, _) -> (wid, [])
                | EvHorzFixedBoxAtom(wid, FixedEmpty(_)) -> (wid, [])
                | EvHorzFixedBoxAtom(wid, FixedString((fontabrv, size), word)) ->
                    let tag = FontInfo.get_tag fontabrv in
                      (wid, [
                        op_Tm_translate (xpos, yposbaseline);
                        op_Tf tag size;
                        op_Tj word;
                      ])
              in
              let opaccnew = List.rev_append ops opacc in
                (xpos +% widdiff, opaccnew)
            )
          in
            ((left_margin, yposbaseline -% dpt), opaccend)
    )
  in

  let oplst = op_cm (SkipLength.zero, SkipLength.zero) :: op_BT :: (List.rev (op_ET :: opaccend)) in

  let pagenew =
    {(Pdfpage.blankpage paper) with
        Pdfpage.content= [Pdfops.stream_of_ops oplst];
        Pdfpage.resources = Pdf.Dictionary[("/Font", Pdf.Dictionary(fontdict))];
          (* temporary; currently adds same font resources to every page *)
    }
  in
    (pdf, pagenew :: pageacc, flnm, fontdict)


let create_empty_pdf (flnm : file_path) : t =
  let pdf = Pdf.empty () in
  let fontdict = FontInfo.get_font_dictionary pdf () in
    (pdf, [], flnm, fontdict)


let write_to_file ((pdf, pageacc, flnm, _) : t) : unit =
  let pagelst = List.rev pageacc in
  let (pdfsub, irpageroot) = Pdfpage.add_pagetree pagelst pdf in
  let pdfout = Pdfpage.add_root irpageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdfout flnm
