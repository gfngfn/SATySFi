
(* for test *)
let print_for_debug msgln = ()


open HorzBox


type t = Pdf.t * Pdfpage.t list * file_path


let left_margin = Length.of_pdf_point 75.   (* temporary; should be variable *)
let top_margin = Length.of_pdf_point 100.   (* temporary; should be variable *)
let leading = Length.of_pdf_point 32.       (* temporary; should be variable *)


let get_paper_height (paper : Pdfpaper.t) : length =
  let dpi = 300. in  (* temporary; should be variable *)
  let pdfpt = Pdfunits.convert dpi (Pdfpaper.unit paper) Pdfunits.PdfPoint (Pdfpaper.height paper) in
    Length.of_pdf_point pdfpt


let rec operators_of_evaled_horz_box yposbaseline hgt dpt (xpos, opacc) evhb =
    match evhb with
    | EvHorz(wid, EvHorzEmpty) -> (xpos +% wid, opacc)
    | EvHorz(wid, EvHorzFrame(hgt_frame, dpt_frame, deco, evhblst)) ->
        let (xposnew, opaccsub) =
          evhblst @|> (xpos, opacc) @|> List.fold_left (operators_of_evaled_horz_box yposbaseline hgt dpt)
        in
        let ops =
          List.append
            (Graphics.op_q :: (Graphics.pdfops_of_path_list (deco (xpos, yposbaseline) wid hgt_frame dpt_frame)))
            (Graphics.op_Q :: [])
        in
        let opaccnew = List.rev_append ops opaccsub in
          (xposnew, opaccnew)

    | EvHorz(wid, EvHorzString((fontabrv, size), otxt)) ->
        let (tag, enc) = FontInfo.get_tag_and_encoding fontabrv in
        let opword = Graphics.op_TJ (OutputText.to_TJ_argument otxt) in
        let ops =
          [
(*
            (* begin: for test; encloses every word with a red box *)
            op_q;
            op_RG (1.0, 0.5, 0.5);
            op_m (xpos, yposbaseline);
            op_l (xpos +% wid, yposbaseline);
            op_re (xpos, yposbaseline +% hgt) (wid, Length.zero -% (hgt -% dpt));
            op_S;
            op_Q;
            (* end: for test *)
*)
            Graphics.op_cm (Length.zero, Length.zero);
            Graphics.op_BT;
            Graphics.op_Tm_translate (xpos, yposbaseline);
            Graphics.op_Tf tag size;
            opword;
            Graphics.op_ET;
          ]
        in
        let opaccnew = List.rev_append ops opacc in
          (xpos +% wid, opaccnew)


let write_page (paper : Pdfpaper.t) (evvblst : evaled_vert_box list) ((pdf, pageacc, flnm) : t) : t =
  let xinit = left_margin in
  let yinit = (get_paper_height paper) -% top_margin in
  let (_, opaccend) =
    evvblst @|> ((xinit, yinit), []) @|> List.fold_left (fun ((xpos, ypos), opacc) evvb ->
      match evvb with
      | EvVertFixedEmpty(vskip)       -> ((left_margin, ypos -% vskip), opacc)
      | EvVertLine(hgt, dpt, evhblst) ->
          let yposbaseline = ypos -% hgt in
          let (xposend, opaccend) =
            evhblst @|> (xpos, opacc) @|> List.fold_left (operators_of_evaled_horz_box yposbaseline hgt dpt)
          in
            ((left_margin, yposbaseline -% dpt), opaccend)
    )
  in

  let oplst = List.rev opaccend in

  let pagenew =
    { (Pdfpage.blankpage paper) with
        Pdfpage.content = [Pdfops.stream_of_ops oplst];
    }
  in
    (pdf, pagenew :: pageacc, flnm)


let create_empty_pdf (flnm : file_path) : t =
  let pdf = Pdf.empty () in
    (pdf, [], flnm)


let write_to_file ((pdf, pageacc, flnm) : t) : unit =
  let fontdict = FontInfo.get_font_dictionary pdf in
  let irfontdict =
    Pdf.addobj pdf (Pdf.Dictionary[("/Font", fontdict)])
  in
  let pagelst =
    pageacc |> List.rev |> List.map (fun page ->
      { page with Pdfpage.resources = Pdf.Indirect(irfontdict); }
    )
  in
  let (pdfsub, irpageroot) = Pdfpage.add_pagetree pagelst pdf in
  let pdfout = Pdfpage.add_root irpageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdfout flnm
