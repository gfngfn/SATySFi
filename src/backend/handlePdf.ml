
open MyUtil
open LengthInterface
open HorzBox


type t = Pdf.t * Pdfpage.t list * file_path


let get_paper_height (paper : Pdfpaper.t) : length =
  let dpi = 300. in  (* temporary; should be variable *)
  let pdfpt = Pdfunits.convert dpi (Pdfpaper.unit paper) Pdfunits.PdfPoint (Pdfpaper.height paper) in
    Length.of_pdf_point pdfpt


let rec ops_of_evaled_horz_box yposbaseline (xpos, opacc) (evhb : evaled_horz_box) =
  let (wid, evhbmain) = evhb in
    match evhbmain with
    | EvHorzEmpty ->
        (xpos +% wid, opacc)

    | EvHorzFrame(hgt_frame, dpt_frame, deco, imhblst) ->
        let gr_background =
          deco (xpos, yposbaseline) wid hgt_frame dpt_frame
            (* -- depth values are nonpositive -- *)
        in
        let ops_foreground = [] in  (* temporary *)
        let opaccinit = Alist.append opacc (Graphics.to_pdfops gr_background) in
        let (xposnew, opaccsub) =
          imhblst @|> (xpos, opaccinit) @|> List.fold_left (ops_of_evaled_horz_box yposbaseline)
        in
        let opaccnew = Alist.append opaccsub ops_foreground in
          (xposnew, opaccnew)

    | EvHorzString(hsinfo, hgt, dpt, otxt) ->
        let tag = FontInfo.get_font_tag hsinfo.font_abbrev in
(*
        let opword = Graphics.op_TJ (OutputText.to_TJ_argument otxt) in
        let opcolor = Graphics.pdfop_of_text_color hsinfo.text_color in
*)
        let ops =
(*
          List.append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)  (* for test *)
*)
            (Graphics.pdfops_of_text (xpos, yposbaseline) hsinfo.rising tag hsinfo.text_font_size hsinfo.text_color otxt)
(*
          [
            Graphics.op_cm_translate (Length.zero, Length.zero);
            Graphics.op_q;
            opcolor;
            Graphics.op_BT;
            Graphics.op_Tm_translate (xpos, yposbaseline);
            Graphics.op_Tf tag hsinfo.text_font_size;
            Graphics.op_Ts hsinfo.rising;
            opword;
            Graphics.op_ET;
            Graphics.op_Q;
          ]
*)
        in
        let opaccnew = Alist.append opacc ops in
          (xpos +% wid, opaccnew)

    | EvHorzMathGlyph(msinfo, hgt, dpt, gid) ->
        let tag = FontInfo.get_math_tag msinfo.math_font_abbrev in
        let otxt = OutputText.append_glyph_id OutputText.empty_hex_style gid in
(*
        let opword = Graphics.op_TJ (OutputText.to_TJ_argument otxt) in
        let opcolor = Graphics.pdfop_of_text_color msinfo.math_color in
*)
        let ops =
(*
        List.append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
          (Graphics.pdfops_of_text (xpos, yposbaseline) Length.zero tag msinfo.math_font_size msinfo.math_color otxt)
(*
          [
            Graphics.op_cm_translate (Length.zero, Length.zero);
            Graphics.op_q;
            opcolor;
            Graphics.op_BT;
            Graphics.op_Tm_translate (xpos, yposbaseline);
            Graphics.op_Tf tag msinfo.math_font_size;
            opword;
            Graphics.op_ET;
            Graphics.op_Q;
          ]
*)
        in
        let opaccnew = Alist.append opacc ops in
        (xpos +% wid, opaccnew)

    | EvHorzRising(hgt, dpt, lenrising, evhblst) ->
        let (_, opaccsub) =
          evhblst |> List.fold_left (ops_of_evaled_horz_box (yposbaseline +% lenrising)) (xpos, opacc)
        in
        let opaccnew =
(*
          List.rev_append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
            opaccsub
        in
          (xpos +% wid, opaccnew)

    | EvHorzEmbeddedVert(hgt, dpt, evvblst) ->
        let ((_, _), opaccnew) = ops_of_evaled_vert_box_list (xpos, yposbaseline +% hgt) opacc evvblst in
          (xpos +% wid, opaccnew)

    | EvHorzInlineGraphics(hgt, dpt, graphics) ->
        let gr =
(*
          List.append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
          (graphics (xpos, yposbaseline))
        in
        let opaccnew = Alist.append opacc (Graphics.to_pdfops gr) in
          (xpos +% wid, opaccnew)

    | EvHorzInlineTabular(hgt, dpt, evtabular) ->
        let ops_tabular =
          ops_of_evaled_tabular (xpos, yposbaseline +% hgt) evtabular
        in
        let opaccnew = Alist.append opacc ops_tabular in
          (xpos +% wid, opaccnew)


    | EvHorzInlineImage(hgt, imgkey) ->
        let tag = ImageInfo.get_tag imgkey in
        let (xratio, yratio) = ImageInfo.get_ratio imgkey wid hgt in
        let ops_image =

            List.append (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt Length.zero)

              (Graphics.pdfops_of_image (xpos, yposbaseline) xratio yratio tag)
(*
          [
            Graphics.op_q;
            Graphics.op_cm_scale xratio yratio (xpos, yposbaseline);
            Graphics.op_Do tag;
            Graphics.op_Q;
          ]
*)
        in
        let opaccnew =
          Alist.append opacc ops_image
        in
          (xpos +% wid, opaccnew)

    | EvHorzHookPageBreak(pbinfo, hookf) ->
        hookf pbinfo (xpos, yposbaseline);  (* -- invokes the hook function -- *)
        (xpos +% wid, opacc)


and ops_of_evaled_tabular point evtabular =
  let (opaccnew, _) =
    evtabular |> List.fold_left (fun (opacc, (xpos, ypos)) (vlen, evcelllst) ->
      let (opaccnew, _) =
        evcelllst |> List.fold_left (fun (opacc, (xpos, ypos)) evcell ->
          match evcell with
          | EvEmptyCell(wid) ->
              (opacc, (xpos +% wid, ypos))

          | EvNormalCell((wid, hgt, dpt), evhblst) ->
              let yposbaseline = ypos -% hgt in
              let (_, opaccsub) =
                  evhblst |> List.fold_left (ops_of_evaled_horz_box yposbaseline) (xpos, opacc)
              in
              let opaccnew =

                (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt) |> Alist.append

                  opaccsub
              in
                (opaccnew, (xpos +% wid, ypos))

          | EvMultiCell((_, _, widsingle, widcell, hgt, dpt), evhblst) ->
              let yposbaseline = ypos -% hgt in
              let (_, opaccsub) =
                  evhblst |> List.fold_left (ops_of_evaled_horz_box yposbaseline) (xpos, opacc)
              in
              let opaccnew =

                (Graphics.pdfops_test_frame (xpos, yposbaseline) widcell hgt dpt) |> Alist.append

                  opaccsub
              in
                (opaccnew, (xpos +% widsingle, ypos))

        ) (opacc, (xpos, ypos))
      in
        (opaccnew, (xpos, ypos -% vlen))
    ) (Alist.empty, point)
  in
    Alist.to_list opaccnew


and ops_of_evaled_vert_box_list (xinit, yinit) opaccinit evvblst =
  evvblst @|> ((xinit, yinit), opaccinit) @|> List.fold_left (fun ((xpos, ypos), opacc) evvb ->
    match evvb with
    | EvVertFixedEmpty(vskip) ->
(*
        (* begin: for debug *)
        let opacc =
          List.rev_append
            (ops_test_box (0.5, 1.0, 0.5) (xpos +% (Length.of_pdf_point 50.), ypos) (Length.of_pdf_point 200.) vskip)
            opacc
        in
        (* end: for debug *)
*)
        ((xpos, ypos -% vskip), opacc)

    | EvVertLine(hgt, dpt, evhblst) ->
        let yposbaseline = ypos -% hgt in
        let (_, opaccend) =
          evhblst @|> (xpos, opacc) @|> List.fold_left (ops_of_evaled_horz_box yposbaseline)
        in
(*
        (* begin: for debug *)
        let opaccend =
          List.rev_append (List.append
            (ops_test_box (1.0, 0.5, 0.5) (xpos, ypos) (Length.of_pdf_point 100.) hgt)
            (ops_test_box (1.0, 0.5, 0.5) (xpos, ypos -% hgt) (Length.of_pdf_point 100.) (Length.negate dpt)))
            opaccend in
        (* end: for debug *)
*)
          ((xpos, yposbaseline +% dpt), opaccend)

    | EvVertFrame(pads, deco, wid, evvblstsub) ->
        let xpossubinit = xpos +% pads.paddingL in
        let ypossubinit = ypos -% pads.paddingT in
        let ((_, ypossub), opaccsub) = ops_of_evaled_vert_box_list (xpossubinit, ypossubinit) opacc evvblstsub in
        let yposend = ypossub -% pads.paddingB in
        let gr = deco (xpos, yposend) wid (ypos -% yposend) Length.zero in
          ((xpos, yposend), Alist.append opaccsub (Graphics.to_pdfops gr))
  )


(* -- PUBLIC -- *)
let pdfops_of_evaled_horz_box (xpos, ypos) evhblst =
  let (_, opacc) = evhblst @|> (xpos, Alist.empty) @|> List.fold_left (ops_of_evaled_horz_box ypos) in
    Alist.to_list opacc


let write_page (pagesch : page_scheme) (evvblst : evaled_vert_box list) ((pdf, pageacc, flnm) : t) : t =

  let paper =
    match pagesch.page_size with
    | A0Paper                -> Pdfpaper.a0
    | A1Paper                -> Pdfpaper.a1
    | A2Paper                -> Pdfpaper.a2
    | A3Paper                -> Pdfpaper.a3
    | A4Paper                -> Pdfpaper.a4
    | A5Paper                -> Pdfpaper.a5
    | USLetter               -> Pdfpaper.usletter
    | USLegal                -> Pdfpaper.uslegal
    | UserDefinedPaper(w, h) -> Pdfpaper.make Pdfunits.PdfPoint (Length.to_pdf_point w) (Length.to_pdf_point h)
  in
  let xinit = pagesch.left_page_margin in
  let yinit = (get_paper_height paper) -% pagesch.top_page_margin in
  let (_, opaccend) = ops_of_evaled_vert_box_list (xinit, yinit) Alist.empty evvblst in

  let oplst = Alist.to_list opaccend in

  let pdfobjstream = Pdfops.stream_of_ops oplst in
(*
  Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate pdfobjstream;
*)
  let pagenew =
    { (Pdfpage.blankpage paper) with
        Pdfpage.content = [pdfobjstream];
    }
  in
    (pdf, pagenew :: pageacc, flnm)


let create_empty_pdf (flnm : file_path) : t =
  let pdf = Pdf.empty () in
    (pdf, [], flnm)


let write_to_file ((pdf, pageacc, flnm) : t) : unit =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  embedding fonts ...");
  let pdfdict_font = FontInfo.get_font_dictionary pdf in
  let pdfarr_procset =
    Pdf.Array(List.map (fun s -> Pdf.Name(s))
                ["/PDF"; "/Text"; "/ImageC"; "ImageB"; "ImageI";])
  in
  let pdfdict_xobject = ImageInfo.get_xobject_dictionary pdf in
  let ir_resources =
    Pdf.addobj pdf (Pdf.Dictionary[
      ("/Font"   , pdfdict_font);
      ("/XObject", pdfdict_xobject);
      ("/ProcSet", pdfarr_procset);
    ])
  in
  print_endline (" ---- ---- ---- ----");
  print_endline ("  writing pages ...");
  let pagelst =
    pageacc |> List.rev |> List.map (fun page ->
      { page with Pdfpage.resources = Pdf.Indirect(ir_resources); }
    )
  in
  let (pdfsub, irpageroot) = Pdfpage.add_pagetree pagelst pdf in
  let pdfout = Pdfpage.add_root irpageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdfout flnm
