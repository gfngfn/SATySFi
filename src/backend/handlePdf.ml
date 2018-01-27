
open MyUtil
open LengthInterface
open HorzBox


type t =
  | PDF of Pdf.t * Pdfpage.t list * file_path


let rec ops_of_evaled_horz_box (pbinfo : page_break_info) yposbaseline (xpos, opacc) (evhb : evaled_horz_box) =
  let (wid, evhbmain) = evhb in
    match evhbmain with
    | EvHorzEmpty ->
        let opaccnew =
(*
          (Graphics.pdfops_test_box (GraphicData.DeviceRGB(0., 0., 1.)) (xpos, yposbaseline) wid (Length.of_pdf_point 2.)) |> Alist.append
*)
          opacc
        in
          (xpos +% wid, opaccnew)

    | EvHorzFrame(hgt_frame, dpt_frame, deco, imhblst) ->
        let gr_background =
          deco (xpos, yposbaseline) wid hgt_frame dpt_frame
            (* -- depth values are nonpositive -- *)
        in
        let opaccinit = Alist.append opacc (Graphics.to_pdfops gr_background (pdfops_of_intermediate_horz_box_list pbinfo)) in
        let (xposnew, opaccsub) =
          imhblst @|> (xpos, opaccinit) @|> List.fold_left (ops_of_evaled_horz_box pbinfo yposbaseline)
        in
        let ops_foreground = [] in  (* temporary *)
        let opaccnew = Alist.append opaccsub ops_foreground in
          (xposnew, opaccnew)

    | EvHorzString(hsinfo, hgt, dpt, otxt) ->
        let tag = FontInfo.get_font_tag hsinfo.font_abbrev in
        let ops =
(*
          List.append (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt)  (* for test *)
*)
          (Graphics.pdfops_of_text (xpos, yposbaseline) hsinfo.rising tag hsinfo.text_font_size hsinfo.text_color otxt)
        in
        let opaccnew = Alist.append opacc ops in
          (xpos +% wid, opaccnew)

    | EvHorzMathGlyph(msinfo, hgt, dpt, otxt) ->
        let tag = FontInfo.get_math_tag msinfo.math_font_abbrev in
        let ops =
(*
          List.append (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
          (Graphics.pdfops_of_text (xpos, yposbaseline) Length.zero tag msinfo.math_font_size msinfo.math_color otxt)
        in
        let opaccnew = Alist.append opacc ops in
          (xpos +% wid, opaccnew)

    | EvHorzRising(hgt, dpt, lenrising, evhblst) ->
        let (_, opaccsub) =
          evhblst |> List.fold_left (ops_of_evaled_horz_box pbinfo (yposbaseline +% lenrising)) (xpos, opacc)
        in
        let opaccnew =
(*
          List.rev_append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
          opaccsub
        in
          (xpos +% wid, opaccnew)

    | EvHorzEmbeddedVert(hgt, dpt, evvblst) ->
        let ((_, _), opaccnew) = ops_of_evaled_vert_box_list pbinfo (xpos, yposbaseline +% hgt) opacc evvblst in
          (xpos +% wid, opaccnew)

    | EvHorzInlineGraphics(hgt, dpt, graphics) ->
        let gr =
(*
          List.append (ops_test_frame (xpos, yposbaseline) wid hgt dpt)
*)
          (graphics (xpos, yposbaseline))
        in
        let opaccnew = Alist.append opacc (Graphics.to_pdfops gr (pdfops_of_intermediate_horz_box_list pbinfo)) in
          (xpos +% wid, opaccnew)

    | EvHorzInlineTabular(hgt, dpt, evtabular) ->
        let ops_tabular =
          ops_of_evaled_tabular pbinfo (xpos, yposbaseline +% hgt) evtabular
        in
        let opaccnew = Alist.append opacc ops_tabular in
          (xpos +% wid, opaccnew)

    | EvHorzInlineImage(hgt, imgkey) ->
        let tag = ImageInfo.get_tag imgkey in
        let (xratio, yratio) = ImageInfo.get_ratio imgkey wid hgt in
        let ops_image =

          List.append (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt Length.zero)

          (Graphics.pdfops_of_image (xpos, yposbaseline) xratio yratio tag)
        in
        let opaccnew = Alist.append opacc ops_image in
          (xpos +% wid, opaccnew)

    | EvHorzHookPageBreak(pbinfo, hookf) ->
        hookf pbinfo (xpos, yposbaseline);  (* -- invokes the hook function -- *)
        (xpos +% wid, opacc)


and ops_of_evaled_tabular (pbinfo : page_break_info) point evtabular =
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
                  evhblst |> List.fold_left (ops_of_evaled_horz_box pbinfo yposbaseline) (xpos, opacc)
              in
              let opaccnew =

                (Graphics.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt) |> Alist.append

                  opaccsub
              in
                (opaccnew, (xpos +% wid, ypos))

          | EvMultiCell((_, _, widsingle, widcell, hgt, dpt), evhblst) ->
              let yposbaseline = ypos -% hgt in
              let (_, opaccsub) =
                  evhblst |> List.fold_left (ops_of_evaled_horz_box pbinfo yposbaseline) (xpos, opacc)
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


and ops_of_evaled_vert_box_list pbinfo (xinit, yinit) opaccinit evvblst =
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
          evhblst @|> (xpos, opacc) @|> List.fold_left (ops_of_evaled_horz_box pbinfo yposbaseline)
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

    | EvVertFrame(pads, _, deco, wid, evvblstsub) ->
        let xpossubinit = xpos +% pads.paddingL in
        let ypossubinit = ypos -% pads.paddingT in
        let ((_, ypossub), opaccsub) = ops_of_evaled_vert_box_list pbinfo (xpossubinit, ypossubinit) opacc evvblstsub in
        let yposend = ypossub -% pads.paddingB in
        let gr = deco (xpos, yposend) wid (ypos -% yposend) Length.zero in
          ((xpos, yposend), Alist.append opaccsub (Graphics.to_pdfops gr (pdfops_of_intermediate_horz_box_list pbinfo)))
  )


and pdfops_of_intermediate_horz_box_list (pbinfo : page_break_info) ((xpos, yposbaseline) : point) (imhblst : intermediate_horz_box list) : Pdfops.t list =
  let evhblst = PageInfo.embed_page_info pbinfo imhblst in
  let (_, opacc) =
      evhblst |> List.fold_left (ops_of_evaled_horz_box pbinfo yposbaseline) (xpos, Alist.empty)
  in
    Alist.to_list opacc


type contents = Pdfops.t Alist.t

type page =
  | Page of Pdfpaper.t * page_content_scheme * contents * page_break_info


let invert_coordinate paper_height (xraw, yraw) =
  (xraw, paper_height -% yraw)


let get_paper_height (paper : Pdfpaper.t) : length =
  let dpi = 300. in  (* temporary; should be variable *)
  let pdfpt = Pdfunits.convert dpi (Pdfpaper.unit paper) Pdfunits.PdfPoint (Pdfpaper.height paper) in
    Length.of_pdf_point pdfpt


let page_of_evaled_vert_box_list (pagesize : page_size) (pbinfo : page_break_info) (pagecontsch : page_content_scheme) (evvblstpage : evaled_vert_box list) : page =
  let paper =
    match pagesize with
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
  let paper_height = get_paper_height paper in

  let pt_init = invert_coordinate paper_height pagecontsch.page_content_origin in
  let (_, opaccpage) = ops_of_evaled_vert_box_list pbinfo pt_init Alist.empty evvblstpage in
    Page(paper, pagecontsch, opaccpage, pbinfo)


let write_page (Page(paper, pagecontsch, opaccpage, pbinfo) : page) (pagepartsf : page_parts_scheme_func) ((PDF(pdf, pageacc, flnm)) : t) : t =

  let paper_height = get_paper_height paper in

  let pagepartssch = pagepartsf pbinfo in  (* -- invokes the page-parts function -- *)
  let evvblst_header = pagepartssch.header_content |> PageInfo.embed_page_info_vert pbinfo in
  let pt_header = invert_coordinate paper_height pagepartssch.header_origin in
  let (_, opacc_header) = ops_of_evaled_vert_box_list pbinfo pt_header opaccpage evvblst_header in

  let evvblst_footer = pagepartssch.footer_content |> PageInfo.embed_page_info_vert pbinfo in
  let pt_footer = invert_coordinate paper_height pagepartssch.footer_origin in
  let (_, opacc_footer) = ops_of_evaled_vert_box_list pbinfo pt_footer opacc_header evvblst_footer in

  let oplst = Alist.to_list opacc_footer in

  let pdfobjstream = Pdfops.stream_of_ops oplst in

  Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate pdfobjstream;
    (* -- conpresses the operatand/operator stream -- *)

  let pagenew =
    { (Pdfpage.blankpage paper) with
        Pdfpage.content = [pdfobjstream];
    }
  in
    PDF(pdf, pagenew :: pageacc, flnm)


let create_empty_pdf (flnm : file_path) : t =
  let pdf = Pdf.empty () in
    PDF(pdf, [], flnm)


let write_to_file ((PDF(pdf, pageacc, flnm)) : t) : unit =
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
