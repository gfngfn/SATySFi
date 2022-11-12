
open MyUtil
open LengthInterface
open GraphicBase
open HorzBox


type t =
  | PDF of Pdf.t * Pdfpage.t Alist.t * abs_path


type 'o op_funcs = {
  graphics   : (intermediate_horz_box list) GraphicD.t -> (point -> intermediate_horz_box list -> 'o list) -> 'o list;
  text       : horz_string_info -> point -> OutputText.t -> 'o list;
  math       : math_string_info -> point -> OutputText.t -> 'o list;
  image      : ImageInfo.key -> point -> length -> length -> 'o list;
  test_box   : color -> point -> length -> length -> 'o list;
  test_frame : color -> point -> length -> length -> length -> 'o list;
  test_scale : color -> point -> length -> 'o list;
  test_skip_fixed         : color -> point -> length -> 'o list;
  test_skip_between_lines : color -> point -> length -> 'o list;
  test_skip_margins       : color -> point -> length -> (bool * length) option -> (bool * length) option -> 'o list
}


let pdfops_of_text (hsinfo : horz_string_info) pt otxt =
  let tag = FontInfo.get_font_tag hsinfo.font_key in
  GraphicD.pdfops_of_text pt tag hsinfo.text_font_size hsinfo.text_color otxt


let pdfops_of_math (msinfo : math_string_info) pt otxt =
  let tag = FontInfo.get_math_tag msinfo.info_math_font_key in
  GraphicD.pdfops_of_text pt tag msinfo.info_math_font_size msinfo.info_math_color otxt


let pdfops_of_image imgkey pt wid hgt =
  let tag = ImageInfo.get_tag imgkey in
  let (xratio, yratio) = ImageInfo.get_ratio imgkey wid hgt in
  GraphicD.pdfops_of_image pt xratio yratio tag


let fs_pdf = {
  graphics   = GraphicD.to_pdfops;
  text       = pdfops_of_text;
  math       = pdfops_of_math;
  image      = pdfops_of_image;
  test_box   = GraphicD.pdfops_test_box;
  test_frame = GraphicD.pdfops_test_frame;
  test_scale = GraphicD.pdfops_test_scale;
  test_skip_fixed         = GraphicD.pdfops_test_skip_fixed;
  test_skip_between_lines = GraphicD.pdfops_test_skip_between_lines;
  test_skip_margins       = GraphicD.pdfops_test_skip_margins;
}


let color_show_space = DeviceRGB(0.0, 0.0, 1.0)
let color_show_bbox  = DeviceRGB(1.0, 0.5, 0.5)
let color_show_block_bbox = DeviceRGB(0.25, 0.75, 0.25)
let color_show_frame = DeviceRGB(1.0, 0.0, 1.0)
let color_show_skip = DeviceRGB(0., 0.5, 0.)
let color_show_overfull = DeviceRGB(1., 0., 0.)
let color_show_unreachable = DeviceRGB(0., 1., 0.)


let warn_ratios (fs : 'o op_funcs) (pbinfo : page_break_info) pt hgt dpt (ratios : ratios) : 'o list =
  let pageno = pbinfo.current_page_number in
  match ratios with
  | TooLong{ required = wid_required; actual = wid_actual } ->
      Logging.warn_overfull_line pageno;
      if OptionState.does_debug_show_overfull () then
        List.append
          (fs.test_frame color_show_overfull pt wid_actual hgt dpt)
          (fs.test_frame color_show_overfull pt wid_required hgt dpt)
      else
        []

  | TooShort{ required = wid_required; actual = wid_actual } ->
      Logging.warn_underfull_line pageno;
      if OptionState.does_debug_show_overfull () then
        List.append
          (fs.test_frame color_show_overfull pt wid_actual hgt dpt)
          (fs.test_frame color_show_overfull pt wid_required hgt dpt)
      else
        []

  | Permissible(_) ->
      []


let warn_reachability (fs : 'o op_funcs) (pbinfo : page_break_info) pt hgt dpt (reach : reachability) : 'o list =
  match reach with
  | Unreachable ->
      Logging.warn_unreachable pbinfo.current_page_number;
      if OptionState.does_debug_show_overfull () then
        let wid = Length.of_pdf_point 2. in  (* temporary *)
        fs.test_frame color_show_unreachable pt wid hgt dpt
      else
        []

  | Reachable(ratios) ->
      warn_ratios fs pbinfo pt hgt dpt ratios


let boolize (br, len) =
  match br with
  | Breakable   -> (true, len)
  | Unbreakable -> (false, len)


let rec ops_of_evaled_horz_box (fs : 'o op_funcs) (pbinfo : page_break_info) yposbaseline (xpos, opacc) (evhb : evaled_horz_box) =
  let (wid, evhbmain) = evhb in
    match evhbmain with
    | EvHorzEmpty ->
        let opaccnew =
          if OptionState.does_debug_show_space () then
            let opsgr = fs.test_box color_show_space (xpos, yposbaseline) wid (Length.of_pdf_point 2.) in
            Alist.append opacc opsgr
          else
            opacc
        in
        (xpos +% wid, opaccnew)

    | EvHorzFrame{ ratios; height = hgt_frame; depth = dpt_frame; decoration = deco; contents = imhbs } ->
        let ops_warning =
          let pt = (xpos, yposbaseline) in
          warn_ratios fs pbinfo pt hgt_frame dpt_frame ratios
        in
        let gr_background =
          deco (xpos, yposbaseline) wid hgt_frame dpt_frame
            (* -- depth values are nonpositive -- *)
        in
        let opsgr = fs.graphics gr_background (pdfops_of_intermediate_horz_box_list fs pbinfo) in
        let opaccinit = Alist.append opacc opsgr in
        let (xposnew, opaccsub) =
          imhbs @|> (xpos, opaccinit) @|> List.fold_left (ops_of_evaled_horz_box fs pbinfo yposbaseline)
        in
        let ops_foreground = ops_warning in
        let opaccnew = Alist.append opaccsub ops_foreground in
        (xposnew, opaccnew)

    | EvHorzString{ info = hsinfo; height = hgt; depth = dpt; output = otxt } ->
        let ops =
          let opsmain =
            fs.text hsinfo (xpos, yposbaseline +% hsinfo.rising) otxt
          in
          if OptionState.does_debug_show_bbox () then
            let opsgr = fs.test_frame color_show_bbox (xpos, yposbaseline) wid hgt dpt in
            List.append opsgr opsmain
          else
            opsmain
        in
        let opaccnew = Alist.append opacc ops in
        (xpos +% wid, opaccnew)

    | EvHorzMathGlyph{ info = msinfo; height = hgt; depth = dpt; output = otxt } ->
        let ops =
          let opsmain =
            fs.math msinfo (xpos, yposbaseline) otxt
          in
          if OptionState.does_debug_show_bbox () then
            let opsgr = fs.test_frame color_show_bbox (xpos, yposbaseline) wid hgt dpt in
            List.append opsgr opsmain
          else
            opsmain
        in
        let opaccnew = Alist.append opacc ops in
          (xpos +% wid, opaccnew)

    | EvHorzRising{ height = _hgt; depth = _dpt; rising = lenrising; contents = evhblst } ->
        let (_, opaccsub) =
          evhblst |> List.fold_left (ops_of_evaled_horz_box fs pbinfo (yposbaseline +% lenrising)) (xpos, opacc)
        in
        let opaccnew =
(*
          if OptionState.does_debug_show_bbox () then
            Alist.append opaccsub (GraphicD.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt)
          else
*)
            opaccsub
        in
        (xpos +% wid, opaccnew)

    | EvHorzEmbeddedVert{ height = hgt; depth = _dpt; contents = evvblst } ->
        let ((_, _), opaccnew) = ops_of_evaled_vert_box_list fs pbinfo (xpos, yposbaseline +% hgt) opacc evvblst in
        (xpos +% wid, opaccnew)

    | EvHorzInlineGraphics{ height = hgt; depth = dpt; graphics } ->
        let gr =
          match graphics with
          | ImGraphicsFixed(grff)    -> grff (xpos, yposbaseline)
          | ImGraphicsVariable(grvf) -> grvf wid (xpos, yposbaseline)
        in
        let opsgr = pdfops_of_graphics fs pbinfo gr in
        let opaccsub = Alist.append opacc opsgr in
        let opaccnew =
          if OptionState.does_debug_show_bbox () then
            let opsgr = fs.test_frame color_show_bbox (xpos, yposbaseline) wid hgt dpt in
            Alist.append opaccsub opsgr
          else
            opaccsub
        in
        (xpos +% wid, opaccnew)

    | EvHorzInlineTabular{
        height        = hgt;
        depth         = _dpt;
        rows          = evtabular;
        column_widths = widlst;
        row_heights   = lenlst;
        rule_graphics = rulesf;
      } ->
        let ops_tabular =
          ops_of_evaled_tabular fs pbinfo (xpos, yposbaseline +% hgt) evtabular
        in
        let (xacc, _) =
          widlst |> List.fold_left (fun (xacc, x) w ->
            let xnew = x +% w in
            (Alist.extend xacc xnew, xnew)
          ) (Alist.extend Alist.empty xpos, xpos)
        in
        let (yacc, _) =
          let yinit = yposbaseline +% hgt in
          lenlst |> List.fold_left (fun (yacc, y) l ->
            let ynew = y -% l in
            (Alist.extend yacc ynew, ynew)
          ) (Alist.extend Alist.empty yinit, yinit)
        in
        let gr = rulesf (Alist.to_list xacc) (Alist.to_list yacc) in
        let ops_rules = pdfops_of_graphics fs pbinfo gr in
        let opaccnew = Alist.append (Alist.append opacc ops_tabular) ops_rules in
        (xpos +% wid, opaccnew)

    | EvHorzInlineImage{ height = hgt; key = imgkey } ->
        let ops_image =
          fs.image imgkey (xpos, yposbaseline) wid hgt
        in
        let opaccnew = Alist.append opacc ops_image in
        (xpos +% wid, opaccnew)

    | EvHorzHookPageBreak(pbinfo, hookf) ->
        hookf pbinfo (xpos, yposbaseline);  (* Invokes the hook function. *)
        (xpos +% wid, opacc)
          (* TODO: consider removing `pbinfo` from parameters *)


and ops_of_evaled_tabular (fs : 'o op_funcs) (pbinfo : page_break_info) point evtabular =
  let (opaccnew, _) =
    evtabular |> List.fold_left (fun (opacc, (xpos, ypos)) (vlen, evcelllst) ->
      let (opaccnew, _) =
        evcelllst |> List.fold_left (fun (opacc, (xpos, ypos)) evcell ->
          match evcell with
          | EvEmptyCell(wid) ->
              (opacc, (xpos +% wid, ypos))

          | EvNormalCell(ratios, (wid, hgt, dpt), evhblst) ->
              let yposbaseline = ypos -% hgt in
              let ops_warning =
                warn_ratios fs pbinfo (xpos, yposbaseline) hgt dpt ratios
              in
              let (_, opaccsub) =
                evhblst |> List.fold_left (ops_of_evaled_horz_box fs pbinfo yposbaseline) (xpos, opacc)
              in
              let opaccnew =
(*
                (GraphicD.pdfops_test_frame (xpos, yposbaseline) wid hgt dpt) |> Alist.append
*)
                  Alist.append opaccsub ops_warning
              in
              (opaccnew, (xpos +% wid, ypos))

          | EvMultiCell(ratios, (_, _, widsingle, _widcell, hgt, dpt), evhblst) ->
              let yposbaseline = ypos -% hgt in
              let ops_warning =
                warn_ratios fs pbinfo (xpos, yposbaseline) hgt dpt ratios
              in
              let (_, opaccsub) =
                  evhblst |> List.fold_left (ops_of_evaled_horz_box fs pbinfo yposbaseline) (xpos, opacc)
              in
              let opaccnew =
(*
                (GraphicD.pdfops_test_frame (xpos, yposbaseline) widcell hgt dpt) |> Alist.append
*)
                  Alist.append opaccsub ops_warning
              in
              (opaccnew, (xpos +% widsingle, ypos))

        ) (opacc, (xpos, ypos))
      in
        (opaccnew, (xpos, ypos -% vlen))
    ) (Alist.empty, point)
  in
    Alist.to_list opaccnew


and ops_of_evaled_vert_box_list (fs : 'o op_funcs) pbinfo (xinit, yinit) opaccinit evvblst =
  evvblst @|> ((xinit, yinit), opaccinit) @|> List.fold_left (fun (((xpos, ypos) as pos), opacc) evvb ->
    match evvb with
    | EvVertFixedEmpty(debug_margins, vskip) ->
        let opacc =
          if OptionState.does_debug_show_block_space () then
            let ops =
              match debug_margins with
              | Fixed              -> fs.test_skip_fixed color_show_skip pos vskip
              | BetweenLines       -> fs.test_skip_between_lines color_show_skip pos vskip
              | LowerOnly(br2)     -> fs.test_skip_margins color_show_skip pos vskip None (Some(boolize (br2, vskip)))
              | UpperOnly(br1)     -> fs.test_skip_margins color_show_skip pos vskip (Some(boolize (br1, vskip))) None
              | Both(pair1, pair2) -> fs.test_skip_margins color_show_skip pos vskip (Some(boolize pair1)) (Some(boolize pair2))
            in
            Alist.append opacc ops
          else
            opacc
        in
        ((xpos, ypos -% vskip), opacc)

    | EvVertLine(reach, hgt, dpt, evhblst) ->
        let yposbaseline = ypos -% hgt in
        let ops_warning =
          warn_reachability fs pbinfo (xpos, yposbaseline) hgt dpt reach
        in
        let (xposlast, opaccend) =
          evhblst @|> (xpos, opacc) @|> List.fold_left (ops_of_evaled_horz_box fs pbinfo yposbaseline)
        in
        let opaccend =
          if OptionState.does_debug_show_block_bbox () then
            let wid = xposlast -% xpos in
            Alist.append opaccend (fs.test_frame color_show_block_bbox (xpos, yposbaseline) wid hgt dpt)
          else
            opaccend
        in
        let opaccend = Alist.append opaccend ops_warning in
        ((xpos, yposbaseline +% dpt), opaccend)

    | EvVertFrame(pads, _, deco, wid, evvblstsub) ->
        let xpossubinit = xpos +% pads.paddingL in
        let ypossubinit = ypos -% pads.paddingT in
        let ((_, ypossub), opaccsub) = ops_of_evaled_vert_box_list fs pbinfo (xpossubinit, ypossubinit) Alist.empty evvblstsub in
        let yposend = ypossub -% pads.paddingB in
        let gr = deco (xpos, yposend) wid (ypos -% yposend) Length.zero in
        let opsgr = pdfops_of_graphics fs pbinfo gr in
        let opaccframe = Alist.append opacc opsgr in
        let opaccnew = Alist.append opaccframe (Alist.to_list opaccsub) in
        ((xpos, yposend), opaccnew)

    | EvVertHookPageBreak(hookf) ->
        hookf pbinfo (xpos, ypos); (* Invokes the hook function. *)
        ((xpos, ypos), opacc)
  )


and pdfops_of_intermediate_horz_box_list (fs : 'o op_funcs) (pbinfo : page_break_info) ((xpos, yposbaseline) : point) (imhblst : intermediate_horz_box list) : 'o list =
  let (evhblst, _) = PageInfo.embed_page_info pbinfo imhblst in
  let (_, opacc) =
    evhblst |> List.fold_left (ops_of_evaled_horz_box fs pbinfo yposbaseline) (xpos, Alist.empty)
  in
  Alist.to_list opacc


and pdfops_of_graphics (fs : 'o op_funcs) (pbinfo : page_break_info) gr =
  fs.graphics gr (pdfops_of_intermediate_horz_box_list fs pbinfo)


type contents = Pdfops.t Alist.t

type page =
  | Page of Pdfpaper.t * page_content_scheme * contents * page_break_info


let invert_coordinate paper_height (xraw, yraw) =
  (xraw, paper_height -% yraw)


let get_paper_height (paper : Pdfpaper.t) : length =
  let dpi = 300. in  (* temporary; should be variable *)
  let pdfpt = Pdfunits.convert dpi (Pdfpaper.unit paper) Pdfunits.PdfPoint (Pdfpaper.height paper) in
  Length.of_pdf_point pdfpt


let opacc_of_body_and_footnote
      (txtlen : length)  (* -- for option `--debug-show-block-bbox` -- *)
      (pbinfo : page_break_info)
      ((ptbody, evvblstbody) : (length * length) * evaled_vert_box list)
      ((ptfootnote, evvblstfootnote) : (length * length) * evaled_vert_box list)
    : Pdfops.t Alist.t =

  let opaccbody =
    let (_, opaccbody) = ops_of_evaled_vert_box_list fs_pdf pbinfo ptbody Alist.empty evvblstbody in
    if OptionState.does_debug_show_block_bbox () then
      Alist.append opaccbody (fs_pdf.test_scale color_show_frame ptbody txtlen)
    else
      opaccbody
  in
  let opaccfootnote =
    let (_, opaccfootnote) = ops_of_evaled_vert_box_list fs_pdf pbinfo ptfootnote Alist.empty evvblstfootnote in
    if OptionState.does_debug_show_block_bbox () then
      let wid = Length.of_pdf_point 5. in
      let len = Length.of_pdf_point 1. in
      Alist.append opaccfootnote (fs_pdf.test_box color_show_frame ptfootnote wid len)
    else
      opaccfootnote
  in
  Alist.cat opaccbody opaccfootnote


let get_body_origin_position (paper_height : length) (origin : length * length) =
  invert_coordinate paper_height origin


let get_footnote_origin_position (paper_height : length) (content_height : length) (origin : length * length) (evvblstfootnote : evaled_vert_box list) =
  let footnote_height = get_height_of_evaled_vert_box_list evvblstfootnote in
  let (xorg, yorg) = origin in
  invert_coordinate paper_height (xorg, yorg +% content_height -% footnote_height)


let get_pdf_paper  ((w, h) : length * length) : Pdfpaper.t =
  Pdfpaper.make Pdfunits.PdfPoint (Length.to_pdf_point w) (Length.to_pdf_point h)


let make_empty_page
      ~(paper_size : length * length)
      (pbinfo : page_break_info)
      (pagecontsch : page_content_scheme)
    : page =
  let paper = get_pdf_paper paper_size in
  Page(paper, pagecontsch, Alist.empty, pbinfo)


let add_column_to_page
      (Page(paper, pagecontsch, opacc, pbinfo))
      (origin_shift : length)
      (evvblstbody : evaled_vert_box list)
      (evvblstfootnote : evaled_vert_box list)
    : page =
  let content_height = pagecontsch.page_content_height in
  let content_origin =
    let (x, y) = pagecontsch.page_content_origin in
    (x +% origin_shift, y)
  in
  let opacccolumn =
    let paper_height = get_paper_height paper in
    let posbody =
      get_body_origin_position
        paper_height
        content_origin
    in
    let posfootnote =
      get_footnote_origin_position
        paper_height
        content_height
        content_origin
        evvblstfootnote
    in
    opacc_of_body_and_footnote content_height pbinfo
      (posbody, evvblstbody)
      (posfootnote, evvblstfootnote)
  in
  Page(paper, pagecontsch, Alist.cat opacc opacccolumn, pbinfo)


let write_page (Page(paper, _pagecontsch, opaccpage, pbinfo) : page) (pagepartsf : page_parts_scheme_func) ((PDF(pdf, pageacc, flnm)) : t) : t =

  let paper_height = get_paper_height paper in

  let pagepartssch = pagepartsf pbinfo in  (* -- invokes the page-parts function -- *)
  let (evvblst_header, _) = pagepartssch.header_content |> PageInfo.embed_page_info_vert pbinfo in
  let pt_header = invert_coordinate paper_height pagepartssch.header_origin in
  let (_, opacc_header) = ops_of_evaled_vert_box_list fs_pdf pbinfo pt_header opaccpage evvblst_header in

  let (evvblst_footer, _) = pagepartssch.footer_content |> PageInfo.embed_page_info_vert pbinfo in
  let pt_footer = invert_coordinate paper_height pagepartssch.footer_origin in
  let (_, opacc_footer) = ops_of_evaled_vert_box_list fs_pdf pbinfo pt_footer opacc_header evvblst_footer in

  let oplst = Alist.to_list opacc_footer in

  let pdfobjstream = Pdfops.stream_of_ops oplst in

  Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate pdfobjstream;
    (* -- conpresses the operatand/operator stream -- *)

  let pagenew =
    { (Pdfpage.blankpage paper) with
        Pdfpage.content = [pdfobjstream];
    }
  in
  let pagenew = Annotation.add_to_pdf pdf pagenew in
  let () = NamedDest.notify_pagebreak pbinfo.current_page_number in
  PDF(pdf, Alist.extend pageacc pagenew, flnm)


let create_empty_pdf (abspath : abs_path) : t =
  let pdf = Pdf.empty () in
  PDF(pdf, Alist.empty, abspath)


let write_to_file ((PDF(pdf, pageacc, abspath)) : t) : unit =
  Logging.begin_to_embed_fonts ();
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
  Logging.begin_to_write_page ();
  let pagelst =
    pageacc |> Alist.to_list |> List.map (fun page ->
      { page with Pdfpage.resources = Pdf.Indirect(ir_resources); }
    )
  in
  let (pdfsub, irpageroot) = Pdfpage.add_pagetree pagelst pdf in
  let pdfout = pdfsub |> (Pdfpage.add_root irpageroot [])
                      |> Outline.add_to_pdf
                      |> NamedDest.add_to_pdf
                      |> DocumentInformationDictionary.add_to_pdf
  in
  Pdfwrite.pdf_to_file pdfout (get_abs_path_string abspath)
