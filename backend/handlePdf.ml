
(* for test *)
let print_for_debug msgln = ()


open HorzBox

let (~%) = SkipLength.to_pdf_point
let op_cm (xdiff, ydiff) =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (~% xdiff, ~% ydiff)])

let op_Tm_translate (xpos, ypos) =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate
                     (SkipLength.to_pdf_point xpos, SkipLength.to_pdf_point ypos)])

let op_Tf tag sl = Pdfops.Op_Tf(tag, SkipLength.to_pdf_point sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_Tj_hex str = Pdfops.Op_Tj_hex(str)
let op_TJ obj = Pdfops.Op_TJ(obj)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET
let op_m (x, y) = Pdfops.Op_m(~% x, ~% y)
let op_l (x, y) = Pdfops.Op_l(~% x, ~% y)
let op_re (x, y) (w, h) = Pdfops.Op_re(~% x, ~% y, ~% w, ~% h)
let op_S = Pdfops.Op_S
let op_q = Pdfops.Op_q
let op_Q = Pdfops.Op_Q
let op_RG (r, g, b) = Pdfops.Op_RG(r, g, b)


let encode_tj_string enc tjs =
  match (enc, tjs) with
  | (Latin1, NoKernText(intext))  -> op_Tj (InternalText.to_utf8 intext)
  | (UTF16BE, NoKernText(intext)) -> op_Tj_hex (InternalText.to_utf16be_hex intext)
  | (Latin1, KernedText(knstr))   -> op_TJ (Pdf.Array(knstr |> List.map (function
                                                                 | TJChar(tjch)   -> Pdf.String(InternalText.to_utf8 tjch)
                                                                 | TJKern(rawwid) ->
                                                                     let () = print_for_debug ("!!RAWWID(L)= " ^ (string_of_int rawwid)) in  (* for debug *)
                                                                       Pdf.Integer(-rawwid) )))
  | (UTF16BE, KernedText(knstr))  -> op_TJ (Pdf.Array(knstr |> List.map (function
                                                                 | TJChar(tjch)   -> Pdf.StringHex(InternalText.to_utf16be_hex tjch)
                                                                 | TJKern(rawwid) ->
                                                                     let () = print_for_debug ("!!RAWWID(U)= " ^ (string_of_int rawwid)) in  (* for debug *)
                                                                       Pdf.Integer(-rawwid) )))
  


type t = Pdf.t * Pdfpage.t list * file_path * (string * Pdf.pdfobject) list


let left_margin = SkipLength.of_pdf_point 75.   (* temporary; should be variable *)
let top_margin = SkipLength.of_pdf_point 100.   (* temporary; should be variable *)
let leading = SkipLength.of_pdf_point 32.       (* temporary; should be variable *)


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
                | EvHorzFixedBoxAtom(wid, EvFixedEmpty(_)) -> (wid, [])
                | EvHorzFixedBoxAtom(wid, EvFixedString((fontabrv, size, enc), tjs)) ->
                    let tag = FontInfo.get_tag fontabrv in
                    let opword = encode_tj_string enc tjs in
                      (wid, [
(*
                        (* begin: for test; underline every word *)
                        op_q;
                        op_RG (1.0, 0.5, 0.5);
                        op_m (xpos, yposbaseline);
                        op_l (xpos +% wid, yposbaseline);
                        op_re (xpos, yposbaseline +% hgt) (wid, SkipLength.zero -% (hgt -% dpt));
                        op_S;
                        op_Q;
                        (* end: for test *)
*)
                        op_cm (SkipLength.zero, SkipLength.zero);
                        op_BT;
                        op_Tm_translate (xpos, yposbaseline);
                        op_Tf tag size;
                        opword;
                        op_ET;
                      ])
              in
              let opaccnew = List.rev_append ops opacc in
                (xpos +% widdiff, opaccnew)
            )
          in
            ((left_margin, yposbaseline -% dpt), opaccend)
    )
  in

  let oplst = List.rev opaccend in

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
      (* temporary; can add the font dictionary to data when writing a PDF file instead of when creating empty data *)
    (pdf, [], flnm, fontdict)


let write_to_file ((pdf, pageacc, flnm, _) : t) : unit =
  let pagelst = List.rev pageacc in
  let (pdfsub, irpageroot) = Pdfpage.add_pagetree pagelst pdf in
  let pdfout = Pdfpage.add_root irpageroot [] pdfsub in
    Pdfwrite.pdf_to_file pdfout flnm
