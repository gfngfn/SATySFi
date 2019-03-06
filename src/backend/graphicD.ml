
open LengthInterface
open GraphicBase
open MyUtil


let (~%) = Length.to_pdf_point
let op_cm_translate (xdiff, ydiff) =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (~% xdiff, ~% ydiff)])

let op_cm_scale xratio yratio (xdiff, ydiff) =
  let matr =
    let open Pdftransform in
      { a = xratio;    b = 0.;
        c = 0.;        d = yratio;
        e = ~% xdiff;  f = ~% ydiff; }
  in
    Pdfops.Op_cm(matr)

let op_Tm_translate (xpos, ypos) =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate(~% xpos, ~% ypos)])

let op_Tf tag sl = Pdfops.Op_Tf(tag, ~% sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_Tj_hex str = Pdfops.Op_Tj_hex(str)
let op_TJ obj = Pdfops.Op_TJ(obj)
let op_Ts len = Pdfops.Op_Ts(~% len)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET
let op_m (x, y) = Pdfops.Op_m(~% x, ~% y)
let op_l (x, y) = Pdfops.Op_l(~% x, ~% y)
let op_c (p1, q1) (p2, q2) (x, y) = Pdfops.Op_c(~% p1, ~% q1, ~% p2, ~% q2, ~% x, ~% y)
let op_h = Pdfops.Op_h
let op_re (x, y) (w, h) = Pdfops.Op_re(~% x, ~% y, ~% w, ~% h)

let op_q = Pdfops.Op_q
let op_Q = Pdfops.Op_Q

let op_RG (r, g, b) = Pdfops.Op_RG(r, g, b)
let op_rg (r, g, b) = Pdfops.Op_rg(r, g, b)
let op_K (c, m, y, k) = Pdfops.Op_K(c, m, y, k)
let op_k (c, m, y, k) = Pdfops.Op_k(c, m, y, k)
let op_G gray = Pdfops.Op_G(gray)
let op_g gray = Pdfops.Op_g(gray)

let op_S = Pdfops.Op_S
let op_f = Pdfops.Op_f
let op_f' = Pdfops.Op_f'
let op_B = Pdfops.Op_B
let op_B' = Pdfops.Op_B'
let op_M ml = Pdfops.Op_M(~% ml)
let op_w lw = Pdfops.Op_w(~% lw)

let op_Do name = Pdfops.Op_Do(name)

(*
let op_J = function
  | ButtCap             -> Pdfops.Op_J(0)
  | RoundCap            -> Pdfops.Op_J(1)
  | ProjectingSquareCap -> Pdfops.Op_J(2)

let op_j = function
  | MiterJoin -> Pdfops.Op_j(0)
  | RoundJoin -> Pdfops.Op_j(1)
  | BevelJoin -> Pdfops.Op_j(2)
*)

let pdfop_of_text_color = function
  | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
  | DeviceGray(gray)       -> op_g gray
  | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)


type dash = length * length * length

type 'a element =
  | Fill         of color * path list
  | Stroke       of length * color * path list
  | DashedStroke of length * dash * color * path list
  | HorzText     of point * 'a


type 'a t = ('a element) Alist.t


let empty = Alist.empty


let extend = Alist.extend


let singleton elem = Alist.extend Alist.empty elem


let shift_element v grelem =
  match grelem with
  | Fill(color, pathlst)                      -> Fill(color, pathlst |> List.map (shift_path v))
  | Stroke(thkns, color, pathlst)             -> Stroke(thkns, color, pathlst |> List.map (shift_path v))
  | DashedStroke(thkns, dash, color, pathlst) -> DashedStroke(thkns, dash, color, pathlst |> List.map (shift_path v))
  | HorzText(pt, textvalue)                   -> HorzText(pt +@% v, textvalue)


let get_element_bbox textbboxf grelem =
  match grelem with
  | Fill(_, pathlst)
  | Stroke(_, _, pathlst)
  | DashedStroke(_, _, _, pathlst)
      -> get_path_list_bbox pathlst
           (* -- currently ignores the thickness of the stroke -- *)

  | HorzText(pt, textvalue) -> textbboxf pt textvalue


let make_fill (color : color) (pathlst : path list) : 'a element =
  Fill(color, pathlst)


let make_stroke (thickness : length) (color : color) (pathlst : path list) : 'a element =
  Stroke(thickness, color, pathlst)


let make_dashed_stroke (thickness : length) (dash : dash) (color : color) (pathlst : path list) : 'a element =
  DashedStroke(thickness, dash, color, pathlst)


let make_text (pt : point) textvalue =
  HorzText(pt, textvalue)


let pdfops_of_elements (ptorigin : point) (elemlst : (point path_element) list) (closingopt : (unit path_element) option) =
  let opacc =
    elemlst |> List.fold_left (fun acc elem ->
      match elem with
      | LineTo(ptto)                    -> Alist.extend acc (op_l ptto)
      | CubicBezierTo(ptc1, ptc2, ptto) -> Alist.extend acc (op_c ptc1 ptc2 ptto)
    ) Alist.empty
  in
  let closingopacc =
    match closingopt with
    | None                                -> opacc
    | Some(LineTo(()))                    -> Alist.extend opacc op_h
    | Some(CubicBezierTo(ptc1, ptc2, ())) -> Alist.append opacc [op_c ptc1 ptc2 ptorigin; op_h]
  in
    Alist.to_list closingopacc


let pdfops_of_path (path : path) : Pdfops.t list =
  match path with
  | GeneralPath(ptorigin, elemlst, closingopt) -> (op_m ptorigin) :: (pdfops_of_elements ptorigin elemlst closingopt)
  | Rectangle(pt1, pt2)                        -> [op_re pt1 pt2]


let pdfops_of_path_list (pathlst : path list) : Pdfops.t list =
  pathlst |> List.map pdfops_of_path |> List.concat


let pdfop_of_stroke_color stroke_color =
  match stroke_color with
  | DeviceRGB(r, g, b)     -> op_RG (r, g, b)
  | DeviceCMYK(c, m, y, k) -> op_K (c, m, y, k)
  | DeviceGray(gray)       -> op_G gray


let pdfops_of_stroke (line_width : length) (stroke_color : color) (pathlst : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list pathlst in
  let op_stroke_color = pdfop_of_stroke_color stroke_color in
  let ops_state =
    [ op_w line_width; ]
  in
  let op_draw = op_S in  (* -- draws only strokes -- *)
    List.concat [[op_q; op_stroke_color]; ops_state; ops_path; [op_draw; op_Q]]


let pdfops_of_dashed_stroke (line_width : length) (d1, d2, d0) (stroke_color : color) (pathlst : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list pathlst in
  let op_stroke_color = pdfop_of_stroke_color stroke_color in
  let ops_state =
    let op_dashed = Pdfops.Op_d([~% d1; ~% d2], ~% d0) in
      [ op_w line_width; op_dashed ]
  in

  let op_draw = op_S in
    List.concat [[op_q; op_stroke_color]; ops_state; ops_path; [op_draw; op_Q]]


let pdfops_of_fill (fill_color : color) (pathlst : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list pathlst in
  let op_fill_color =
    match fill_color with
    | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
    | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)
    | DeviceGray(gray)       -> op_g gray
  in
  let op_draw = op_f' in  (* -- draws fills by the even-odd rule -- *)
    List.concat [[op_q; op_fill_color]; ops_path; [op_draw; op_Q]]


let pdfops_of_text (pt : point) (tag : string) (fontsize : length) (color : color) (otxt : OutputText.t) =
  let pdfopstxt =
    (OutputText.to_TJ_arguments otxt) |> List.fold_left (fun acc (hopt, pdfobjs) ->
      let optxt = op_TJ (Pdf.Array(pdfobjs)) in
        match hopt with
        | None ->
            Alist.append acc [op_Ts Length.zero; optxt]

        | Some(FontFormat.PerMille(h)) ->
            let r = fontsize *% (float_of_int h *. 0.001) in
              Alist.append acc [op_Ts r; optxt]

    ) Alist.empty |> Alist.to_list
  in
  List.concat [
    [
      op_cm_translate (Length.zero, Length.zero);
      op_q;
      pdfop_of_text_color color;
      op_BT;
      op_Tm_translate pt;
      op_Tf tag fontsize;
    ];
    pdfopstxt;
    [
      op_ET;
      op_Q;
    ];
  ]


let pdfops_of_image (pt : point) xratio yratio tag =
  [
    op_q;
    op_cm_scale xratio yratio pt;
    op_Do tag;
    op_Q;
  ]


let to_pdfops (gr : 'a t) (f : point -> 'a -> Pdfops.t list) : Pdfops.t list =
  let grelemlst = Alist.to_list gr in
    grelemlst |> List.map (function
      | Fill(color, pathlst)                    -> pdfops_of_fill color pathlst
      | Stroke(thk, color, pathlst)             -> pdfops_of_stroke thk color pathlst
      | DashedStroke(thk, dash, color, pathlst) -> pdfops_of_dashed_stroke thk dash color pathlst
      | HorzText(pt, textvalue)                 -> f pt textvalue
    ) |> List.concat


(* -- 'pdfops_test_box': output bounding box of vertical elements for debugging -- *)
let pdfops_test_box color (xpos, ypos) wid hgt =
  [
    op_q;
    pdfop_of_stroke_color color;
    op_re (xpos, ypos) (wid, Length.negate hgt);
    op_S;
    op_Q;
  ]


(* -- 'pdfops_test_frame': output bounding box of horizontal elements for debugging -- *)
let pdfops_test_frame color (xpos, yposbaseline) wid hgt dpt =
  [
    op_q;
    pdfop_of_stroke_color color;
    op_w (Length.of_pdf_point 0.1);
    op_m (xpos, yposbaseline);
    op_l (xpos +% wid, yposbaseline);
    op_re (xpos, yposbaseline +% hgt) (wid, Length.zero -% (hgt -% dpt));
    op_S;
    op_Q;
  ]


let pdfops_test_scale color (xpos, ypos) len =
  let len_unit = Length.of_pdf_point 10. in
  let divs =
    let n = truncate (len /% len_unit) in
    (range 0 n) |> List.map (fun i ->
       let lendiv =
         if i mod 10 = 0 then
           Length.of_pdf_point 6.
         else if i mod 5 = 0 then
           Length.of_pdf_point 4.
         else
           Length.of_pdf_point 2.
       in
       let ydiv = ypos -% len_unit *% (float i) in
       [
         op_m (xpos, ydiv);
         op_l (xpos -% lendiv, ydiv);
       ]

    ) |> List.concat
  in
  List.concat [
    [
      op_q;
      pdfop_of_stroke_color color;
      op_w (Length.of_pdf_point 0.5);
      op_m (xpos, ypos);
      op_l (xpos, ypos -% len);
    ];
    divs;
    [
      op_S;
      op_Q;
    ];
  ]
