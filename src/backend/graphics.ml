
open HorzBox


let (~%) = Length.to_pdf_point
let op_cm (xdiff, ydiff) =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (~% xdiff, ~% ydiff)])

let op_Tm_translate (xpos, ypos) =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate
                     (~% xpos, ~% ypos)])

let op_Tf tag sl = Pdfops.Op_Tf(tag, ~% sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_Tj_hex str = Pdfops.Op_Tj_hex(str)
let op_TJ obj = Pdfops.Op_TJ(obj)
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

let op_J = function
  | ButtCap             -> Pdfops.Op_J(0)
  | RoundCap            -> Pdfops.Op_J(1)
  | ProjectingSquareCap -> Pdfops.Op_J(2)

let op_j = function
  | MiterJoin -> Pdfops.Op_j(0)
  | RoundJoin -> Pdfops.Op_j(1)
  | BevelJoin -> Pdfops.Op_j(2)

let op_d = function
  | SolidLine              -> Pdfops.Op_d([], 0.)
  | DashedLine(d1, d2, d0) -> Pdfops.Op_d([~% d1; ~% d2], ~% d0)


let pdfop_of_text_color = function
  | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
  | DeviceGray(gray)       -> op_g gray
  | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)


let pdfops_of_elements (ptorigin : point) (elemlst : (point path_element) list) (closingopt : (unit path_element) option) =
  let opacc =
    elemlst |> List.fold_left (fun acc elem ->
      match elem with
      | LineTo(ptto)                    -> (op_l ptto) :: acc
      | CubicBezierTo(ptc1, ptc2, ptto) -> (op_c ptc1 ptc2 ptto) :: acc
    ) []
  in
  let closingopacc =
    match closingopt with
    | None                                -> opacc
    | Some(LineTo(()))                    -> op_h :: opacc
    | Some(CubicBezierTo(ptc1, ptc2, ())) -> op_h :: (op_c ptc1 ptc2 ptorigin) :: opacc
  in
    List.rev closingopacc


let pdfops_of_path (path : path) : Pdfops.t list =
  match path with
  | GeneralPath(ptorigin, elemlst, closingopt) -> (op_m ptorigin) :: (pdfops_of_elements ptorigin elemlst closingopt)
  | Rectangle(pt1, pt2)                        -> [op_re pt1 pt2]


let pdfops_of_path_list (pathlst : path list) : Pdfops.t list =
  pathlst |> List.map pdfops_of_path |> List.concat


let pdfops_of_graphics (gstate : graphics_state) (gcmd : graphics_command) (pathlst : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list pathlst in
  let op_stroke_color =
    match gstate.stroke_color with
    | DeviceRGB(r, g, b)     -> op_RG (r, g, b)
    | DeviceCMYK(c, m, y, k) -> op_K (c, m, y, k)
    | DeviceGray(gray)       -> op_G gray
  in
  let op_fill_color =
    match gstate.fill_color with
    | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
    | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)
    | DeviceGray(gray)       -> op_g gray
  in
  let ops_state =
    [
      op_w gstate.line_width;
      op_J gstate.line_cap;
      op_j gstate.line_join;
      op_d gstate.line_dash;
      op_M gstate.miter_limit;
    ]
  in
  let drawop =
    match gcmd with
    | DrawStroke        -> op_S
    | DrawFillByNonzero -> op_f
    | DrawFillByEvenOdd -> op_f'
    | DrawBothByNonzero -> op_B
    | DrawBothByEvenOdd -> op_B'
  in
    List.concat [[op_q]; [op_stroke_color]; [op_fill_color]; ops_state; ops_path; [drawop; op_Q]]


let pdfops_of_stroke (line_width : length) (stroke_color : color) (pathlst : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list pathlst in
  let op_stroke_color =
    match stroke_color with
    | DeviceRGB(r, g, b)     -> op_RG (r, g, b)
    | DeviceCMYK(c, m, y, k) -> op_K (c, m, y, k)
    | DeviceGray(gray)       -> op_G gray
  in
  let ops_state =
    [
      op_w line_width;
    ]
  in
  let op_draw = op_S in  (* -- draws only strokes -- *)
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
