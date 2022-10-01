
open LengthInterface
open GraphicBase
open MyUtil


type dash = length * length * length

type 'a element =
  | Fill         of color * path list
  | Stroke       of length * color * path list
  | DashedStroke of length * dash * color * path list
  | HorzText     of point * 'a
  | LinearTrans  of point * matrix * 'a t
  | Clip         of path list * 'a t

and 'a t = ('a element) list


let empty : 'a t = []


let concat (grs : ('a t) list) : 'a t =
  List.concat grs


let singleton (grelem : 'a element) : 'a t =
  [ grelem ]


let rec shift_element (v : point) (grelem : 'a element) : 'a element =
  match grelem with
  | Fill(color, paths)                      -> Fill(color, paths |> List.map (shift_path v))
  | Stroke(thkns, color, paths)             -> Stroke(thkns, color, paths |> List.map (shift_path v))
  | DashedStroke(thkns, dash, color, paths) -> DashedStroke(thkns, dash, color, paths |> List.map (shift_path v))
  | HorzText(pt, textvalue)                 -> HorzText(pt +@% v, textvalue)
  | LinearTrans(pt, mat, gr_sub)            -> LinearTrans(pt +@% v, mat, shift v gr_sub)
  | Clip(paths, gr_sub)                     -> Clip(List.map (shift_path v) paths, shift v gr_sub)


and shift (v : point) (gr : 'a t) : 'a t =
  gr |> List.map (shift_element v)


let rec get_element_bbox (textbboxf : point -> 'a -> bbox_corners) (grelem : 'a element) : bbox_corners option =
  match grelem with
  | Fill(_, paths)
  | Stroke(_, _, paths)
  | DashedStroke(_, _, _, paths)
  | Clip(paths, _) ->
      Some(get_path_list_bbox paths)
        (* Ignores the thickness of the stroke *)

  | HorzText(pt, textvalue) ->
      Some(textbboxf pt textvalue)

  | LinearTrans(pt, mat, gr) ->
      get_bbox textbboxf gr |> Option.map (centered_linear_transform_bbox mat ~center:pt)


and get_bbox (textbboxf : point -> 'a -> bbox_corners) (gr : 'a t) : bbox_corners option =
  gr |> List.fold_left (fun bbox0_opt grelem ->
    let bbox_opt = get_element_bbox textbboxf grelem in
    match bbox0_opt with
    | None ->
        bbox_opt

    | Some(bbox0) ->
        begin
          match bbox_opt with
          | None       -> bbox0_opt
          | Some(bbox) -> Some(unite_bbox bbox0 bbox)
        end
  ) None


let make_fill (color : color) (paths : path list) : 'a t =
  singleton @@ Fill(color, paths)


let make_stroke (thickness : length) (color : color) (paths : path list) : 'a t =
  singleton @@ Stroke(thickness, color, paths)


let make_dashed_stroke (thickness : length) (dash : dash) (color : color) (paths : path list) : 'a t =
  singleton @@ DashedStroke(thickness, dash, color, paths)


let make_text (pt : point) (textvalue : 'a) : 'a t =
  singleton @@ HorzText(pt, textvalue)


let make_linear_trans (mat : matrix) (gr : 'a t) : 'a t =
  singleton @@ LinearTrans((Length.zero, Length.zero), mat, gr)


let make_clip (gr_sub : 'a t) (paths : path list) : 'a t =
  singleton @@ Clip(paths, gr_sub)


let op_cm_translate ((xdiff, ydiff) : vector) : Pdfops.t =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (!=> xdiff, !=> ydiff)])


let op_cm_scale (xratio : float) (yratio : float) ((xdiff, ydiff) : vector) : Pdfops.t =
  let matr =
    let open Pdftransform in
      { a = xratio;    b = 0.;
        c = 0.;        d = yratio;
        e = !=> xdiff;  f = !=> ydiff; }
  in
  Pdfops.Op_cm(matr)


let op_cm_linear_trans ((a, b, c, d) : matrix) ((xoff, yoff) : vector) : Pdfops.t =
  let matr1 =
    let open Pdftransform in
    { a = 1.; b = 0.;
      c = 0.; d = 1.;
      e = ~-.(!=> xoff);  f = ~-.(!=> yoff); }
  in
  let matr2 =
    let open Pdftransform in
    { a = a; b = c;  (* transpose *)
      c = b; d = d;  (* transpose *)
      e = 0.;  f = 0.; }
  in
  let matr3 =
    let open Pdftransform in
    { a = 1.; b = 0.;
      c = 0.; d = 1.;
      e = !=> xoff;  f = !=> yoff; }
  in
  let matr =
    matr1 |> Pdftransform.matrix_compose matr2
          |> Pdftransform.matrix_compose matr3
  in
  Pdfops.Op_cm(matr)


let op_Tm_translate ((xpos, ypos) : point) : Pdfops.t =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform [ Pdftransform.Translate(!=> xpos, !=> ypos) ])


let op_Tf tag sl = Pdfops.Op_Tf(tag, !=> sl)
(*
let op_Tj str = Pdfops.Op_Tj(str)
let op_Tj_hex str = Pdfops.Op_Tj_hex(str)
*)
let op_TJ obj = Pdfops.Op_TJ(obj)
let op_Ts len = Pdfops.Op_Ts(!=> len)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET
let op_m (x, y) = Pdfops.Op_m(!=> x, !=> y)
let op_l (x, y) = Pdfops.Op_l(!=> x, !=> y)
let op_c (p1, q1) (p2, q2) (x, y) = Pdfops.Op_c(!=> p1, !=> q1, !=> p2, !=> q2, !=> x, !=> y)
let op_h = Pdfops.Op_h
let op_re (x, y) (w, h) = Pdfops.Op_re(!=> x, !=> y, !=> w, !=> h)

let op_q = Pdfops.Op_q
let op_Q = Pdfops.Op_Q

let op_RG (r, g, b) = Pdfops.Op_RG(r, g, b)
let op_rg (r, g, b) = Pdfops.Op_rg(r, g, b)
let op_K (c, m, y, k) = Pdfops.Op_K(c, m, y, k)
let op_k (c, m, y, k) = Pdfops.Op_k(c, m, y, k)
let op_G gray = Pdfops.Op_G(gray)
let op_g gray = Pdfops.Op_g(gray)

let op_S = Pdfops.Op_S

(*
let op_f = Pdfops.Op_f
*)
let op_f' = Pdfops.Op_f'
(*
let op_B = Pdfops.Op_B
let op_B' = Pdfops.Op_B'
let op_M ml = Pdfops.Op_M(!=> ml)
*)

let op_w lw = Pdfops.Op_w(!=> lw)

let op_Do name = Pdfops.Op_Do(name)


let pdfop_of_text_color = function
  | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
  | DeviceGray(gray)       -> op_g gray
  | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)


let pdfops_of_elements (pt_origin : point) (elems : (point path_element) list) (closing_opt : (unit path_element) option) =
  let opacc =
    elems |> List.fold_left (fun acc elem ->
      match elem with
      | LineTo(pt_to)                    -> Alist.extend acc (op_l pt_to)
      | CubicBezierTo(ptc1, ptc2, pt_to) -> Alist.extend acc (op_c ptc1 ptc2 pt_to)
    ) Alist.empty
  in
  let opacc =
    match closing_opt with
    | None                                -> opacc
    | Some(LineTo(()))                    -> Alist.extend opacc op_h
    | Some(CubicBezierTo(ptc1, ptc2, ())) -> Alist.append opacc [ op_c ptc1 ptc2 pt_origin; op_h ]
  in
  Alist.to_list opacc


let pdfops_of_path (path : path) : Pdfops.t list =
  let GeneralPath(ptorigin, elems, closingopt) = path in
  (op_m ptorigin) :: (pdfops_of_elements ptorigin elems closingopt)


let pdfops_of_path_list (paths : path list) : Pdfops.t list =
  paths |> List.map pdfops_of_path |> List.concat


let pdfop_of_stroke_color (stroke_color : color) : Pdfops.t =
  match stroke_color with
  | DeviceRGB(r, g, b)     -> op_RG (r, g, b)
  | DeviceCMYK(c, m, y, k) -> op_K (c, m, y, k)
  | DeviceGray(gray)       -> op_G gray


let pdfop_of_fill_color (fill_color : color) : Pdfops.t =
  match fill_color with
  | DeviceRGB(r, g, b)     -> op_rg (r, g, b)
  | DeviceCMYK(c, m, y, k) -> op_k (c, m, y, k)
  | DeviceGray(gray)       -> op_g gray


let pdfops_of_stroke (line_width : length) (stroke_color : color) (paths : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list paths in
  let op_stroke_color = pdfop_of_stroke_color stroke_color in
  let ops_state = [ op_w line_width; ] in
  let op_draw = op_S in  (* Draws only strokes *)
  List.concat [ [ op_q; op_stroke_color ]; ops_state; ops_path; [ op_draw; op_Q ] ]


let pdfops_of_dashed_stroke (line_width : length) (d1, d2, d0) (stroke_color : color) (paths : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list paths in
  let op_stroke_color = pdfop_of_stroke_color stroke_color in
  let ops_state =
    let op_dashed = Pdfops.Op_d([ !=> d1; !=> d2 ], !=> d0) in
    [ op_w line_width; op_dashed ]
  in
  let op_draw = op_S in
  List.concat [ [ op_q; op_stroke_color ]; ops_state; ops_path; [ op_draw; op_Q ] ]


let pdfops_of_fill (fill_color : color) (paths : path list) : Pdfops.t list =
  let ops_path = pdfops_of_path_list paths in
  let op_fill_color = pdfop_of_fill_color fill_color in
  let op_draw = op_f' in  (* Draws fills by the even-odd rule *)
  List.concat [ [ op_q; op_fill_color ]; ops_path; [ op_draw; op_Q ] ]


let pdfops_of_text (pt : point) (tag : string) (fontsize : length) (color : color) (otxt : OutputText.t) : Pdfops.t list =
  let pdfops_txt =
    otxt |> OutputText.to_TJ_arguments |> List.fold_left (fun acc (h_opt, pdfobjs) ->
      let op_txt = op_TJ (Pdf.Array(pdfobjs)) in
      match h_opt with
      | None ->
          Alist.append acc [ op_Ts Length.zero; op_txt ]

      | Some(FontFormat.PerMille(h)) ->
          let r = fontsize *% (float_of_int h *. 0.001) in
          Alist.append acc [ op_Ts r; op_txt ]

    ) Alist.empty |> Alist.to_list
  in
  List.concat [
    [
      op_q;
      op_cm_translate (Length.zero, Length.zero);
      pdfop_of_text_color color;
      op_BT;
      op_Tm_translate pt;
      op_Tf tag fontsize;
    ];
    pdfops_txt;
    [
      op_ET;
      op_Q;
    ];
  ]


let pdfops_of_image (pt : point) (xratio : float) (yratio : float) (tag : string) : Pdfops.t list =
  [
    op_q;
    op_cm_scale xratio yratio pt;
    op_Do tag;
    op_Q;
  ]


let rec to_pdfops (gr : 'a t) (textf : point -> 'a -> Pdfops.t list) : Pdfops.t list =
  gr |> List.map (function
    | Fill(color, paths)                    -> pdfops_of_fill color paths
    | Stroke(thk, color, paths)             -> pdfops_of_stroke thk color paths
    | DashedStroke(thk, dash, color, paths) -> pdfops_of_dashed_stroke thk dash color paths
    | HorzText(pt, textvalue)               -> textf pt textvalue
    | LinearTrans(pt, mat, gr_sub)          -> pdfops_of_linear_trans pt mat gr_sub textf
    | Clip(paths, gr_sub)                   -> pdfops_of_clip paths gr_sub textf
  ) |> List.concat


and pdfops_of_linear_trans (pt : point) (mat : matrix) (gr_sub : 'a t) (textf : point -> 'a -> Pdfops.t list) : Pdfops.t list =
  List.concat [
    [
      op_q;
      op_cm_linear_trans mat pt;
    ];
    to_pdfops gr_sub textf;
    [
      op_Q;
    ];
  ]


and pdfops_of_clip (paths : path list) (gr_sub : 'a t) (textf : point -> 'a -> Pdfops.t list) : Pdfops.t list =
  List.concat [
    [
      op_q;
      op_cm_translate (Length.zero, Length.zero);
    ];
    pdfops_of_path_list paths;
    [
      Pdfops.Op_W';
      Pdfops.Op_n;
    ];
    to_pdfops gr_sub textf;
    [ op_Q ];
  ]


(* Outputs the bounding box of vertical elements for debugging *)
let pdfops_test_box (color : color) (pt : point) (wid : length) (hgt : length) : Pdfops.t list =
  [
    op_q;
    pdfop_of_stroke_color color;
    op_re pt (wid, Length.negate hgt);
    op_S;
    op_Q;
  ]


(* Outputs the bounding box of horizontal elements for debugging *)
let pdfops_test_frame (color : color) (pt : point) (wid : length) (hgt : length) (dpt : length) : Pdfops.t list =
  let (xpos, ypos_baseline) = pt in
  [
    op_q;
    pdfop_of_stroke_color color;
    op_w (Length.of_pdf_point 0.1);
    op_m (xpos, ypos_baseline);
    op_l (xpos +% wid, ypos_baseline);
    op_re (xpos, ypos_baseline +% hgt) (wid, Length.zero -% (hgt -% dpt));
    op_S;
    op_Q;
  ]


let pdfops_test_skip_fixed (color : color) ((xpos, ypos) : point) (len : length) : Pdfops.t list =
  let thk = Length.of_pdf_point 1. in
  let indent1 = Length.of_pdf_point 2. in
  let indent2 = Length.of_pdf_point 4. in
  let x1 = xpos +% indent1 in
  let x2 = xpos +% indent2 in
  let yB = ypos -% len in
  [
    op_q;
    pdfop_of_stroke_color color;
    op_w thk;
    op_m (x1, ypos); op_l (x1, yB);
    op_m (x2, ypos); op_l (x2, yB);
    op_S;
    op_Q;
  ]


let pdfops_test_skip_between_lines (color : color) ((xpos, ypos) : point) (len : length) : Pdfops.t list =
  let thk = Length.of_pdf_point 1. in
  let indent = Length.of_pdf_point 3. in
  let x = xpos +% indent in
  [
    op_q;
    pdfop_of_stroke_color color;
    op_w thk;
    op_m (x, ypos); op_l (x, ypos -% len);
    op_S;
    op_Q;
  ]


let pdfops_test_skip_margins (color : color) ((xpos, ypos) : point) (len : length) (upper_opt : (bool * length) option) (lower_opt : (bool * length) option) : Pdfops.t list =
  let thk = Length.of_pdf_point 1. in
  let thk_nonbreakable = Length.of_pdf_point 0.2 in
  let indentC = Length.of_pdf_point 5. in
  let gap = Length.of_pdf_point 2. in
  let widhalf = Length.of_pdf_point 1. in
  let xC = xpos +% indentC in
  let xU = xC -% gap in
  let xL = xC +% gap in
  let yB = ypos -% len in
  let pdfopsU =
    match upper_opt with
    | None ->
        []

    | Some((breakableU, lenU)) ->
        let yBU = ypos -% lenU in
        let op_draw = if breakableU then op_f' else op_S in
        [
          op_m (xU -% widhalf, ypos);
          op_l (xU, yBU);
          op_l (xU +% widhalf, ypos);
          op_h; op_draw;
        ]
  in
  let pdfopsL =
    match lower_opt with
    | None ->
        []

    | Some((breakableL, lenL)) ->
        let yTL = ypos -% len +% lenL in
        let op_draw = if breakableL then op_f' else op_S in
        [
          op_m (xL, yTL);
          op_l (xL -% widhalf, yB);
          op_l (xL +% widhalf, yB);
          op_h; op_draw;
        ]
  in
  List.concat [
    [
      op_q;
      pdfop_of_stroke_color color;
      pdfop_of_fill_color color;
      op_w thk;
      op_m (xC, ypos); op_l (xC, yB);
      op_S;
      op_w thk_nonbreakable;
    ];
    pdfopsU;
    pdfopsL;
    [
      op_Q;
    ]
  ]


let pdfops_test_scale (color : color) ((xpos, ypos) : point) (len : length) : Pdfops.t list =
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
