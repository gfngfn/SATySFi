
open HorzBox


let (~%) = Length.to_pdf_point
let op_cm (xdiff, ydiff) =
  Pdfops.Op_cm(Pdftransform.matrix_of_transform [Pdftransform.Translate (~% xdiff, ~% ydiff)])

let op_Tm_translate (xpos, ypos) =
  Pdfops.Op_Tm(Pdftransform.matrix_of_transform
                 [Pdftransform.Translate
                     (Length.to_pdf_point xpos, Length.to_pdf_point ypos)])

let op_Tf tag sl = Pdfops.Op_Tf(tag, Length.to_pdf_point sl)
let op_Tj str = Pdfops.Op_Tj(str)
let op_Tj_hex str = Pdfops.Op_Tj_hex(str)
let op_TJ obj = Pdfops.Op_TJ(obj)
let op_BT = Pdfops.Op_BT
let op_ET = Pdfops.Op_ET
let op_m (x, y) = Pdfops.Op_m(~% x, ~% y)
let op_l (x, y) = Pdfops.Op_l(~% x, ~% y)
let op_c (p1, q1) (p2, q2) (x, y) = Pdfops.Op_c(~% p1, ~% q1, ~% p2, ~% q2, ~% x, ~% y)
let op_re (x, y) (w, h) = Pdfops.Op_re(~% x, ~% y, ~% w, ~% h)
let op_S = Pdfops.Op_S
let op_q = Pdfops.Op_q
let op_Q = Pdfops.Op_Q
let op_RG (r, g, b) = Pdfops.Op_RG(r, g, b)


let pdfops_of_elements (elemlst : path_element list) =
  elemlst |> List.map (function
    | LineTo(ptto)                    -> op_l ptto
    | CubicBezierTo(ptc1, ptc2, ptto) -> op_c ptc1 ptc2 ptto
  )


let pdfops_of_path (path : path) : Pdfops.t list =
  let pathops =
    match path with
    | GeneralPath(ptfrom, elemlst) -> (op_m ptfrom) :: (pdfops_of_elements elemlst)
    | Rectangle(pt1, pt2)          -> [op_re pt1 pt2]
  in
    List.append pathops [op_S]


let pdfops_of_path_list (pathlst : path list) : Pdfops.t list =
  pathlst |> List.map pdfops_of_path |> List.concat
