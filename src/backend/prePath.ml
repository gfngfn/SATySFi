
open LengthInterface
open HorzBox

(* -- pair of the beginning point and the list of reversed path elements -- *)
type t = point * (point path_element) list


let start pt0 =
  (pt0, [])


let line_to pt prepath =
  let (pt0, pathelemacc) = prepath in
    (pt0, LineTo(pt) :: pathelemacc)


let bezier_to ptS ptT pt prepath =
  let (pt0, pathelemacc) = prepath in
    (pt0, CubicBezierTo(ptS, ptT, pt) :: pathelemacc)


let terminate prepath =
  let (pt0, pathelemacc) = prepath in
    GeneralPath(pt0, List.rev pathelemacc, None)


let close_with_line prepath =
  let (pt0, pathelemacc) = prepath in
    GeneralPath(pt0, List.rev pathelemacc, Some(LineTo(())))


let close_with_bezier ptS ptT prepath =
  let (pt0, pathelemacc) = prepath in
    GeneralPath(pt0, List.rev pathelemacc, Some(CubicBezierTo(ptS, ptT, ())))


let starting_point prepath =
  let (pt0, _) = prepath in
  pt0


let current_point prepath =
  let (pt0, pathelemacc) = prepath in
  match pathelemacc with
  | []                           -> pt0
  | LineTo(pt) :: _              -> pt
  | CubicBezierTo(_, _, pt) :: _ -> pt
