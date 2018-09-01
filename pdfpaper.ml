(* Type for paper sizes --- unit, width, height. *)
type t = Paper of (Pdfunits.t * float * float)

let make u w h = Paper (u, w, h)

let unit (Paper (u, _, _)) = u
let width (Paper (_, w, _)) = w
let height (Paper (_, _, h)) = h

(* Make a paper size landscape *)
let landscape (Paper (u, w, h)) = Paper (u, h, w)

(* European `A' sizes. *)
let a0 = Paper (Pdfunits.Millimetre, 841., 1189.)
let a1 = Paper (Pdfunits.Millimetre, 594., 841.)
let a2 = Paper (Pdfunits.Millimetre, 420., 594.)
let a3 = Paper (Pdfunits.Millimetre, 297., 420.)
let a4 = Paper (Pdfunits.Millimetre, 210., 297.)
let a5 = Paper (Pdfunits.Millimetre, 148., 210.)
let a6 = Paper (Pdfunits.Millimetre, 105., 148.)
let a7 = Paper (Pdfunits.Millimetre, 74., 105.)
let a8 = Paper (Pdfunits.Millimetre, 52., 74.)
let a9 = Paper (Pdfunits.Millimetre, 37., 52.)
let a10 = Paper (Pdfunits.Millimetre, 26., 37.)

(* US Imperial sizes. *)
let usletter = Paper (Pdfunits.Inch, 8.5, 11.)
let uslegal = Paper (Pdfunits.Inch, 8.5, 14.)


