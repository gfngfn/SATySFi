
type t = float


let zero = 0.

let add = ( +. )
let subtr = ( -. )
let mult = ( *. )
let div = ( /. )

let max = max
let min = min

let negate x = 0. -. x

let abs x = if x < 0. then -.x else x

let less_than = ( < )

let leq = ( <= )

let is_nearly_zero sl = (sl < 0.01)

let of_pdf_point pt = pt
let to_pdf_point len = len

let convert pdfunit flt =
  let dpi = 72. in  (* temporary; dpi *)
    Pdfunits.convert dpi pdfunit Pdfunits.PdfPoint flt

let of_centimeter = convert Pdfunits.Centimetre
let of_millimeter = convert Pdfunits.Millimetre
let of_inch       = convert Pdfunits.Inch

let pp ppf len =
  Format.fprintf ppf "%spt" (string_of_float len)
