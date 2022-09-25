
type length = Length.t
[@@deriving show]

type point = length * length
[@@deriving show]

type stretchable =
  | FiniteStretch of length
  | Fils          of int

type length_info = {
  natural     : length;
  shrinkable  : length;
  stretchable : stretchable;
}


let ( +% ) = Length.add
let ( -% ) = Length.subtr
let ( *% ) = Length.mult
let ( *%! ) l n = l *% (float_of_int n)
let ( /% ) = Length.div
let ( <% ) = Length.less_than
let ( <=% ) = Length.leq


let ( !=> ) = Length.to_pdf_point
let ( !<= ) = Length.of_pdf_point


let add_stretchable strc1 strc2 =
  match (strc1, strc2) with
  | (FiniteStretch(w1), FiniteStretch(w2)) -> FiniteStretch(w1 +% w2)
  | (Fils(i1), Fils(i2))                   -> Fils(i1 + i2)
  | (Fils(i1), _)                          -> Fils(i1)
  | (_, Fils(i2))                          -> Fils(i2)
