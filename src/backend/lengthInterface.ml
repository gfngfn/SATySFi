
type length = Length.t  [@@deriving show]

type point = length * length

let ( +% ) = Length.add
let ( -% ) = Length.subtr
let ( *% ) = Length.mult
let ( *%! ) l n = l *% (float_of_int n)
let ( /% ) = Length.div
let ( <% ) = Length.less_than
let ( <=% ) = Length.leq
