
type t = int
[@@deriving show]

let bottom = 0

let succ lev = lev + 1

let less_than lev1 lev2 = (lev1 < lev2)
