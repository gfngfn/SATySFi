
open Types
open HorzBox

val get_right_math_kind : math_box -> math_kind

val get_left_math_kind : math_box -> math_kind

val main : math_context -> math_box list -> horz_box list

val space_between_maths : math_context -> math_box list -> math_box list -> horz_box option
