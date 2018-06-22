
module Types = Types_
open Types
open HorzBox

val get_right_math_kind : input_context -> math -> math_kind

val get_left_math_kind : input_context -> math -> math_kind

val main : math_context -> math list -> horz_box list

val space_between_maths : math_context -> math list -> math list -> horz_box option
