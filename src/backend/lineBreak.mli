
open LengthInterface
open HorzBox

val main : bool -> bool -> length -> length -> input_context -> horz_box list -> vert_box list

val natural : horz_box list -> intermediate_horz_box list * length * length

val fit : horz_box list -> length -> intermediate_horz_box list * length * length

val get_natural_metrics : horz_box list -> length * length * length
