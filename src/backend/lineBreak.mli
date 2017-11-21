open HorzBox

val main : length -> length -> length -> length -> horz_box list -> intermediate_vert_box list

val natural : horz_box list -> evaled_horz_box list

val get_natural_metrics : horz_box list -> length * length * length
