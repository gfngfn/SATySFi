
open HorzBox

val solidify : vert_box list -> intermediate_vert_box list

val main : page_scheme -> vert_box list -> (evaled_vert_box list) list

val embed_page_info : page_break_info -> intermediate_horz_box list -> evaled_horz_box list
