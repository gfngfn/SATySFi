
open HorzBox

val solidify : vert_box list -> intermediate_vert_box list

val main : (page_break_info -> page_content_scheme) -> vert_box list -> page list
(*
val embed_page_info : page_break_info -> intermediate_horz_box list -> evaled_horz_box list

val embed_page_info_vert : page_break_info -> intermediate_vert_box list -> evaled_vert_box list
*)
