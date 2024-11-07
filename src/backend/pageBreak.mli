
open MyUtil
open LengthInterface
open HorzBox

exception PageNumberLimitExceeded of int

val solidify : vert_box list -> intermediate_vert_box list

val main : abs_path -> paper_size:(length * length) -> column_hook_func -> page_content_scheme_func -> page_parts_scheme_func -> vert_box list -> HandlePdf.t

val main_multicolumn : abs_path -> paper_size:(length * length) -> length list -> column_hook_func -> column_hook_func -> page_content_scheme_func -> page_parts_scheme_func -> vert_box list -> HandlePdf.t

val adjust_to_first_line : intermediate_vert_box list -> length * length

val adjust_to_last_line : intermediate_vert_box list -> length * length
