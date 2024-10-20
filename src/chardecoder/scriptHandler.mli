
open CharBasis
open LineBreakBox
open HorzBox

val divide_by_script : context_main -> line_break_element list -> line_break_chunk_main list

val get_font_with_ratio : context_main -> script -> font_with_ratio

val get_language_system : context_main -> script -> language_system

val get_string_info : context_main -> script -> horz_string_info
