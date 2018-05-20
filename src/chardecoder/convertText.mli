
open CharBasis
open LineBreakBox
open HorzBox

val to_chunks : context_main -> Uchar.t list -> line_break_chunk list

val chunks_to_boxes : (horz_box list -> lb_pure_box list) -> script -> line_break_chunk list -> script -> lb_box list

val chunks_to_boxes_pure : script -> line_break_chunk list -> script -> lb_pure_box list
