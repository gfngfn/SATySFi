
open CharBasis
open LineBreakBox

val to_chunks : HorzBox.input_context -> Uchar.t list -> line_break_chunk list

val chunks_to_boxes : script -> line_break_chunk list -> script -> lb_box list

val chunks_to_boxes_pure : script -> line_break_chunk list -> script -> lb_pure_box list
