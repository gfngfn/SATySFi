
open LineBreakBox

val to_chunks : HorzBox.input_context -> Uchar.t list -> line_break_chunk list

val chunks_to_boxes : CharBasis.script -> line_break_chunk list -> CharBasis.script -> lb_box list

val chunks_to_boxes_pure : line_break_chunk list -> lb_pure_box list
