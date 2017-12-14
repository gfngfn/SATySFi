
val to_chunks : HorzBox.input_context -> Uchar.t list -> LineBreakBox.line_break_chunk list
(*
val to_boxes_pure : HorzBox.input_context -> Uchar.t list -> LineBreakBox.lb_pure_box list
*)
val insert_auto_space : LineBreakBox.lb_box list -> LineBreakBox.lb_box list

val chunks_to_boxes : LineBreakBox.line_break_chunk list -> LineBreakBox.lb_box list

val chunks_to_boxes_pure : LineBreakBox.line_break_chunk list -> LineBreakBox.lb_pure_box list
