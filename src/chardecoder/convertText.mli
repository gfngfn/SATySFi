
val to_boxes : HorzBox.input_context -> Uchar.t list -> LineBreakBox.lb_box list

val to_boxes_pure : HorzBox.input_context -> Uchar.t list -> LineBreakBox.lb_pure_box list

val insert_auto_space : LineBreakBox.lb_box list -> LineBreakBox.lb_box list
