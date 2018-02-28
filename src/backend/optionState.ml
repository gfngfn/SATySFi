

let debug_show_bbox_ref = ref false


let set_debug_show_bbox () =
  debug_show_bbox_ref := true


let debug_show_bbox () =
  !debug_show_bbox_ref
