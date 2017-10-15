
type message_category =
  | LBC | LexHorz | Eval | LineBreak | PageBreak

(* --
  categories of messages printed while debugging;
  you can change this depending on what you would like to test
--  *)
let enabled_list =
  [ LBC; LineBreak; ]


let printS cat msg =
  if List.mem cat enabled_list then print_string msg else ()

let printE cat msg =
  if List.mem cat enabled_list then print_endline msg else ()

(* -- LineBreakDataMap.ml -- *)
let lbc msg      = printS LBC msg
let lbcE msg     = printE LBC msg

(* -- evaluator.ml -- *)
let lexhorz msg  = printS LexHorz msg
let lexhorzE msg = printE LexHorz msg
let eval msg     = printS Eval msg
let evalE msg    = printE Eval msg

(* -- lineBreak.ml -- *)
let linebreakE msg = printE LineBreak msg

(* -- pageBreak.ml -- *)
let pagebreakE msg = printE PageBreak msg
