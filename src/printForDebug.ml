
type message_category =
  | LBC | LexHorz | Eval | LineBreak | PageBreak | Kern | Lexer | Main | FontFormat | EmbVert | InitFont

(* --
  categories of messages printed while debugging;
  you can change this depending on what you would like to test
--  *)
let enabled_list =
  []
(*
  [ LBC; LineBreak; LexHorz; Lexer; ]
*)

let printS cat msg =
  if List.mem cat enabled_list then print_string msg else ()

let printE cat msg =
  if List.mem cat enabled_list then print_endline msg else ()

(*
let printP cat =
  if List.mem cat enabled_list then
    Format.printf
  else
    Format.fprintf (Format.formatter_of_out_channel (open_out "/dev/null"))
*)

let formatter cat =
  if List.mem cat enabled_list then Format.std_formatter else Format.formatter_of_out_channel (open_out "/dev/null")


(* -- LineBreakDataMap.ml -- *)
let lbc msg      = printS LBC msg
let lbcE msg     = printE LBC msg

(* -- evaluator.ml -- *)
let lexhorz msg  = printS LexHorz msg
let lexhorzE msg = printE LexHorz msg
let eval msg     = printS Eval msg
let evalE msg    = printE Eval msg
let embvertE msg = printE EmbVert msg

(* -- lineBreak.ml -- *)
let linebreakE msg = printE LineBreak msg

(* -- pageBreak.ml -- *)
let pagebreakE msg = printE PageBreak msg
let pagebreakF = formatter PageBreak

(* -- fontInfo.ml / fontFormat.ml -- *)
let kernE msg = printE Kern msg
let fontfmtE msg = printE FontFormat msg
let initfontE msg = printE InitFont msg

(* -- lexer.mll -- *)
let lexerE msg = printE Lexer msg

(* -- main.ml -- *)
let mainE msg = printE Main msg
