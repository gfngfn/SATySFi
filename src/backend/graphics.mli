
open LengthInterface
open GraphicBase

(*
val op_cm_translate : point -> Pdfops.t
val op_cm_scale : float -> float -> point -> Pdfops.t
val op_Tm_translate : point -> Pdfops.t
val op_Tf : string -> length -> Pdfops.t
val op_Tj : string -> Pdfops.t
val op_Tj_hex : string -> Pdfops.t
val op_TJ : Pdf.pdfobject -> Pdfops.t
val op_Ts : length -> Pdfops.t

val op_BT : Pdfops.t
val op_ET : Pdfops.t

val op_m : point -> Pdfops.t
val op_l : point -> Pdfops.t
val op_c : point -> point -> point -> Pdfops.t
val op_RG : float * float * float -> Pdfops.t
val op_re : point -> point -> Pdfops.t
val op_q : Pdfops.t
val op_Q : Pdfops.t
val op_S : Pdfops.t
val op_Do : string -> Pdfops.t
*)

type dash = length * length * length

type 'a element

type 'a t

val empty : 'a t

val extend : 'a t -> 'a element -> 'a t

val singleton : 'a element -> 'a t

val make_fill : color -> path list -> 'a element

val make_stroke : length -> color -> path list -> 'a element

val make_dashed_stroke : length -> dash -> color -> path list -> 'a element

val make_text : point -> 'a -> 'a element

val pdfops_of_text : point -> length -> string -> length -> color -> OutputText.t -> Pdfops.t list

val pdfops_of_image : point -> float -> float -> string -> Pdfops.t list

val pdfops_test_box : color -> point -> length -> length -> Pdfops.t list

val pdfops_test_frame : point -> length -> length -> length -> Pdfops.t list

val to_pdfops : 'a t -> (point -> 'a -> Pdfops.t list) -> Pdfops.t list
(*
val pdfop_of_text_color : color -> Pdfops.t

val pdfops_of_path_list : path list -> Pdfops.t list
(*
val pdfops_of_graphics : graphics_state -> graphics_command -> path list -> Pdfops.t list
*)
va<l pdfops_of_stroke : length -> color -> path list -> Pdfops.t list

val pdfops_of_dashed_stroke : length -> length * length * length -> color -> path list -> Pdfops.t list

val pdfops_of_fill : color -> path list -> Pdfops.t list
*)
