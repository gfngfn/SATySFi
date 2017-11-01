
open HorzBox

val op_cm : point -> Pdfops.t
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

val pdfop_of_text_color : color -> Pdfops.t

val pdfops_of_path_list : path list -> Pdfops.t list

val pdfops_of_graphics : graphics_state -> graphics_command -> path list -> Pdfops.t list

val pdfops_of_stroke : length -> color -> path list -> Pdfops.t list

val pdfops_of_dashed_stroke : length -> length * length * length -> color -> path list -> Pdfops.t list

val pdfops_of_fill : color -> path list -> Pdfops.t list

