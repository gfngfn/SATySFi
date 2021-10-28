
open LengthInterface
open GraphicBase


type dash = length * length * length

type 'a element

type 'a t

val empty : 'a t

val extend : 'a t -> 'a element -> 'a t

val singleton : 'a element -> 'a t

val shift_element : point -> 'a element -> 'a element

val get_element_bbox : (point -> 'a -> point * point) -> 'a element -> point * point

val make_fill : color -> path list -> 'a element

val make_stroke : length -> color -> path list -> 'a element

val make_dashed_stroke : length -> dash -> color -> path list -> 'a element

val make_text : point -> 'a -> 'a element

val make_linear_trans : (float * float * float * float) -> 'a element -> 'a element

val pdfops_of_text : point -> string -> length -> color -> OutputText.t -> Pdfops.t list

val pdfops_of_image : point -> float -> float -> string -> Pdfops.t list

val pdfops_test_box : color -> point -> length -> length -> Pdfops.t list

val pdfops_test_frame : color -> point -> length -> length -> length -> Pdfops.t list

val pdfops_test_skip_fixed : color -> point -> length -> Pdfops.t list

val pdfops_test_skip_between_lines : color -> point -> length -> Pdfops.t list

val pdfops_test_skip_margins : color -> point -> length -> (bool * length) option -> (bool * length) option -> Pdfops.t list

val pdfops_test_scale : color -> point -> length -> Pdfops.t list

val to_pdfops : 'a t -> (point -> 'a -> Pdfops.t list) -> Pdfops.t list

val clip_graphics : 'a element -> path list -> 'a element
