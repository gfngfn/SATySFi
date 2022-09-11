
open LengthInterface
open GraphicBase


type dash = length * length * length

type 'a t

val empty : 'a t

val concat : ('a t) list -> 'a t

val shift : point -> 'a t -> 'a t

val get_bbox : (point -> 'a -> bbox_corners) -> 'a t -> bbox_corners option

val make_fill : color -> path list -> 'a t

val make_stroke : length -> color -> path list -> 'a t

val make_dashed_stroke : length -> dash -> color -> path list -> 'a t

val make_text : point -> 'a -> 'a t

val make_linear_trans : matrix -> 'a t -> 'a t

val make_clip : 'a t -> path list -> 'a t

val pdfops_of_text : point -> string -> length -> color -> OutputText.t -> Pdfops.t list

val pdfops_of_image : point -> float -> float -> string -> Pdfops.t list

val pdfops_test_box : color -> point -> length -> length -> Pdfops.t list

val pdfops_test_frame : color -> point -> length -> length -> length -> Pdfops.t list

val pdfops_test_skip_fixed : color -> point -> length -> Pdfops.t list

val pdfops_test_skip_between_lines : color -> point -> length -> Pdfops.t list

val pdfops_test_skip_margins : color -> point -> length -> (bool * length) option -> (bool * length) option -> Pdfops.t list

val pdfops_test_scale : color -> point -> length -> Pdfops.t list

val to_pdfops : 'a t -> (point -> 'a -> Pdfops.t list) -> Pdfops.t list
