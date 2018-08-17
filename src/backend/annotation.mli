
open LengthInterface


type t =
  | Link of Action.t * length * length * length * length * length


val register_annotation : t -> unit
val add_annotations : Pdf.t -> Pdfpage.t -> Pdfpage.t
