
open LengthInterface
open GraphicBase

exception NotDuringPageBreak

type t =
  | Link of Action.t

val register : t -> ((length * length) * length * length * length) -> (length * color) option -> unit

val add_to_pdf : Pdf.t -> Pdfpage.t -> Pdfpage.t
