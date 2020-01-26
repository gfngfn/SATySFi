
open LengthInterface
open GraphicBase

type text = {
  contents : string;
  is_open  : bool;
  kind     : string;
}

type t =
  | Link of Pdfaction.t
  | Text of text

val register : t -> ((length * length) * length * length * length) -> (length * color) option -> unit

val add_to_pdf : Pdf.t -> Pdfpage.t -> Pdfpage.t
