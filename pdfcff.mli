(** Convert a CFF Type 1 Font to a Type 3 Font. *)

(** The Type 3 font is not necessarily valid for inclusion in a PDF, we just
intend to use it for rendering. *)
val to_type3 : Pdf.t -> Pdftext.font -> Pdftext.font

