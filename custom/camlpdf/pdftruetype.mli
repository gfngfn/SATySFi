(** Convert a TrueType font to a Type 3 Font. *)

(** The type 3 font will not necessarily be valid for inclusion in a PDF file,
this is used for rendering. *)

val to_type3 : Pdf.t -> Pdftext.font -> Pdftext.font

