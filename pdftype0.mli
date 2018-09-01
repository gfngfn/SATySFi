(** Type 0 font support *)

(** Convert an Type 0 composite font containing CFF / Type 1 / Truetype
descendant font into a Type 3 font. Only deals with /Identity-H at the moment.
Used for rendering, not a valid type3 font for inclusion in a PDF. *)
val to_type3 : Pdf.t -> Pdftext.font -> Pdftext.font

