(** PDF Jpeg Support *)

(** Return the JPEG data starting at the current position in the [Pdfio.input],
leaving the input ready to read the first byte following the JPEG data. *)
val get_jpeg_data : Pdfio.input -> Pdfio.bytes

