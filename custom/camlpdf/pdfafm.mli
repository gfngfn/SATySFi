(** Parse Adobe Font Metrics files *)

(** Return the header lines (header item, contents), character metrics
(character, width) and kerning pairs (char1, char2, kerm), and (charname,
width) pairs from an AFM file. May raise [Failure]. *)
val read :
  Pdfio.input ->
  (string * string) list * (int * int) list * (int * int * int) list * (string * int) list
