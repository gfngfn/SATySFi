(** Units and Unit Conversion *)

(** The type of units *)
type t = PdfPoint | Inch | Centimetre | Millimetre | Pixel

(** [convert d u u'] produces a convertor converting from unit [u] to [u'] with
dpi [d] *)
val convert : float -> t -> t -> (float -> float)

