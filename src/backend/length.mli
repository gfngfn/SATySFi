
type t

val zero : t

val add : t -> t -> t

val subtr : t -> t -> t

val mult : t -> float -> t

val div : t -> t -> float

val max : t -> t -> t

val min : t -> t -> t

val negate : t -> t

val abs : t -> t

val less_than : t -> t -> bool

val leq : t -> t -> bool

val is_nearly_zero : t -> bool

val of_pdf_point : float -> t

val to_pdf_point : t -> float

val of_centimeter : float -> t

val of_millimeter : float -> t

val of_inch : float -> t

val pp : Format.formatter -> t -> unit
