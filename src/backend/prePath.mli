
open HorzBox

type t

val start : point -> t

val line_to : point -> t -> t

val bezier_to : point -> point -> point -> t -> t

val terminate : t -> path

val close_with_line : t -> path

val close_with_bezier : point -> point -> t -> path

val starting_point : t -> point

val current_point : t -> point
