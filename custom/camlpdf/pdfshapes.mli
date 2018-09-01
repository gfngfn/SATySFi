(** Basic Shapes *)

(** The factor by which the radius of a circle is multiplied to find the length
of the bezier control lines when approximating quarter arcs to make circles. *)
val kappa : float

(** Calling [restrict_angle s a] restricts an angle [a] to one of those at [s,
2s, 3s...] returning the chosen one. *)
val restrict_angle : float -> float -> float

(** Calling [circle x y r] builds a path representing a circle at [(x, y)] with
radius [r]. *)
val circle : float -> float -> float -> Pdfgraphics.path

(** Calling [rectangle x y w h] builds a path representing a rectangle with top
left [(x, y)], width [w] and height [h]. *)
val rectangle : float -> float -> float -> float -> Pdfgraphics.path

