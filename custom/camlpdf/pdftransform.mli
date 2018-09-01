(** Affine Transformations in Two Dimensions *)

(** {2 Types} *)

(** A single transformation operation. *)
type transform_op =
  | Scale of (float * float) * float * float
  | Rotate of (float * float) * float
  | Translate of float * float
  | ShearX of (float * float) * float
  | ShearY of (float * float) * float

(** A list of transformations, the first at the end of the list (thus, to append
 another transformation, just cons to the beginning.) *)
type transform = transform_op list

(** A transformation matrix (first row [a c e], second row [b d f], third row [0 0 1]) *)
type transform_matrix =
  {a : float; b : float; c : float; d : float; e : float; f : float}

(** The identity transform *) 
val i : transform

(** Make a string of a transform for debug purposes. *)
val string_of_transform : transform -> string

(** {2 Making transformation matrices} *)

(** The identity matrix *)
val i_matrix : transform_matrix

(** String of a transformation matrix. *)
val string_of_matrix : transform_matrix -> string

(** Make a transformation matrix from x and y translation. *)
val mktranslate : float -> float -> transform_matrix

(** Make a transformation matrix from an x and y scale and a point to scale about. *)
val mkscale : (float * float) -> float -> float -> transform_matrix

(** Make a rotation matrix to rotate by a number of radians around a point. *)
val mkrotate : (float * float) -> float -> transform_matrix

(** Matrix to shear in x by a given factor about a point. *)
val mkshearx : (float * float) -> float -> transform_matrix

(** Matrix to shear in y by a given factor about a point. *)
val mksheary : (float * float) -> float -> transform_matrix

(** {2 Composing and manipulating transforms} *)

(** [compose t ts] adds operation [t] to the transform [ts]. *)
val compose : transform_op -> transform -> transform

(** [append a b] is a transform with the same effect as performing b then a *)
val append : (transform -> transform -> transform)

(** [compose a b] produces a matrix equivalent to performing [b] then [a]. *)
val matrix_compose : transform_matrix -> transform_matrix -> transform_matrix

(** Make a matrix from a single transformation operation *)
val matrix_of_op : transform_op -> transform_matrix

(** Make a matrix from a transform *)
val matrix_of_transform : transform -> transform_matrix

(** {2 Inverting transforms} *)

(** Exception raised if a matrix is non-invertable *)
exception NonInvertable

(** Matrix inversion. May raise [NonInvertable] *)
val matrix_invert : transform_matrix -> transform_matrix

(** Transform a coordinate by a given transform. *) 
val transform : transform -> float * float -> float * float

(** Transform a coordinate by a given transformation matrix. *)
val transform_matrix : transform_matrix -> float * float -> float * float

(** {2 Decomposing and recomposing matrices} *)

(** Decompose a transformation matrix to scale, aspect, rotation, shear,
translation in x, translation in y. Always succeeds, but results are not
guaranteed to mean anything. *)
val decompose :
  transform_matrix -> float * float * float * float * float * float

(** Recompose from the above information. It is not guaranteed that recompose
(decompose t) = t *)
val recompose :
  float -> float -> float -> float -> float -> float -> transform_matrix

