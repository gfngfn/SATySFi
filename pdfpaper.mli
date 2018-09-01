(** Media Sizes *)

(** A paper size consists of its unit, width and height. *)
type t

(** Make a paper size given its unit, width and height. *)
val make : Pdfunits.t -> float -> float -> t

(** Project the unit from a paper size *)
val unit : t -> Pdfunits.t

(** Project the width from a paper size. *)
val width : t -> float

(** Project the height from a paper size. *)
val height : t -> float

(** Flip a paper size between landscape and portrait, swapping its dimensions. *)
val landscape : t -> t

val a0 : t
val a1 : t
val a2 : t
val a3 : t
val a4 : t
val a5 : t
val a6 : t
val a7 : t
val a8 : t
val a9 : t
val a10 : t
(** ISO A series paper sizes, portrait. *)

val usletter : t
val uslegal : t
(** United States paper sizes. portrait. *)

