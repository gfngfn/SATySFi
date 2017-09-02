open HorzBox

type t

val op_m : SkipLength.t * SkipLength.t -> Pdfops.t
val op_l : SkipLength.t * SkipLength.t -> Pdfops.t
val op_RG : float * float * float -> Pdfops.t
val op_re : SkipLength.t * SkipLength.t -> SkipLength.t * SkipLength.t -> Pdfops.t
val op_S : Pdfops.t

val create_empty_pdf : string -> t

val write_page : Pdfpaper.t -> evaled_vert_box list -> t -> t

val write_to_file : t -> unit
