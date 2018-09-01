(** Colour Spaces *)

(** A Tristimulus Point *)
type point = float * float * float

(** ICC Based Colour Spaces *)
type iccbased =
 {icc_n : int;
  icc_alternate : t;
  icc_range : float array;
  icc_metadata : Pdf.pdfobject option;
  icc_stream : Pdf.pdfobject}

(** Colour spaces *)
and t =
  | DeviceGray
  | DeviceRGB
  | DeviceCMYK
  | CalGray of point * point * float (* White, Black, Gamma *)
  | CalRGB of point * point * float array * float array (* White, Black, Gamma, Matrix *)
  | Lab of point * point * float array (* White, Black, Range *)
  | ICCBased of iccbased
  | Indexed of t * (int, int list) Hashtbl.t (* Base colourspace, lookup table *)
  | Pattern
  | PatternWithBaseColourspace of t
  | Separation of string * t * Pdffun.t
  | DeviceN of string array * t * Pdffun.t * Pdf.pdfobject

(** Produce a debug string *)
val string_of_colourspace : t -> string

(** Read the name of a colour, if it has one. *)
val name_of_colourspace : t -> string option

(** Read a colourspace from a PDF given a document, page resources dictionary and the colourspace object *)
val read_colourspace : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Write a colourspace to a PDF, returning it. *)
val write_colourspace : Pdf.t -> t -> Pdf.pdfobject

