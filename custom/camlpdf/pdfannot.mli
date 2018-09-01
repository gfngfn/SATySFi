(** Annotations *)

(** Border styles *)
type style = NoStyle | Solid | Dashed | Beveled | Inset | UnderlineStyle

(** Annotation borders. *)
type border =
  {width : float;
   vradius : float;
   hradius : float;
   style : style;
   dasharray : int array}

(** Annotation types *)
type subtype =
  | Text
  | Link
  | FreeText
  | Line
  | Square
  | Circle
  | Polygon
  | PolyLine
  | Highlight
  | Underline
  | Squiggly
  | StrikeOut
  | Stamp
  | Caret
  | Ink
  | Popup of t
  | FileAttachment
  | Sound
  | Movie
  | Widget
  | Screen
  | PrinterMark
  | TrapNet
  | Watermark
  | ThreeDee
  | Unknown

(** Annotations. *)
and t =
  {subtype : subtype;
   annot_contents : string option;
   subject : string option;
   rectangle : float * float * float * float;
   border : border;
   colour : (int * int * int) option;
   annotrest : Pdf.pdfobject}

val annotations_of_page : Pdf.t -> Pdfpage.t -> t list
(** Return the annotations on a page in a document. *)

val add_annotation : Pdf.t -> Pdfpage.t -> t -> Pdfpage.t
(** Add an annotation to a page in a document. *)

val make_border : ?vradius:float ->
?hradius:float -> ?style:style -> ?dasharray:int array -> float -> border
(** Make a border. *)

val make : ?content:string ->
  ?border:border ->
  ?rectangle:float * float * float * float ->
  ?colour:int * int * int -> ?subject:string -> subtype -> t
(** Make an annotation of a given [subtype]. *)
