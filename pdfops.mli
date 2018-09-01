(** Parsing PDF Graphics Streams *)

(** A flat representation of the PDF graphics stream operators. Where the operator
contains the asterisk character (not allowable in OCaml syntax), a prime is
substituted. *)
type t =
  | Op_w of float
  | Op_J of int
  | Op_j of int
  | Op_M of float 
  | Op_d of float list * float
  | Op_ri of string
  | Op_i of int
  | Op_gs of string
  | Op_q
  | Op_Q
  | Op_cm of Pdftransform.transform_matrix
  | Op_m of float * float
  | Op_l of float * float
  | Op_c of float * float * float * float * float * float
  | Op_v of float * float * float * float
  | Op_y of float * float * float * float
  | Op_h
  | Op_re of float * float * float * float
  | Op_S
  | Op_s
  | Op_f
  | Op_F
  | Op_f'
  | Op_B
  | Op_B'
  | Op_b
  | Op_b'
  | Op_n
  | Op_W
  | Op_W'
  | Op_BT
  | Op_ET
  | Op_Tc of float
  | Op_Tw of float
  | Op_Tz of float
  | Op_TL of float
  | Op_Tf of string * float
  | Op_Tr of int
  | Op_Ts of float
  | Op_Td of float * float
  | Op_TD of float * float
  | Op_Tm of Pdftransform.transform_matrix
  | Op_T'
  | Op_Tj of string
  | Op_Tj_hex of string
  | Op_TJ of Pdf.pdfobject
  | Op_' of string
  | Op_'' of float * float * string
  | Op_d0 of float * float
  | Op_d1 of float * float * float * float * float * float
  | Op_CS of string
  | Op_cs of string
  | Op_SC of float list 
  | Op_sc of float list 
  | Op_SCN of float list
  | Op_scn of float list
  | Op_SCNName of string * float list
  | Op_scnName of string * float list
  | Op_G of float
  | Op_g of float
  | Op_RG of float * float * float
  | Op_rg of float * float * float
  | Op_K of float * float * float * float
  | Op_k of float * float * float * float
  | Op_sh of string
  | InlineImage of (Pdf.pdfobject * Pdfio.bytes)
  | Op_Do of string
  | Op_MP of string
  | Op_DP of string * Pdf.pdfobject 
  | Op_BMC of string 
  | Op_BDC of string * Pdf.pdfobject 
  | Op_EMC 
  | Op_BX
  | Op_EX
  | Op_Unknown of string

(** Parse a single byte stream to an operator list given a document and resource
dictionary. *)
val parse_stream :
  Pdf.t -> Pdf.pdfobject -> Pdfio.bytes list -> t list

(** Given a pdf document, resource dictionary and list of streams representing
the graphics content (PDF allows a single page's graphics content to be split
over several streams), return a list of operators. Raises PDFError on bad
content. *)
val parse_operators :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject list -> t list

(** Flatten a list of operators to an uncompressed PDF stream. *)
val stream_of_ops : t list -> Pdf.pdfobject

(** Make a string of a single operator (for debug purposes). *)
val string_of_op : t -> string

(** Same as [string_of_op], but of several operators (for debug purposes). *)
val string_of_ops : t list -> string

(** Given a pdf, a resources dictionary and a colourspace dictionary, give the
number of bytes per pixel in the stored image data. *)
val components : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> int

(**/**)
val debug : bool ref

