(** Reading PDF Files *)

val read_debug : bool ref

(** Read a PDF from a [Pdfio.input], with an optional user password which, if
absent, is assumed to be the empty string, and optional owner password. *)
val pdf_of_input : ?revision:int -> string option -> string option -> Pdfio.input -> Pdf.t

(** Same as [pdf_of_input], but delay loading of streams and parsing of objects
(they will be loaded and parsed when needed). Useful if we only intend to do
something simple, like read metadata.  *)
val pdf_of_input_lazy : ?revision:int -> string option -> string option -> Pdfio.input -> Pdf.t

(** Same as [pdf_of_input], but from an OCaml channel. *)
val pdf_of_channel : ?revision:int -> ?source:string -> string option -> string option -> in_channel -> Pdf.t

(** As [pdf_of_channel], but delay loading of streams and parsing of objects like [pdf_of_input_lazy]. *)
val pdf_of_channel_lazy : ?revision:int -> ?source:string -> string option -> string option -> in_channel -> Pdf.t

(** Read a PDF from the given filename with optional user and owner passwords. *)
val pdf_of_file : ?revision:int -> string option -> string option -> string -> Pdf.t

(** Read the number of revisions of the document, by performing a dummy read. For
example, if this function returns 3, then appropriate values to pass to
[?revision] in a subsequent call to pdf_of_input are 1, 2, and 3. *)
val revisions : Pdfio.input -> int

(**/**)

(* For internal use by other parts of the library *)

val read_header : (Pdfio.input -> int * int)

val lex_stream_data : Pdfio.input -> int -> bool -> Pdfgenlex.t

val getuntil_white_or_delimiter : (Pdfio.input -> char list)

val getuntil_white_or_delimiter_string : (Pdfio.input -> string)

val lex_name : Pdfio.input -> Pdfgenlex.t

val lex_number : Pdfio.input -> Pdfgenlex.t

val lex_string : Pdfio.input -> Pdfgenlex.t

val lex_hexstring : Pdfio.input -> Pdfgenlex.t

val lex_comment : Pdfio.input -> Pdfgenlex.t

val lex_dictionary : Pdfio.input -> Pdfgenlex.t list

val parse : ?failure_is_ok:bool -> Pdfgenlex.t list -> int * Pdf.pdfobject

val dropwhite : Pdfio.input -> unit

val print_lexeme : Pdfgenlex.t -> unit

val string_of_lexeme : Pdfgenlex.t -> string

val parse_single_object : string -> Pdf.pdfobject

(** Return encryption method in use *)
val what_encryption : Pdf.t -> Pdfwrite.encryption_method option

(** Return list of permissions *)
val permissions : Pdf.t -> Pdfcrypt.permission list

(** Given a filename, see if the file is linearized. *)
val is_linearized : Pdfio.input -> bool

