(** Writing PDF Files *)

(** When set to [true], various pieces of information are printed to standard
output when a PDF is written. On library startup, is [false]. *)
val write_debug : bool ref

(** {2 Encryption methods} *)

(** Encryption methods. The boolean for [AES128bit], [AES256bit] and
[AES256BitISO] indicates encryption of metadata or lack thereof.
AlreadyEncrypted is used as a flag to prevent garbage collection internally by
[pdf_to_file_recrypting]. *)
type encryption_method =
  | PDF40bit
  | PDF128bit
  | AES128bit of bool
  | AES256bit of bool
  | AES256bitISO of bool
  | AlreadyEncrypted

(** The type of an encryption with certain user permissions. *)
type encryption = 
  {encryption_method : encryption_method;
   owner_password : string;
   user_password : string;
   permissions : Pdfcrypt.permission list}

(** {2 Writing to outputs, channels and files.} *)

(** Write a PDF document to an [Pdfio.output], optionally encrypting and/or
linearizing. For now, linearization will not preserve object streams. If
[?preserve_objstm] is set (default is false), object streams which were in the
original file will be preserved. If [?create_objstm] is set (default is false),
additional new object streams will be created. To re-encrypt the file using its
existing encryption, provide the user or owner password in the [?recrypt]
argument. The unlabelled boolean argument is true if linearization is required.
*)
val pdf_to_output :
  ?preserve_objstm:bool ->
  ?generate_objstm:bool ->
  ?compress_objstm:bool ->
  ?recrypt:string option ->
  bool -> encryption option -> Pdf.t -> Pdfio.output -> unit

(** As [pdf_to_output] but to an OCaml channel. If the second boolean is set, build a new
/ID (don't set this for encrypted documents). *)
val pdf_to_channel :
  ?preserve_objstm:bool ->
  ?generate_objstm:bool ->
  ?compress_objstm:bool ->
  ?recrypt:string option -> 
  bool -> encryption option -> bool -> Pdf.t -> out_channel -> unit

(** As [pdf_to_channel] but to a named file. *)
val pdf_to_file_options :
  ?preserve_objstm:bool ->
  ?generate_objstm:bool ->
  ?compress_objstm:bool ->
  ?recrypt:string option ->
  bool -> encryption option -> bool -> Pdf.t -> string -> unit

(** Simple write to given file name. Equivalent to [pdf_to_file_options false None true] *)
val pdf_to_file : Pdf.t -> string -> unit

(** {2 String of a PDF object} *)

(** Calculate a string of a pdf object. Due to OCaml's modest limit
on string length, this should be used only when the length of the output is
known to be limited (for example for debug purposes). *)
val string_of_pdf : Pdf.pdfobject -> string

(**/**)

(* For debug, print out the PDFs objects to standard output *)
val debug_whole_pdf : Pdf.t -> unit

