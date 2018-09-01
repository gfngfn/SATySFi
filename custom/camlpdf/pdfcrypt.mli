(** Decrypting PDF files *)

(** Note that encryption depends on fixed object and generation numbers: don't
change these (for example by calling [Pdf.remove_unreferenced] on the PDF)
before writing.

Encryption support is part of the [Pdfwrite] module, since files are encrypted when they're written. *)

(** If this is set, various debug is produced on standard output. On startup, is set to false *)
val crypt_debug : bool ref

(* Permissions. *)
type permission =
  | NoEdit 
  | NoPrint
  | NoCopy 
  | NoAnnot
  | NoForms
  | NoExtract
  | NoAssemble
  | NoHqPrint

(** Decrypt a PDF document, given the user password, returning the permissions
under which the document was encrypted. *)
val decrypt_pdf : ?keyfromowner:string -> string -> Pdf.t -> Pdf.t option * permission list

(** Decrypt a PDF document, given the owner password. *)
val decrypt_pdf_owner : string -> Pdf.t -> Pdf.t option

(** Is a PDF encrypted? *)
val is_encrypted : Pdf.t -> bool

(**/**)
(* only for the use of PDFWrite *)

(** [recrypt_pdf decrypted_and_modified] re-encrypts a PDF document
which was decrypted using the user password and owner password from the
original encrypted file and the same permissions and encryption parameters. *)
val recrypt_pdf : ?renumber:bool -> Pdf.t -> string -> Pdf.t

(** Encrypt a PDF documnent, using 40 bit encryption, with given user and
owner passwords. *)
val encrypt_pdf_40bit : string -> string -> permission list -> Pdf.t -> Pdf.t

(** Ditto for 128 bit encryption *)
val encrypt_pdf_128bit : string -> string -> permission list -> Pdf.t -> Pdf.t

(** Encrypt a file using the AESV2 Crypt filter *)
val encrypt_pdf_AES : bool -> string -> string -> permission list -> Pdf.t -> Pdf.t

(** Encrypt a file using the AESV3 Crypt filter *)
val encrypt_pdf_AES256 : bool -> string -> string -> permission list -> Pdf.t -> Pdf.t

(** Encrypt a file using the AESV4 (ISO) Crypt filter *)
val encrypt_pdf_AES256ISO : bool -> string -> string -> permission list -> Pdf.t -> Pdf.t

(* only for the use of Pdfread *)
val decrypt_single_stream :
  string option -> string option -> Pdf.t -> int -> int -> Pdf.pdfobject -> Pdf.pdfobject

(* Don't call on an unencrypted PDF *)
val get_encryption_values : Pdf.t -> Pdfcryptprimitives.encryption * string * string * int32 * string * string option * string option

val banlist_of_p : int32 -> permission list

val string_of_pdf : (Pdf.pdfobject -> string) ref

