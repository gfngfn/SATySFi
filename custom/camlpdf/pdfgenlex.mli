(** A very fast lexer for very basic tokens. Used internally by [Pdfread]. *)

(**/**)
type t =
  | LexNull
  | LexBool      of bool
  | LexInt       of int
  | LexReal      of float
  | LexString    of string
  | LexStringHex of string
  | LexName      of string
  | LexLeftSquare
  | LexRightSquare
  | LexLeftDict
  | LexRightDict
  | LexStream    of Pdf.stream
  | LexEndStream
  | LexObj
  | LexEndObj
  | LexR
  | LexComment
  | StopLexing
  | LexNone

val string_of_token : t -> string

val string_of_tokens : t list -> string

val lex_single : (Pdfio.input -> t)

val lex : (Pdfio.input -> t list)

val lex_string : string -> t list

