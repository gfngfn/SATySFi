type line =
  | NormalLine  of string
  | DisplayLine of string

type error_category =
  | Lexer
  | Parser
  | Typechecker
  | Evaluator
  | Interface
  | System

val show_error_category : error_category -> string

val report_error : error_category -> line list -> unit

exception ErrorReported

type error_store

val with_error_store : (error_store -> 'a) -> 'a

val record_error : error_store -> error_category -> line list -> unit
