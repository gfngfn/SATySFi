
open MyUtil
open Types

val process_common : string -> Lexing.lexbuf -> (untyped_source_file, parse_error) result

val process_file : abs_path -> (untyped_source_file, parse_error) result

val process_text : string -> string -> (untyped_source_file, parse_error) result
