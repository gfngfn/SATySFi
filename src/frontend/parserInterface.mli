
open MyUtil
open Types

val process_common : abs_path -> Lexing.lexbuf -> (untyped_source_file, parse_error) result

val process_file : abs_path -> (untyped_source_file, parse_error) result

val process_text : abs_path -> string -> (untyped_source_file, parse_error) result
