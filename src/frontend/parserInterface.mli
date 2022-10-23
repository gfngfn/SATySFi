
open MyUtil
open Types

val process_file : abs_path -> (header_element list * Types.untyped_source_file, Range.t) result

val process_text : string -> string -> (header_element list * Types.untyped_source_file, Range.t) result
