
open MyUtil
open Types

val process_file : abs_path -> (untyped_source_file, Range.t) result

val process_text : string -> string -> (untyped_source_file, Range.t) result
