
open MyUtil
open Types
open ConfigError

val main : extensions:(string list) -> abs_path -> (PackageNameSet.t * (abs_path * untyped_library_file) list * untyped_document_file, config_error) result
