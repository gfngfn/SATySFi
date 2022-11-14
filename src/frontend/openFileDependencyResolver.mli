
open MyUtil
open Types
open PackageSystemBase
open ConfigError

val main : extensions:(string list) -> input_kind -> PackageConfig.t GlobalTypeenv.t -> abs_path -> ((abs_path * untyped_library_file) list * untyped_document_file, config_error) result
