
open MyUtil
open Types

type error =
  | PackageConfigNotFound of abs_path
  | PackageConfigError    of YamlDecoder.error
  | FailedToParse         of Range.t
  | NotALibraryFile       of abs_path
[@@deriving show]

val main : extensions:(string list) -> abs_path -> (package_info, error) result
