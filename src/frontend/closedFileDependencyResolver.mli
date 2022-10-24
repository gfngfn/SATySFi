
open MyUtil
open Types

type error =
  | FileModuleNotFound   of Range.t * module_name
  | CyclicFileDependency of (abs_path * untyped_library_file) cycle
[@@deriving show]

val main : (abs_path * untyped_library_file) list -> ((abs_path * untyped_library_file) list, error) result
