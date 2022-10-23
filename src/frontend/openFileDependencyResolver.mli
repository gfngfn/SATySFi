
open MyUtil
open Types

type error =
  | CyclicFileDependency            of (abs_path * file_info) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path

val main : abs_path -> ((abs_path * file_info) list * PackageNameSet.t, error) result
