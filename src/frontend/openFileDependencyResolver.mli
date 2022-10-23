
open MyUtil
open Types

type error =
  | CyclicFileDependency            of (abs_path * untyped_source_file) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | FailedToParse                   of Range.t

val main : abs_path -> ((abs_path * untyped_source_file) list * PackageNameSet.t, error) result
