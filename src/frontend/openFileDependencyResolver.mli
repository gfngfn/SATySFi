
open MyUtil
open Types

type error =
  | CyclicFileDependency            of (abs_path * untyped_library_file) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | FailedToParse                   of Range.t

val main : abs_path -> (PackageNameSet.t * (abs_path * untyped_library_file) list * untyped_document_file, error) result
