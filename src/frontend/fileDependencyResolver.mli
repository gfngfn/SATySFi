
open MyUtil
open Types

exception CyclicFileDependency            of (abs_path * file_info) cycle
exception CannotReadFileOwingToSystem     of string
exception LibraryContainsWholeReturnValue of abs_path
exception DocumentLacksWholeReturnValue   of abs_path

val main : abs_path -> (abs_path * file_info) list
