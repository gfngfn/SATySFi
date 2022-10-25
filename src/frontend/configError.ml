
open MyUtil
open Types


type config_error =
  | CyclicFileDependency            of (abs_path * untyped_library_file) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | CannotUseHeaderUse              of Range.t
  | FailedToParse                   of parse_error
  | MainModuleNameMismatch of {
      expected : module_name;
      got      : module_name;
    }
  | PackageDirectoryNotFound  of string list
  | PackageConfigNotFound     of abs_path
  | PackageConfigError        of YamlDecoder.error
  | NotALibraryFile           of abs_path
  | CyclicPackageDependency   of (module_name * package_info) cycle
  | TypeError                 of TypeError.type_error
  | FileModuleNotFound        of Range.t * module_name
  | NotADocumentFile          of abs_path * mono_type
  | NotAStringFile            of abs_path * mono_type
  | NoMainModule              of module_name
  | UnknownPackageDependency  of Range.t * module_name
