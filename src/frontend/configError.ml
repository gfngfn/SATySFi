
open MyUtil
open Types
open HorzBox


type config_error =
  | CyclicFileDependency            of (abs_path * untyped_library_file) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | CannotUseHeaderUse              of module_name ranged
  | CannotUseHeaderUseOf            of module_name ranged
  | FailedToParse                   of parse_error
  | MainModuleNameMismatch of {
      expected : module_name;
      got      : module_name;
    }
  | PackageDirectoryNotFound  of string list
  | PackageConfigNotFound     of abs_path
  | PackageConfigError        of abs_path * YamlDecoder.error
  | LockConfigNotFound        of abs_path
  | LockConfigError           of abs_path * YamlDecoder.error
  | LockNameConflict          of lock_name
  | DependencyOnUnknownLock of {
      depending : lock_name;
      depended  : lock_name;
    }
  | CyclicLockDependency      of (lock_name * untyped_package) cycle
  | NotALibraryFile           of abs_path
  | TypeError                 of TypeError.type_error
  | FileModuleNotFound        of Range.t * module_name
  | FileModuleNameConflict    of module_name * abs_path
  | NotADocumentFile          of abs_path * mono_type
  | NotAStringFile            of abs_path * mono_type
  | NoMainModule              of module_name
  | UnknownPackageDependency  of Range.t * module_name
  | CannotFindLibraryFile     of lib_path * string list
  | LocalFileNotFound of {
      relative   : string;
      candidates : string list;
    }

type font_error =
  | InvalidFontAbbrev     of font_abbrev
  | InvalidMathFontAbbrev of math_font_abbrev
  | NotASingleFont        of font_abbrev * abs_path
  | NotATTCElement        of font_abbrev * abs_path * int
  | NotASingleMathFont    of math_font_abbrev * abs_path
  | NotATTCMathFont       of math_font_abbrev * abs_path * int
  | ConfigErrorAsToFont   of config_error
