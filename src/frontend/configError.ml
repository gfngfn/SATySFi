
open MyUtil
open Types
open HorzBox


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
  | CyclicLockDependency      of (module_name * lock_info) cycle
  | TypeError                 of TypeError.type_error
  | FileModuleNotFound        of Range.t * module_name
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
