
open MyUtil
open PackageSystemBase
open Types


type yaml_error =
  | ParseError             of string
  | FieldNotFound          of YamlDecoder.context * string
  | NotAFloat              of YamlDecoder.context
  | NotAString             of YamlDecoder.context
  | NotABool               of YamlDecoder.context
  | NotAnArray             of YamlDecoder.context
  | NotAnObject            of YamlDecoder.context
  | UnexpectedTag          of YamlDecoder.context * string
  | UnexpectedLanguage     of string
  | NotASemanticVersion    of YamlDecoder.context * string
  | NotAVersionRequirement of YamlDecoder.context * string
  | InvalidPackageName     of YamlDecoder.context * string
  | MultiplePackageDefinition of {
      context      : YamlDecoder.context;
      package_name : string;
    }
  | DuplicateRegistryLocalName of {
      context             : YamlDecoder.context;
      registry_local_name : registry_local_name;
    }
  | DuplicateRegistryHashValue of {
      context             : YamlDecoder.context;
      registry_hash_value : registry_hash_value;
    }
  | NotACommand of {
      context : YamlDecoder.context;
      prefix  : char;
      string  : string;
    }

module YamlError = struct
  type t = yaml_error
  let parse_error s = ParseError(s)
  let field_not_found context s = FieldNotFound(context, s)
  let not_a_float context = NotAFloat(context)
  let not_a_string context = NotAString(context)
  let not_a_bool context = NotABool(context)
  let not_an_array context = NotAnArray(context)
  let not_an_object context = NotAnObject(context)
end

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
  | PackageConfigError        of abs_path * yaml_error
  | LockConfigNotFound        of abs_path
  | LockConfigError           of abs_path * yaml_error
  | RegistryConfigNotFound    of abs_path
  | RegistryConfigNotFoundIn  of lib_path * abs_path list
  | RegistryConfigError       of abs_path * yaml_error
  | LibraryRootConfigNotFound of abs_path
  | LibraryRootConfigNotFoundIn of lib_path * abs_path list
  | LibraryRootConfigError    of abs_path * yaml_error
  | LockNameConflict          of lock_name
  | LockedPackageNotFound     of lib_path * abs_path list
  | DependencyOnUnknownLock of {
      depending : lock_name;
      depended  : lock_name;
    }
  | CyclicLockDependency      of (lock_name * untyped_package) cycle
  | NotALibraryFile           of abs_path
  | TypeError                 of TypeError.type_error
  | FileModuleNotFound        of Range.t * module_name
  | FileModuleNameConflict    of module_name * abs_path * abs_path
  | NotADocumentFile          of abs_path * mono_type
  | NotAStringFile            of abs_path * mono_type
  | NoMainModule              of module_name
  | UnknownPackageDependency  of Range.t * module_name
  | CannotFindLibraryFile     of lib_path * abs_path list
  | LocalFileNotFound of {
      relative   : string;
      candidates : abs_path list;
    }
  | CannotSolvePackageConstraints
  | DocumentAttributeError        of DocumentAttribute.error
  | MarkdownClassNotFound         of module_name
  | NoMarkdownConversion          of module_name
  | MoreThanOneMarkdownConversion of module_name
  | MarkdownError                 of MarkdownParser.error
  | LockFetcherError              of LockFetcher.error
  | PackageRegistryFetcherError   of PackageRegistryFetcher.error
  | CanonicalRegistryUrlError     of CanonicalRegistryUrl.error
