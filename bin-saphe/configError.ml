
open MyUtil
open PackageSystemBase


type yaml_error =
  | ParseError             of string
  | FieldNotFound          of YamlDecoder.context * string
  | NotAFloat              of YamlDecoder.context
  | NotAString             of YamlDecoder.context
  | NotABool               of YamlDecoder.context
  | NotAnArray             of YamlDecoder.context
  | NotAnObject            of YamlDecoder.context
  | UnexpectedTag          of YamlDecoder.context * string
  | BreaksVersionRequirement of YamlDecoder.context * SemanticVersion.requirement
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
  | CannotBeUsedAsAName of YamlDecoder.context * string
  | UnsupportedConfigFormat of string
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
  | CannotDetermineStoreRoot  of { envvar : string }
  | PackageDirectoryNotFound  of string list
  | PackageConfigNotFound     of abs_path
  | PackageConfigError        of abs_path * yaml_error
  | LockConfigNotFound        of abs_path
  | LockConfigError           of abs_path * yaml_error
  | RegistryConfigNotFound    of abs_path
  | RegistryConfigError       of abs_path * yaml_error
  | StoreRootConfigNotFound   of abs_path
  | StoreRootConfigError      of abs_path * yaml_error
  | LockNameConflict          of lock_name
  | LockedPackageNotFound     of lib_path * abs_path list
  | DependencyOnUnknownLock of {
      depending : lock_name;
      depended  : lock_name;
    }
  | CannotFindLibraryFile     of lib_path * abs_path list
  | CannotSolvePackageConstraints
(*
  | DocumentAttributeError        of DocumentAttribute.error
  | MarkdownClassNotFound         of module_name
  | NoMarkdownConversion          of module_name
  | MoreThanOneMarkdownConversion of module_name
  | MarkdownError                 of MarkdownParser.error
*)
  | FailedToFetchTarball of {
      lock_name   : lock_name;
      exit_status : int;
      command     : string;
    }
  | FailedToExtractTarball of {
      lock_name   : lock_name;
      exit_status : int;
      command     : string;
    }
  | FailedToFetchExternalZip of {
      url         : string;
      exit_status : int;
      command     : string;
    }
  | ExternalZipChecksumMismatch of {
      url      : string;
      path     : abs_path;
      expected : string;
      got      : string;
    }
  | TarGzipChecksumMismatch of {
      lock_name : lock_name;
      url       : string;
      path      : abs_path;
      expected  : string;
      got       : string;
    }
  | FailedToExtractExternalZip of {
      exit_status : int;
      command     : string;
    }
  | FailedToCopyFile of {
      exit_status : int;
      command     : string;
    }
  | PackageRegistryFetcherError   of PackageRegistryFetcher.error
  | CanonicalRegistryUrlError     of CanonicalRegistryUrl.error
