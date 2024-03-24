
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
  | BranchNotFound         of {
      context       : YamlDecoder.context;
      expected_tags : string list;
      got_tags      : string list;
    }
  | MoreThanOneBranchFound of {
      context       : YamlDecoder.context;
      expected_tags : string list;
      got_tags      : string list;
    }
  | BreaksVersionRequirement of YamlDecoder.context * SemanticVersion.requirement
  | NotASemanticVersion    of YamlDecoder.context * string
  | NotAVersionRequirement of YamlDecoder.context * string
  | InvalidPackageName     of YamlDecoder.context * string
  | DuplicateRegistryHashValue of {
      context             : YamlDecoder.context;
      registry_hash_value : registry_hash_value;
    }
  | CannotBeUsedAsAName of YamlDecoder.context * string
  | UnsupportedRegistryFormat of string
  | NotACommand of {
      context : YamlDecoder.context;
      prefix  : char;
      string  : string;
    }
[@@deriving show { with_path = false }]

module YamlError = struct
  type t = yaml_error
  let parse_error s = ParseError(s)
  let field_not_found context s = FieldNotFound(context, s)
  let not_a_float context = NotAFloat(context)
  let not_a_string context = NotAString(context)
  let not_a_bool context = NotABool(context)
  let not_an_array context = NotAnArray(context)
  let not_an_object context = NotAnObject(context)
  let branch_not_found context expected_tags got_tags =
    BranchNotFound{ context; expected_tags; got_tags }
  let more_than_one_branch_found context expected_tags got_tags =
    MoreThanOneBranchFound{ context; expected_tags; got_tags }
end

type config_error =
  | CannotDetermineStoreRoot  of { envvar : string }
  | PackageDirectoryNotFound  of string list
  | PackageConfigNotFound     of abs_path
  | PackageConfigError        of abs_path * yaml_error
  | NotAPackageButADocument   of abs_path
  | LockConfigNotFound        of abs_path
  | LockConfigError           of abs_path * yaml_error
  | RegistryConfigNotFound    of abs_path
  | RegistryConfigError       of abs_path * yaml_error
  | ReleaseConfigNotFound     of abs_path
  | ReleaseConfigError        of abs_path * yaml_error
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
  | CannotWriteEnvelopeConfig of {
      message : string;
      path    : abs_path;
    }
  | CannotWriteLockConfig of {
      message : string;
      path    : abs_path;
    }
  | CannotWriteDepsConfig of {
      message : string;
      path    : abs_path;
    }
  | CannotWriteStoreRootConfig of {
      message : string;
      path    : abs_path;
    }
  | MultiplePackageDefinition of {
      package_name : string;
    }
  | DuplicateRegistryLocalName of {
      registry_local_name : registry_local_name;
    }
  | UndefinedRegistryLocalName of {
      registry_local_name : registry_local_name;
    }
  | CannotTestDocument
  | FileAlreadyExists of {
      path : abs_path
    }
  | InvalidExtensionForDocument of {
      path      : abs_path;
      extension : string;
    }
  | FailedToWriteFile of {
      path    : abs_path;
      message : string;
    }
  | NotALibraryLocalFixed of {
      dir : abs_path;
    }
  | LocalFixedDoesNotSupportLanguageVersion of {
      dir                  : abs_path;
      language_version     : SemanticVersion.t;
      language_requirement : SemanticVersion.requirement;
    }
