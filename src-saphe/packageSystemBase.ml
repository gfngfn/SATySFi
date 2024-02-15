
open MyUtil
open EnvelopeSystemBase

type lock_name = string
[@@deriving show]

type lock_info = {
  lock_name         : lock_name;
  lock_dependencies : lock_name list;
  lock_directory    : abs_path;
}
[@@deriving show { with_path = false }]

type package_name = string
[@@deriving show { with_path = false }]

(* The type for names that stand for a package registry.
   Names of this type are supposed to be valid
   within the scope of one package config file. *)
type registry_local_name = string
[@@deriving show { with_path = false }]

module RegistryLocalNameMap = Map.Make(String)

type parsed_package_dependency_spec =
  | ParsedRegisteredDependency of {
      package_name        : package_name;
      registry_local_name : registry_local_name;
      version_requirement : SemanticVersion.requirement;
    }
  | ParsedLocalFixedDependency of {
      relative_path : string;
    }
[@@deriving show { with_path = false }]

type parsed_package_dependency =
  | ParsedPackageDependency of {
      used_as : string;
      spec    : parsed_package_dependency_spec;
    }
[@@deriving show { with_path = false }]

module RegisteredPackageId = struct
  type t = {
    registry_hash_value : string;
    package_name        : package_name;
  }
  [@@deriving show { with_path = false }]

  let compare (pkgid1 : t) (pkgid2 : t) =
    let { registry_hash_value = h1; package_name = p1 } = pkgid1 in
    let { registry_hash_value = h2; package_name = p2 } = pkgid2 in
    List.compare String.compare [ h1; p1 ] [ h2; p2 ]
end

module PackageId = struct
  type t =
    | Registered of RegisteredPackageId.t
    | LocalFixed of {
        absolute_path : abs_path;
      }
  [@@deriving show { with_path = false }]

  let compare (pkgid1 : t) (pkgid2 : t) =
    match (pkgid1, pkgid2) with
    | ( Registered(regpkgid1), Registered(regpkgid2)) ->
        RegisteredPackageId.compare regpkgid1 regpkgid2

    | (Registered(_), LocalFixed(_)) ->
        1

    | (LocalFixed{ absolute_path = abspath1 }, LocalFixed{ absolute_path = abspath2 }) ->
        AbsPath.compare abspath1 abspath2

    | (LocalFixed(_), Registered(_)) ->
        -1
end

module RegisteredPackageIdMap = Map.Make(RegisteredPackageId)

module LocalFixedPackageIdMap = Map.Make(AbsPath)

module PackageIdMap = Map.Make(PackageId)

module PackageIdSet = Set.Make(PackageId)

(* The type for MD5 hash values made of a URL and a branch name. *)
type registry_hash_value = string
[@@deriving show { with_path = false }]

module RegistryHashValueMap = Map.Make(String)

module RegisteredLock = struct
  type t = {
    registered_package_id : RegisteredPackageId.t;
    locked_version        : SemanticVersion.t;
  }
  [@@deriving show { with_path = false }]

  let compare (reglock1 : t) (reglock2 : t) : int =
    let { registered_package_id = regpkgid1; locked_version = v1 } = reglock1 in
    let { registered_package_id = regpkgid2; locked_version = v2 } = reglock2 in
    let comp_pkgid = RegisteredPackageId.compare regpkgid1 regpkgid2 in
    if comp_pkgid <> 0 then
      comp_pkgid
    else
      SemanticVersion.compare v1 v2
end

module Lock = struct
  type t =
    | Registered of RegisteredLock.t
    | LocalFixed of { absolute_path : abs_path }
  [@@deriving show { with_path = false }]

  let compare (lock1 : t) (lock2 : t) : int =
    match (lock1, lock2) with
    | (Registered(reglock1), Registered(reglock2)) ->
        RegisteredLock.compare reglock1 reglock2

    | (Registered(_), LocalFixed(_)) ->
        -1

    | (LocalFixed{ absolute_path = abspath1 }, LocalFixed{ absolute_path = abspath2 }) ->
        AbsPath.compare abspath1 abspath2

    | (LocalFixed(_), Registered(_)) ->
        1
end

module LockMap = Map.Make(Lock)

type package_dependency_spec =
  | RegisteredDependency of {
      registered_package_id : RegisteredPackageId.t;
      version_requirement   : SemanticVersion.requirement;
    }
  | LocalFixedDependency of {
      absolute_path : abs_path;
    }
[@@deriving show { with_path = false }]

type package_dependency =
  | PackageDependency of {
      used_as : string;
      spec    : package_dependency_spec;
    }
[@@deriving show { with_path = false }]

type package_dependency_in_registry =
  | PackageDependencyInRegistry of {
      package_name        : package_name;
      used_as             : string;
      version_requirement : SemanticVersion.requirement;
    }
[@@deriving show { with_path = false }]

type implementation_source =
  | NoSource
  | TarGzip of {
      url      : string;
      checksum : string;
    }
[@@deriving show { with_path = false }]

type implementation_record =
  | ImplRecord of {
      version              : SemanticVersion.t;
      source               : implementation_source;
      language_requirement : SemanticVersion.requirement;
      dependencies         : package_dependency_in_registry list;
    }

type package_context = {
  language_version         : SemanticVersion.t;
  registered_package_impls : (implementation_record list) RegisteredPackageIdMap.t;
  local_fixed_dependencies : (package_dependency list) LocalFixedPackageIdMap.t;
}

type locked_dependency = {
  depended_lock      : Lock.t;
  dependency_used_as : string;
}
[@@deriving show { with_path = false }]

type package_solution = {
  lock                     : Lock.t;
  locked_source            : implementation_source;
  locked_dependencies      : locked_dependency list;
  used_in_test_only        : bool;
  explicitly_depended      : string option;
  explicitly_test_depended : string option;
}
[@@deriving show { with_path = false }]

type input_kind =
  | InputSatysfi
  | InputMarkdown

type dependency_flag =
  | SourceDependency
  | TestOnlyDependency
[@@deriving show { with_path = false }]

type implementation_spec =
  | ImplSpec of {
      lock   : Lock.t;
      source : implementation_source;
    }

type registry_remote =
  | GitRegistry of {
      url    : string;
      branch : string;
    }
[@@deriving show { with_path = false }]

type extraction = {
  extracted_from : string;
  extracted_to   : string;
}
[@@deriving show { with_path = false }]

type external_resource =
  | ExternalZip of {
      url         : string;
      checksum    : string;
      extractions : extraction list;
    }
[@@deriving show { with_path = false }]

type parsed_package_contents =
  | ParsedLibrary of {
      main_module_name    : string;
      source_directories  : relative_path list;
      test_directories    : relative_path list;
      dependencies        : parsed_package_dependency list;
      test_dependencies   : parsed_package_dependency list;
      markdown_conversion : markdown_conversion option;
    }
  | ParsedFont of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }
  | ParsedDocument of {
      dependencies : parsed_package_dependency list;
    }
[@@deriving show { with_path = false }]

type parsed_package_config = ParsedPackageConfig of {
  language_requirement : SemanticVersion.requirement;
  package_name         : package_name;
  package_authors      : string list;
  registry_specs       : (registry_local_name * registry_remote) list;
  external_resources   : (string * external_resource) list;
  package_contents     : parsed_package_contents;
}
[@@deriving show { with_path = false }]
