
open MyUtil

type lock_name = string  [@@deriving show]

type lock_info = {
  lock_name         : lock_name;
  lock_dependencies : lock_name list;
  lock_directory    : abs_path;
}
[@@deriving show { with_path = false }]

type package_name = string
[@@deriving show { with_path = false }]

module PackageNameMap = Map.Make(String)

module PackageNameSet = Set.Make(String)

module Lock = struct
  type t = {
    package_name   : package_name;
    locked_version : SemanticVersion.t;
  }
  [@@deriving show { with_path = false }]

  let compare (lock1 : t) (lock2 : t) : int =
    let { package_name = s1; locked_version = v1 } = lock1 in
    let { package_name = s2; locked_version = v2 } = lock2 in
    match String.compare s1 s2 with
    | 0       -> SemanticVersion.compare v1 v2
    | nonzero -> nonzero
end

module LockMap = Map.Make(Lock)

(* The type for names that stand for a package registry.
   Names of this type are supposed to be valid
   within the scope of one package config file. *)
type registry_local_name = string
[@@deriving show { with_path = false }]

module RegistryLocalNameMap = Map.Make(String)

(* The type for MD5 hash values made of a URL and a branch name. *)
type registry_hash_value = string
[@@deriving show { with_path = false }]

module RegistryHashValueMap = Map.Make(String)

type package_dependency =
  | PackageDependency of {
      package_name        : package_name;
      registry_local_name : registry_local_name;
      version_requirement : SemanticVersion.requirement;
    }
[@@deriving show { with_path = false }]

type package_dependency_in_registry =
  | PackageDependencyInRegistry of {
      package_name        : package_name;
      version_requirement : SemanticVersion.requirement;
    }
[@@deriving show { with_path = false }]

type implementation_source =
  | NoSource
  | TarGzip of {
      url : string;
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
  registries : ((implementation_record list) PackageNameMap.t) RegistryLocalNameMap.t;
}

type package_solution = {
  lock                : Lock.t;
  locked_source       : implementation_source;
  locked_dependencies : Lock.t list;
  used_in_test_only   : bool;
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
      lock_name           : lock_name;
      container_directory : abs_path;
      source              : implementation_source;
    }

type registry_remote =
  | GitRegistry of {
      url    : string;
      branch : string;
    }
