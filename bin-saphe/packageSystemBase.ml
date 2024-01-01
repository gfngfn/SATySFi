
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

module PackageId = struct
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

module PackageIdMap = Map.Make(PackageId)

module PackageIdSet = Set.Make(PackageId)

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

module Lock = struct
  type t = {
    package_id     : PackageId.t;
    locked_version : SemanticVersion.t;
  }
  [@@deriving show { with_path = false }]

  let compare (lock1 : t) (lock2 : t) : int =
    let { package_id = pkgid1; locked_version = v1 } = lock1 in
    let { package_id = pkgid2; locked_version = v2 } = lock2 in
    let comp_pkgid = PackageId.compare pkgid1 pkgid2 in
    if comp_pkgid <> 0 then
      comp_pkgid
    else
      SemanticVersion.compare v1 v2
end

module LockMap = Map.Make(Lock)

type package_dependency_spec =
  | RegisteredDependency of {
      package_name : package_name;
      registry_local_name : registry_local_name;
      version_requirement : SemanticVersion.requirement;
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

type registry_spec = {
  packages_in_registry : (implementation_record list) PackageIdMap.t;
  registry_hash_value  : registry_hash_value;
}

type package_context = {
  language_version : SemanticVersion.t;
  registries       : registry_spec RegistryLocalNameMap.t;
}

type locked_dependency = {
  depended_lock      : Lock.t;
  dependency_used_as : string;
}

type package_solution = {
  lock                : Lock.t;
  locked_source       : implementation_source;
  locked_dependencies : locked_dependency list;
  used_in_test_only   : bool;
  explicitly_depended : string option;
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

type extraction = {
  extracted_from : string;
  extracted_to   : string;
}

type external_source =
  | ExternalZip of {
      url         : string;
      checksum    : string;
      extractions : extraction list;
    }

type font_spec = {
  font_item_name    : string;
  used_as_math_font : bool;
}
[@@deriving show]

type font_file_contents =
  | OpentypeSingle     of font_spec
  | OpentypeCollection of font_spec list
[@@deriving show]
