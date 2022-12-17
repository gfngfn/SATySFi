
open MyUtil
open PackageSystemBase


let current_language_version =
  match SemanticVersion.parse "0.1.0" with
  | Some(semver) -> semver
  | None         -> assert false


let package_root_directory (registry_hash_value : registry_hash_value) (package_name : package_name) : lib_path =
  make_lib_path (Printf.sprintf "packages/%s/%s" registry_hash_value package_name)


let registry_root_directory (registry_hash_value : registry_hash_value) : lib_path =
  make_lib_path (Printf.sprintf "registries/%s" registry_hash_value)


let cache_locks_directory =
  make_lib_path "cache/locks"


let package_config_file_name =
  "satysfi.yaml"


let library_root_config_file_name =
  "satysfi-library-root.yaml"


let package_registry_config_file_name =
  "satysfi-registry.yaml"
