
open MyUtil
open PackageSystemBase


(* TODO: remove this *)
let current_language_version =
  match SemanticVersion.parse "0.1.0" with
  | Some(semver) -> semver
  | None         -> assert false


let lock_tarball_name (package_name : package_name) (locked_version : SemanticVersion.t) : lock_name =
  Printf.sprintf "%s.%s" package_name (SemanticVersion.to_string locked_version)


let lock_directory (lock : Lock.t) : lib_path =
  let Lock.{ registry_hash_value; package_name; locked_version } = lock in
  make_lib_path
    (Printf.sprintf "packages/%s/%s/%s"
      registry_hash_value
      package_name
      (lock_tarball_name package_name locked_version))


let registry_root_directory (registry_hash_value : registry_hash_value) : lib_path =
  make_lib_path (Printf.sprintf "registries/%s" registry_hash_value)


let lock_tarball_cache_directory (registry_hash_value : registry_hash_value) : lib_path =
  make_lib_path (Printf.sprintf "cache/locks/%s" registry_hash_value)


let package_config_file_name =
  "satysfi.yaml"


let library_root_config_file =
  make_lib_path "satysfi-library-root.yaml"


let package_registry_config_file_name =
  "satysfi-registry.yaml"
