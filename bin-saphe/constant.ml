
open MyUtil
open PackageSystemBase


let current_ecosystem_version =
  match SemanticVersion.parse "0.0.1" with
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


let library_root_config_path (absdir_library_root : abs_path) : abs_path =
  make_abs_path (Filename.concat (get_abs_path_string absdir_library_root) "saphe-library-root.yaml")


let package_registry_config_file_name =
  "saphe-registry.yaml"


let library_package_config_path (absdir_library : abs_path) : abs_path =
  make_abs_path (Filename.concat (get_abs_path_string absdir_library) "saphe.yaml")


let document_package_config_path (abspath_doc : abs_path) : abs_path =
  let path_without_extension = Filename.remove_extension (get_abs_path_string abspath_doc) in
  make_abs_path (Printf.sprintf "%s.saphe.yaml" path_without_extension)


let library_lock_config_path (absdir_library : abs_path) : abs_path =
  make_abs_path (Filename.concat (get_abs_path_string absdir_library) "saphe.lock.yaml")


let document_lock_config_path (abspath_doc : abs_path) : abs_path =
  let path_without_extension = Filename.remove_extension (get_abs_path_string abspath_doc) in
  make_abs_path (Printf.sprintf "%s.saphe.lock.yaml" path_without_extension)


(* Should be in sync with SATySFi *)
let envelope_config_path (absdir_library : abs_path) : abs_path =
  make_abs_path (Filename.concat (get_abs_path_string absdir_library) "satysfi-envelope.yaml")