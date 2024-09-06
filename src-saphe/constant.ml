
open MyUtil
open PackageSystemBase


let current_ecosystem_version =
  match SemanticVersion.parse "0.1.0-alpha.1" with
  | Some(semver) -> semver
  | None         -> assert false


let lock_tarball_name (package_name : package_name) (locked_version : SemanticVersion.t) : lock_name =
  Printf.sprintf "%s.%s" package_name (SemanticVersion.to_string locked_version)


let registered_lock_directory ~store_root:(absdir_store_root : abs_path) (reglock : RegisteredLock.t) : abs_path =
  let RegisteredLock.{ registered_package_id; locked_version } = reglock in
  let RegisteredPackageId.{ registry_hash_value; package_name } = registered_package_id in
  append_to_abs_directory absdir_store_root
    (Printf.sprintf "packages/%s/%s/%s"
      registry_hash_value
      package_name
      (lock_tarball_name package_name locked_version))


let lock_directory ~(store_root : abs_path) (lock : Lock.t) : abs_path =
  match lock with
  | Lock.Registered(reglock)         -> registered_lock_directory ~store_root reglock
  | Lock.LocalFixed{ absolute_path } -> absolute_path

(* Should be in sync with SATySFi *)
let envelope_config_path ~dir:(absdir_library : abs_path) : abs_path =
  append_to_abs_directory absdir_library "satysfi-envelope.yaml"


let registered_lock_envelope_config ~(store_root : abs_path) (reglock : RegisteredLock.t) : abs_path =
  envelope_config_path ~dir:(registered_lock_directory ~store_root reglock)


let registry_root_directory_path ~store_root:(absdir_store_root : abs_path) (registry_hash_value : registry_hash_value) : abs_path =
  append_to_abs_directory absdir_store_root (Printf.sprintf "registries/%s" registry_hash_value)


let lock_tarball_cache_directory ~store_root:(absdir_store_root : abs_path) (registry_hash_value : registry_hash_value) : abs_path =
  append_to_abs_directory absdir_store_root (Printf.sprintf "cache/locks/%s" registry_hash_value)


let store_root_config_path ~store_root:(absdir_store_root : abs_path) : abs_path =
  append_to_abs_directory absdir_store_root "saphe-store-root.yaml"


let package_registry_config_path ~registry_dir:(absdir_registry_repo : abs_path) : abs_path =
  append_to_abs_directory absdir_registry_repo "saphe-registry.yaml"


let registry_repo_package_store ~registry_repo_root:(absdir_registry_repo : abs_path) : abs_path =
  append_to_abs_directory absdir_registry_repo "packages"


let release_config_extension =
  ".saphe-release.yaml"


let library_package_config_path ~dir:(absdir_library : abs_path) : abs_path =
  append_to_abs_directory absdir_library "saphe.yaml"


let document_package_config_path ~doc:(abspath_doc : abs_path) : abs_path =
  let path_without_extension = Filename.remove_extension (get_abs_path_string abspath_doc) in
  make_abs_path (Printf.sprintf "%s.saphe.yaml" path_without_extension)


let library_lock_config_path ~dir:(absdir_library : abs_path) : abs_path =
  append_to_abs_directory absdir_library "saphe.lock.yaml"


let library_deps_config_path ~dir:(absdir_library : abs_path) : abs_path =
  append_to_abs_directory absdir_library "satysfi-deps.yaml"


let document_lock_config_path ~doc:(abspath_doc : abs_path) : abs_path =
  let path_without_extension = Filename.remove_extension (get_abs_path_string abspath_doc) in
  make_abs_path (Printf.sprintf "%s.saphe.lock.yaml" path_without_extension)


let default_output_path ~doc:(abspath_doc : abs_path) : abs_path =
  let path_without_extension = Filename.remove_extension (get_abs_path_string abspath_doc) in
  make_abs_path (Printf.sprintf "%s.pdf" path_without_extension)


let document_deps_config_path ~dir:(absdir_doc_root : abs_path) ~(doc_basename : string) : abs_path =
  let doc_basename_without_extension = Filename.remove_extension doc_basename in
  append_to_abs_directory absdir_doc_root
    (Printf.sprintf "%s.satysfi-deps.yaml" doc_basename_without_extension)


let dump_path ~dir:(absdir_doc_root : abs_path) ~(doc_basename : string) : abs_path =
  let doc_basename_without_extension = Filename.remove_extension doc_basename in
  append_to_abs_directory absdir_doc_root
    (Printf.sprintf "%s.satysfi-aux.yaml" doc_basename_without_extension)


let default_intermediate_directory_name : string =
  "./_build"
