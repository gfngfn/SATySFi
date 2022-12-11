
let current_language_version =
  match SemanticVersion.parse "0.1.0" with
  | Some(semver) -> semver
  | None         -> assert false


let packages_directory =
  "packages"


let registries_directory =
  "registries"


let cache_directory =
  "cache"


let cache_locks_directory =
  "cache/locks"


let package_config_file_name =
  "satysfi.yaml"


let library_root_config_file_name =
  "satysfi-library-root.yaml"


let package_registry_config_file_name =
  "satysfi-registry.yaml"
