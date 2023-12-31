
open MyUtil
open PackageSystemBase
open ConfigError
open ConfigUtil


type 'a ok = ('a, config_error) result

type lock_contents =
  | RegisteredLock of {
      registry_hash_value : registry_hash_value;
      package_name        : package_name;
      version             : SemanticVersion.t;
    }

type lock_dependency = {
  depended_lock_name : lock_name;
  used_as            : string;
}

type locked_package = {
  lock_name         : lock_name;
  lock_contents     : lock_contents;
  lock_dependencies : lock_dependency list;
  test_only_lock    : bool;
}

type t = {
  locked_packages     : locked_package list;
  direct_dependencies : lock_dependency list;
}


let lock_contents_decoder : lock_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "registered" ==> begin
      get "registry_hash_value" string >>= fun registry_hash_value ->
      get "package_name" package_name_decoder >>= fun package_name ->
      get "version" version_decoder >>= fun version ->
      succeed @@ RegisteredLock{ registry_hash_value; package_name; version }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let lock_contents_encoder (contents : lock_contents) : Yaml.value =
  match contents with
  | RegisteredLock{ registry_hash_value; package_name; version } ->
      `O([
        ("type", `String("registered"));
        ("registry_hash_value", `String(registry_hash_value));
        ("package_name", `String(package_name));
        ("version", `String(SemanticVersion.to_string version));
      ])


let lock_dependency_decoder : lock_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun depended_lock_name ->
  get "used_as" string >>= fun used_as ->
  succeed { depended_lock_name; used_as }


let lock_decoder : locked_package ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun lock_name ->
  get "contents" lock_contents_decoder >>= fun lock_contents ->
  get_or_else "dependencies" (list lock_dependency_decoder) [] >>= fun lock_dependencies ->
  get_or_else "test_only" bool false >>= fun test_only_lock ->
  succeed { lock_name; lock_contents; lock_dependencies; test_only_lock }


let lock_dependency_encoder (dep : lock_dependency) : Yaml.value =
  `O([
    ("name", `String(dep.depended_lock_name));
    ("used_as", `String(dep.used_as));
  ])


let lock_encoder (lock : locked_package) : Yaml.value =
  `O([
    ("name", `String(lock.lock_name));
    ("contents", lock_contents_encoder lock.lock_contents);
    ("dependencies", `A(lock.lock_dependencies |> List.map lock_dependency_encoder));
    ("test_only", `Bool(lock.test_only_lock));
  ])


let lock_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "ecosystem" (version_checker Constant.current_ecosystem_version) >>= fun () ->
  get_or_else "locks" (list lock_decoder) [] >>= fun locked_packages ->
  get_or_else "dependencies" (list lock_dependency_decoder) [] >>= fun direct_dependencies ->
  succeed {
    locked_packages;
    direct_dependencies;
  }


let lock_config_encoder (lock_config : t) : Yaml.value =
  `O([
    ("ecosystem", `String(SemanticVersion.(requirement_to_string (CompatibleWith(Constant.current_ecosystem_version)))));
    ("locks", `A(lock_config.locked_packages |> List.map lock_encoder));
    ("dependencies", `A(lock_config.direct_dependencies |> List.map lock_dependency_encoder));
  ])


let load (abspath_lock_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_lock_config
      |> Result.map_error (fun _ -> LockConfigNotFound(abspath_lock_config))
  in
  ConfigDecoder.run lock_config_decoder s
    |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))


let write (abspath_lock_config : abs_path) (lock_config : t) : (unit, config_error) result =
  let yaml = lock_config_encoder lock_config in
  let data = encode_yaml yaml in
  write_file abspath_lock_config data
    |> Result.map_error (fun message ->
      CannotWriteLockConfig{ message; path = abspath_lock_config }
    )
