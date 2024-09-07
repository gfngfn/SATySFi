
open MyUtil
open PackageSystemBase
open ConfigError
open ConfigUtil


type 'a ok = ('a, config_error) result

type lock_dependency = {
  depended_lock_name : lock_name;
  used_as            : string;
}

type locked_package = {
  lock_name         : lock_name;
  lock_contents     : Lock.t;
  lock_dependencies : lock_dependency list;
  test_only_lock    : bool;
}

type t = {
  locked_packages            : locked_package list;
  explicit_dependencies      : lock_dependency list;
  explicit_test_dependencies : lock_dependency list;
}


let lock_contents_decoder ~dir:(absdir_lock_config : abs_path) : Lock.t ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "registered" ==> begin
      get "registry_hash_value" registry_hash_value_decoder >>= fun registry_hash_value ->
      get "package_name" package_name_decoder >>= fun package_name ->
      get "version" version_decoder >>= fun locked_version ->
      let registered_package_id = RegisteredPackageId.{ registry_hash_value; package_name } in
      let reglock = RegisteredLock.{ registered_package_id; locked_version } in
      succeed @@ Lock.Registered(reglock)
    end;
    "local" ==> begin
      get "relative_path" string >>= fun relpathstr ->
      succeed @@ Lock.LocalFixed{
        absolute_path = append_to_abs_directory absdir_lock_config relpathstr;
      }
    end;
  ]


let lock_contents_encoder ~dir:(absdir_lock_config : abs_path) (contents : Lock.t) : (string * Yaml.value) list =
  match contents with
  | Lock.Registered(reglock) ->
      let RegisteredLock.{ registered_package_id; locked_version } = reglock in
      let RegisteredPackageId.{ registry_hash_value; package_name } = registered_package_id in
      [
        ("registered", `O([
          ("registry_hash_value", `String(registry_hash_value));
          ("package_name", `String(package_name));
          ("version", `String(SemanticVersion.to_string locked_version));
        ]));
      ]

  | Lock.LocalFixed{ absolute_path } ->
      [
        ("local", `O([
          ("relative_path", `String(AbsPath.make_relative ~from:absdir_lock_config absolute_path));
        ]));
      ]


let lock_dependency_decoder : lock_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun depended_lock_name -> (* Allows arbitrary strings, even ones containing slashes. *)
  get "used_as" uppercased_identifier_decoder >>= fun used_as ->
  succeed { depended_lock_name; used_as }


let lock_dependency_encoder (dep : lock_dependency) : Yaml.value =
  `O([
    ("name", `String(dep.depended_lock_name));
    ("used_as", `String(dep.used_as));
  ])


let lock_decoder ~(dir : abs_path) : locked_package ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun lock_name -> (* Allow arbitrary strings, even ones containing slashes. *)
  get_or_else "dependencies" (list lock_dependency_decoder) [] >>= fun lock_dependencies ->
  get_or_else "test_only" bool false >>= fun test_only_lock ->
  lock_contents_decoder ~dir >>= fun lock_contents ->
  succeed { lock_name; lock_contents; lock_dependencies; test_only_lock }


let lock_encoder ~(dir : abs_path) (lock : locked_package) : Yaml.value =
  let fields_common =
    [
      ("name", `String(lock.lock_name));
      ("dependencies", `A(lock.lock_dependencies |> List.map lock_dependency_encoder));
      ("test_only", `Bool(lock.test_only_lock));
    ]
  in
  let fields_contents = lock_contents_encoder ~dir lock.lock_contents in
  `O(List.append fields_common fields_contents)


let lock_config_decoder ~(dir : abs_path) : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "saphe" (version_checker Constant.current_ecosystem_version) >>= fun () ->
  get_or_else "locks" (list (lock_decoder ~dir)) [] >>= fun locked_packages ->
  get_or_else "dependencies" (list lock_dependency_decoder) [] >>= fun explicit_dependencies ->
  get_or_else "test_dependencies" (list lock_dependency_decoder) [] >>= fun explicit_test_dependencies ->
  succeed { locked_packages; explicit_dependencies; explicit_test_dependencies }


let lock_config_encoder ~(dir : abs_path) (lock_config : t) : Yaml.value =
  let { locked_packages; explicit_dependencies; explicit_test_dependencies } = lock_config in
  let requirement = SemanticVersion.CompatibleWith(Constant.current_ecosystem_version) in
  `O([
    ("saphe", `String(SemanticVersion.requirement_to_string requirement));
    ("locks", `A(locked_packages |> List.map (lock_encoder ~dir)));
    ("dependencies", `A(explicit_dependencies |> List.map lock_dependency_encoder));
    ("test_dependencies", `A(explicit_test_dependencies |> List.map lock_dependency_encoder));
  ])


let load (abspath_lock_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_lock_config
      |> Result.map_error (fun _ -> LockConfigNotFound(abspath_lock_config))
  in
  ConfigDecoder.run (lock_config_decoder  ~dir:(dirname abspath_lock_config)) s
    |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))


let write (abspath_lock_config : abs_path) (lock_config : t) : (unit, config_error) result =
  let yaml = lock_config_encoder ~dir:(dirname abspath_lock_config) lock_config in
  let data = encode_yaml yaml in
  write_file abspath_lock_config data
    |> Result.map_error (fun message ->
      CannotWriteLockConfig{ message; path = abspath_lock_config }
    )
