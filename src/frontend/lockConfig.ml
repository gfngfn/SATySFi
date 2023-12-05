
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

type locked_package = {
  lock_name         : lock_name;
  lock_contents     : lock_contents;
  lock_dependencies : lock_name list;
  test_only_lock    : bool;
}

type t = {
  locked_packages : locked_package list;
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


let lock_decoder : locked_package ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun lock_name ->
  get "contents" lock_contents_decoder >>= fun lock_contents ->
  get_or_else "dependencies" (list string) [] >>= fun lock_dependencies ->
  get_or_else "test_only" bool false >>= fun test_only_lock ->
  succeed { lock_name; lock_contents; lock_dependencies; test_only_lock }


let lock_encoder (lock : locked_package) : Yaml.value =
  `O([
    ("name", `String(lock.lock_name));
    ("contents", lock_contents_encoder lock.lock_contents);
    ("dependencies", `A(lock.lock_dependencies |> List.map (fun lock_name -> `String(lock_name))));
    ("test_only", `Bool(lock.test_only_lock));
  ])


let lock_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "language" language_version_checker >>= fun () ->
  get_or_else "locks" (list lock_decoder) [] >>= fun locked_packages ->
  succeed {
    locked_packages;
  }


let lock_config_encoder (lock_config : t) : Yaml.value =
  `O([
    ("language", `String("^0.1.0"));
    ("locks", `A(lock_config.locked_packages |> List.map lock_encoder));
  ])


let load (abspath_lock_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_lock_config
      |> Result.map_error (fun _ -> LockConfigNotFound(abspath_lock_config))
  in
  ConfigDecoder.run lock_config_decoder s
    |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))


let write (display_config : Logging.config) (abspath_lock_config : abs_path) (lock_config : t) : unit =
  let yaml = lock_config_encoder lock_config in
  match Yaml.to_string ~encoding:`Utf8 ~layout_style:`Block ~scalar_style:`Plain yaml with
  | Ok(data) ->
      Core.Out_channel.write_all (get_abs_path_string abspath_lock_config) ~data;
      Logging.end_lock_output display_config abspath_lock_config

  | Error(_) ->
      assert false
