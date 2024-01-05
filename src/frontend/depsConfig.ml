
open MyUtil
open EnvelopeSystemBase
open ConfigError
open ConfigUtil


type t = deps_config


let envelope_dependency_decoder : envelope_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun dependency_name ->
  get "used_as" string >>= fun dependency_used_as ->
  succeed { dependency_name; dependency_used_as }


let envelope_spec_decoder : envelope_spec ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun envelope_name ->
  get "path" string >>= fun envelope_path ->
  get "dependencies" (list envelope_dependency_decoder) >>= fun envelope_dependencies ->
  get "test_only" bool >>= fun test_only_envelope ->
  succeed { envelope_name; envelope_path; envelope_dependencies; test_only_envelope }


let deps_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "envelopes" (list envelope_spec_decoder) >>= fun envelopes ->
  get "dependencies" (list envelope_dependency_decoder) >>= fun explicit_dependencies ->
  succeed { envelopes; explicit_dependencies }


let load (abspath_deps_config : abs_path) : (t, config_error) result =
  let open ResultMonad in
  let* s =
    read_file abspath_deps_config
      |> Result.map_error (fun _ -> DepsConfigNotFound(abspath_deps_config))
  in
  ConfigDecoder.run deps_config_decoder s
    |> Result.map_error (fun e -> DepsConfigError(abspath_deps_config, e))
