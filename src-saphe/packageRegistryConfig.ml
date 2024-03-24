
open MyUtil
open ConfigError
open ConfigUtil


type 'a ok = ('a, config_error) result

type t = unit


let registry_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "registry_format" string >>= function
  | "1"    -> succeed ()
  | format -> failure (fun _yctx -> UnsupportedRegistryFormat(format))


let load (abspath_registry_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_registry_config
      |> Result.map_error (fun _ -> RegistryConfigNotFound(abspath_registry_config))
  in
  ConfigDecoder.run registry_config_decoder s
    |> Result.map_error (fun e -> RegistryConfigError(abspath_registry_config, e))
