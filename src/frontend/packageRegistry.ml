
open ConfigError
open PackageSystemBase


type 'a ok = ('a, config_error) result


let load_cache () : package_context ok =
  failwith "TODO: PackageRegistry.load_cache"
