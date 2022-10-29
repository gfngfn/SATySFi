
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

let main ~extensions:(_ : string list) (_lock_config : LockConfig.t) : (untyped_package list) ok =
  let open ResultMonad in
  return [] (* TODO: implement this *)
