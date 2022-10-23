
open MyUtil
open Types

type error =
  unit (* TODO: define this *)

type 'a ok = ('a, error) result

let main (_absdir : abs_path) : package_info ok =
  let open ResultMonad in
  let package = failwith "TODO: PackageReader" in
  return package
