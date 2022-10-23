
open Types

type error =
  unit (* TODO: define this *)

type 'a ok = ('a, error) result

let main (_package_names : PackageNameSet.t) : (package_info list) ok =
  Ok([]) (* TODO: define this *)
