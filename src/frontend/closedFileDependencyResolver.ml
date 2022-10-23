
open MyUtil
open Types

type error =
  unit (* TODO: define this *)

type 'a ok = ('a, error) result


let main (_utlibs : (abs_path * untyped_library_file) list) : ((abs_path * untyped_library_file) list) ok =
  let open ResultMonad in
  return []
