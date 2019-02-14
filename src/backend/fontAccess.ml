
open MyUtil

type t =
  | Single     of lib_path
  | Collection of lib_path * int
