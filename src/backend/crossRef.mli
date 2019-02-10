
open MyUtil

exception InvalidYOJSON                of abs_path * string
exception DumpFileOtherThanAssoc       of abs_path
exception DumpFileValueOtherThanString of abs_path * string * string

val initialize : abs_path -> bool

type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list
  | CountMax

val needs_another_trial : abs_path -> answer

val register : string -> string -> unit

val probe : string -> string option

val get : string -> string option
