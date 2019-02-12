
open MyUtil

val initialize : abs_path -> bool

type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list
  | CountMax

val needs_another_trial : abs_path -> answer

val register : string -> string -> unit

val probe : string -> string option

val get : string -> string option
