
type file_path = string

val initialize : file_path -> bool

type answer =
  | NeedsAnotherTrial
  | CanTerminate
  | CountMax

val needs_another_trial : file_path -> answer

val register : string -> string -> unit

val get : string -> string option
