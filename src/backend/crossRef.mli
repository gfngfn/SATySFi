
val initialize : unit -> unit

type answer =
  | NeedsAnotherTrial
  | CanTerminate
  | CountMax

val needs_another_trial : unit -> answer

val register : string -> string -> unit

val get : string -> string option
