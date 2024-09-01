
open MyUtil

val initialize : abs_path -> bool

val reset : unit -> unit

type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list

val judge_termination : unit -> answer

val write_dump_file : abs_path -> unit

val register : string -> string -> unit

val probe : string -> string option

val get : string -> string option
