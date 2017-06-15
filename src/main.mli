open Types
open Typeenv

val main : Typeenv.t -> environment -> string list -> string -> unit

val error_log_environment : (unit -> unit) -> unit
