
open MyUtil

type state = {
  input_file     : abs_path option;
  show_full_path : bool;
}

val set : state -> unit

val does_show_full_path : unit -> bool

val job_directory : unit -> string
