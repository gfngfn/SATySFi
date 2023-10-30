
open MyUtil

type state = {
  input_file : abs_path option;
}

val set : state -> unit

val job_directory : unit -> string
