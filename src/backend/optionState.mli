
open MyUtil

type build_state = {
  input_file : abs_path;
}

type test_state = {
  input_file_to_test : abs_path;
}

type command_state =
  | BuildState of build_state
  | TestState  of test_state
  | SolveState

type state = {
  command_state  : command_state;
  show_full_path : bool;
}

val set : state -> unit

val does_show_full_path : unit -> bool

val job_directory : unit -> string
