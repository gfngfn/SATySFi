
open MyUtil


type state = {
  input_file     : abs_path option;
}


let state = ref None


let set r =
  state := Some(r)


let get () =
  match !state with
  | None    -> assert false
  | Some(r) -> r


let get_input_file () =
  match (get ()).input_file with
  | Some(input_file) -> input_file
  | None             -> assert false


let job_directory () =
  let abspath = get_input_file () in
  Filename.dirname (get_abs_path_string abspath)
