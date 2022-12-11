
open MyUtil


type run_result = {
  exit_status : int;
  command     : string;
}


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let mkdir_p (absdir : abs_path) : unit =
  Core_unix.mkdir_p (get_abs_path_string absdir)


let run_wget
  ~(wget_command : string)
  ~(url : string)
  ~(output : abs_path)
=
  let command =
    Printf.sprintf "\"%s\" -O \"%s\" \"%s\""
      (escape_string wget_command)
      (escape_string (get_abs_path_string output))
      (escape_string url)
  in
  let exit_status = Sys.command command in
  { exit_status; command }


let run_tar_xzf_strip_components_1
  ~(tar_command : string)
  ~(tarball : abs_path)
  ~(output_dir : abs_path)
=
  let command =
    Printf.sprintf "\"%s\" -xzf \"%s\" -C \"%s\" --strip-components 1"
      (escape_string tar_command)
      (escape_string (get_abs_path_string tarball))
      (escape_string (get_abs_path_string output_dir))
  in
  let exit_status = Sys.command command in
  { exit_status; command }


let run_git_pull
  ~(git_command : string)
  ~(repo_dir : abs_path)
  ~(url : string)
  ~(branch : string)
=
  let command =
    Printf.sprintf "\"%s\" -C \"%s\" pull \"%s\" \"%s\""
      (escape_string git_command)
      (escape_string (get_abs_path_string repo_dir))
      (escape_string url)
      (escape_string branch)
  in
  let exit_status = Sys.command command in
  { exit_status; command }


let run_git_clone
  ~(git_command : string)
  ~(repo_dir : abs_path)
  ~(url : string)
  ~(branch : string)
=
  let command =
    Printf.sprintf "\"%s\" clone --branch \"%s\" \"%s\" \"%s\""
      (escape_string git_command)
      (escape_string branch)
      (escape_string url)
      (escape_string (get_abs_path_string repo_dir))
  in
  let exit_status = Sys.command command in
  { exit_status; command }
