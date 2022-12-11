
open MyUtil

type run_result = {
  exit_status : int;
  command     : string;
}

val mkdir_p : abs_path -> unit

val run_wget :
  wget_command:string ->
  url:string ->
  output:abs_path ->
  run_result

val run_tar_xzf_strip_components_1 :
  tar_command:string ->
  tarball:abs_path ->
  output_dir:abs_path ->
  run_result

val run_git_pull :
  git_command:string ->
  repo_dir:abs_path ->
  url:string ->
  branch:string ->
  run_result

val run_git_clone :
  git_command:string ->
  repo_dir:abs_path ->
  url:string ->
  branch:string ->
  run_result
