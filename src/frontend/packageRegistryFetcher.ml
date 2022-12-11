
open MyUtil
open PackageSystemBase


type error =
  | FailedToUpdateGitRegistry of {
      exit_status : int;
      command     : string;
    }


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let main ~(git_command : string) (absdir_registry_repo : abs_path) (registry_remote : registry_remote) : (unit, error) result =
  let open ResultMonad in
  let absdirstr_registry_repo = get_abs_path_string absdir_registry_repo in
  let abspathstr_registry_config =
    Filename.concat absdirstr_registry_repo Constant.package_registry_config_file_name
  in
  let (exit_status, command) =
    if Sys.file_exists abspathstr_registry_config then
      match registry_remote with
      | GitRegistry{ url; branch} ->
          let pull_command =
            Printf.sprintf "\"%s\" -C \"%s\" pull \"%s\" \"%s\""
              (escape_string git_command)
              (escape_string absdirstr_registry_repo)
              (escape_string url)
              (escape_string branch)
          in
          let exit_status = Sys.command pull_command in
          (exit_status, pull_command)
    else
      match registry_remote with
      | GitRegistry{ url; branch } ->
          let clone_command =
            Printf.sprintf "\"%s\" clone --branch \"%s\" \"%s\" \"%s\""
              (escape_string git_command)
              (escape_string branch)
              (escape_string url)
              (escape_string absdirstr_registry_repo)
          in
          let exit_status = Sys.command clone_command in
          (exit_status, clone_command)
  in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToUpdateGitRegistry{ exit_status; command }
