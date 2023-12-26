
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
  let abspath_registry_config =
    let absdirstr_registry_repo = get_abs_path_string absdir_registry_repo in
    make_abs_path (Filename.concat absdirstr_registry_repo Constant.package_registry_config_file_name)
  in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      let ShellCommand.{ exit_status; command } =
        if Sys.file_exists (get_abs_path_string abspath_registry_config) then
          ShellCommand.run_git_pull ~git_command ~repo_dir:absdir_registry_repo ~url ~branch
        else
          ShellCommand.run_git_clone ~git_command ~repo_dir:absdir_registry_repo ~url ~branch
      in
      if exit_status = 0 then
        return ()
      else
        err @@ FailedToUpdateGitRegistry{ exit_status; command }
