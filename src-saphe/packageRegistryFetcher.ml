
open MyUtil
open PackageSystemBase


type error =
  | FailedToFetchGitRegistry of {
      exit_status : int;
      command     : string;
    }
  | FailedToUpdateGitRegistry of {
      exit_status : int;
      command     : string;
    }


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let main ~(do_update : bool) ~(git_command : string) (absdir_registry_repo : abs_path) (registry_remote : registry_remote) : (bool, error) result =
  let open ResultMonad in
  let abspath_registry_config = Constant.package_registry_config_path ~registry_dir:absdir_registry_repo in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      if file_exists abspath_registry_config then
        if do_update then
          let ShellCommand.{ exit_status; command } =
            ShellCommand.run_git_pull ~git_command ~repo_dir:absdir_registry_repo ~url ~branch
          in
          if exit_status = 0 then
            return false
          else
            err @@ FailedToUpdateGitRegistry{ exit_status; command }
        else
          return false
      else
        let ShellCommand.{ exit_status; command } =
          ShellCommand.run_git_clone ~git_command ~repo_dir:absdir_registry_repo ~url ~branch
        in
        if exit_status = 0 then
          return true
        else
          err @@ FailedToFetchGitRegistry{ exit_status; command }
