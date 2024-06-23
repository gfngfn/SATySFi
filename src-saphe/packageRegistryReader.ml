
open MyUtil
open ConfigError
open PackageSystemBase


let listup_package_directories (absdir_package_store : abs_path) : ((package_name * abs_path) list, config_error) result =
  let open ResultMonad in
  let filenames = Sys.readdir (get_abs_path_string absdir_package_store) |> Array.to_list in
  let* acc =
    filenames |> foldM (fun acc filename ->
      let abspath = append_to_abs_directory absdir_package_store filename in
      if (try Sys.is_directory (get_abs_path_string abspath) with _ -> false) then
        let absdir = abspath in
        let package_name = filename in (* TODO: check that `filename` is a lowercased identifier *)
        return @@ Alist.extend acc (package_name, absdir)
      else
      (* If `filename` is not a directory, ignores it: *)
        return acc
    ) Alist.empty
  in
  return @@ Alist.to_list acc


let listup_release_configs (package_name : package_name) (absdir_single_package : abs_path) : (SemanticVersion.t * abs_path) list =
  let filenames = Sys.readdir (get_abs_path_string absdir_single_package) |> Array.to_list in
  let acc =
    filenames |> List.fold_left (fun acc filename ->
      match Core.String.chop_suffix filename ~suffix:Constant.release_config_extension with
      | None ->
        (* If `filename` does not end with the release config extension, ignores it: *)
          acc

      | Some(s) ->
          begin
            match Core.String.chop_prefix s ~prefix:(Printf.sprintf "%s." package_name) with
            | None ->
                (* If `filename` does not start with the expected package name, ignores it: *)
                acc

            | Some(s) ->
                begin
                  match SemanticVersion.parse s with
                  | None ->
                      (* If the middle part of `filename` is not a semver, ignores it: *)
                      acc

                  | Some(semver) ->
                      let abspath = append_to_abs_directory absdir_single_package filename in
                      Alist.extend acc (semver, abspath)
                end
          end
    ) Alist.empty
  in
  Alist.to_list acc


let main (absdir_registry_repo : abs_path) : ((package_name * (registry_remote list * implementation_record) list) list, config_error) result =
  let open ResultMonad in

  (* Lists up package directories: *)
  let* package_dirs =
    let absdir_package_store = Constant.registry_repo_package_store ~registry_repo_root:absdir_registry_repo in
    listup_package_directories absdir_package_store
  in

  let* acc =
    package_dirs |> foldM (fun acc (package_name, absdir_single_package) ->
      (* Lists up all paths to the release configs: *)
      let config_paths = listup_release_configs package_name absdir_single_package in

      (* Reads all the release configs: *)
      let* implacc =
        config_paths |> foldM (fun implacc (package_version, abspath_release_config) ->
          let*
            PackageReleaseConfig.{
              ecosystem_requirement;
              registry_remotes;
              package_name = package_name_for_checking;
              implementation;
            } = PackageReleaseConfig.load abspath_release_config
          in
          let ImplRecord{ package_version = package_version_for_checking; _ } = implementation in
          if not (SemanticVersion.fulfill ecosystem_requirement Constant.current_ecosystem_version) then
          (* If the package release config is not for the current version of Saphe, ignores it: *)
            return implacc
          else if not (String.equal package_name_for_checking package_name) then
          (* TODO: warn and ignore this instead of emitting errors *)
            err @@ PackageNameMismatchOfRelease{
              path          = abspath_release_config;
              from_filename = package_name;
              from_content  = package_name_for_checking;
            }
          else if not (SemanticVersion.equal package_version_for_checking package_version) then
          (* TODO: warn and ignore this instead of emitting errors *)
            err @@ PackageVersionMismatchOfRelease{
              path          = abspath_release_config;
              from_filename = package_version;
              from_content  = package_version_for_checking;
            }
          else
            return @@ Alist.extend implacc (registry_remotes, implementation)
        ) Alist.empty
      in
      return @@ Alist.extend acc (package_name, Alist.to_list implacc)

    ) Alist.empty
  in

  return @@ Alist.to_list acc
