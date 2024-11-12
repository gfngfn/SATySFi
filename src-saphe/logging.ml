
open MyUtil
open LoggingUtil
open PackageSystemBase


let show_package_dependency_before_solving (spec : logging_spec) (dependencies_with_flags : (dependency_flag * package_dependency) list) =
  if is_not_quiet spec then begin
    Printf.printf "  package dependencies to solve:\n";
    dependencies_with_flags |> List.iter (fun (flag, dep) ->
      let PackageDependency{ used_as; spec = dep_spec } = dep in
      match dep_spec with
      | RegisteredDependency{ registered_package_id; version_requirement; _ } ->
          let RegisteredPackageId.{ package_name; _ } = registered_package_id in
          let s_restr = SemanticVersion.requirement_to_string version_requirement in
          let s_test_only =
            match flag with
            | SourceDependency   -> ""
            | TestOnlyDependency -> ", test_only"
          in
          Printf.printf "  - %s (%s%s) used as %s\n"
            package_name
            s_restr
            s_test_only
            used_as

      | LocalFixedDependency{ absolute_path } ->
          Printf.printf "  - '%s' used as %s\n"
            (show_path spec absolute_path)
            used_as
    )
  end


let show_package_dependency_solutions (spec : logging_spec) (solutions : package_solution list) =
  if is_not_quiet spec then begin
    Printf.printf "  package dependency solutions:\n";
    solutions |> List.iter (fun solution ->
      match solution.lock with
      | Lock.Registered(RegisteredLock.{ registered_package_id; locked_version; _ }) ->
          let RegisteredPackageId.{ package_name; _ } = registered_package_id in
          Printf.printf "  - %s %s\n"
            package_name
            (SemanticVersion.to_string locked_version)

      | Lock.LocalFixed{ absolute_path } ->
          Printf.printf "  - %s\n"
            (show_path spec absolute_path)
    )
  end


let end_lock_config_output (spec : logging_spec) (abspath_lock_config : abs_path) =
  if is_not_quiet spec then begin
    Printf.printf "  lock config written on '%s'.\n"
      (show_path spec abspath_lock_config)
  end


let end_envelope_config_output (spec : logging_spec) (abspath_envelope_config : abs_path) =
  if is_not_quiet spec then begin
    Printf.printf "  envelope config written on '%s'.\n"
      (show_path spec abspath_envelope_config)
  end


let end_deps_config_output (spec : logging_spec) (abspath_deps_config : abs_path) =
  if is_verbose spec then begin
    Printf.printf "  deps config written on '%s'.\n"
      (show_path spec abspath_deps_config)
  end


let lock_already_installed (spec : logging_spec) (lock_name : lock_name) (absdir : abs_path) =
  if is_verbose spec then begin
    Printf.printf "  '%s': already installed at '%s'\n"
      lock_name
      (AbsPath.to_string absdir)
  end else if is_not_quiet spec then begin
    Printf.printf "  '%s': already installed\n"
      lock_name
  end


let lock_cache_exists (spec : logging_spec) (lock_name : lock_name) (abspath_tarball : abs_path) =
  if is_not_quiet spec then begin
    Printf.printf "  cache for '%s' exists at '%s'\n"
      lock_name
      (AbsPath.to_string abspath_tarball)
  end


let store_root_config_updated (spec : logging_spec) ~(created : bool) (abspath_store_root_config : abs_path) =
  let verb = if created then "created" else "updated" in
  if is_verbose spec then begin
    Printf.printf "  %s the store root config '%s'\n"
      verb
      (AbsPath.to_string abspath_store_root_config)
  end else if is_not_quiet spec then begin
    Printf.printf "  %s the store root config\n"
      verb
  end

let package_registry_updated (spec : logging_spec) ~(created : bool) (absdir_registry_repo : abs_path) =
  if is_not_quiet spec then begin
    let verb = if created then "fetched" else "updated" in
    Printf.printf "  %s the package registry '%s'\n"
      verb
      (AbsPath.to_string absdir_registry_repo)
  end


let initialize_file (spec : logging_spec) (abspath_doc : abs_path) =
  Printf.printf "  created '%s'\n" (show_path spec abspath_doc)


let initialize_package_config (spec : logging_spec) (abspath_package_config : abs_path) =
  Printf.printf "  created a package config '%s'\n" (show_path spec abspath_package_config)


let downloading_lock (spec : logging_spec) (lock_name : lock_name) (absdir : abs_path) =
  if is_not_quiet spec then begin
    Printf.printf "  downloading '%s' to '%s'...\n" lock_name (AbsPath.to_string absdir)
  end
