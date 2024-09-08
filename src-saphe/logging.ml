
open MyUtil
open LoggingUtil
open PackageSystemBase


let show_path (spec : logging_spec) =
  display_path spec.path_display_setting


let show_package_dependency_before_solving (spec : logging_spec) (dependencies_with_flags : (dependency_flag * package_dependency) list) =
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


let show_package_dependency_solutions (spec : logging_spec) (solutions : package_solution list) =
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


let end_lock_config_output (spec : logging_spec) (abspath_lock_config : abs_path) =
  Printf.printf "  lock config written on '%s'.\n"
    (show_path spec abspath_lock_config)


let end_envelope_config_output (spec : logging_spec) (abspath_envelope_config : abs_path) =
  Printf.printf "  envelope config written on '%s'.\n"
    (show_path spec abspath_envelope_config)


let end_deps_config_output (spec : logging_spec) (abspath_deps_config : abs_path) =
  if is_verbose spec.verbosity then begin
    Printf.printf "  deps config written on '%s'.\n"
      (show_path spec abspath_deps_config)
  end


let lock_already_installed (lock_name : lock_name) (absdir : abs_path) =
  Printf.printf "  '%s': already installed at '%s'\n"
    lock_name
    (AbsPath.to_string absdir)


let lock_cache_exists (lock_name : lock_name) (abspath_tarball : abs_path) =
  Printf.printf "  cache for '%s' exists at '%s'\n"
    lock_name
    (AbsPath.to_string abspath_tarball)


let store_root_config_updated ~(created : bool) (abspath_store_root_config : abs_path) =
  let verb = if created then "created" else "updated" in
  Printf.printf "  %s the store root config '%s'\n"
    verb
    (AbsPath.to_string abspath_store_root_config)


let package_registry_updated ~(created : bool) (absdir_registry_repo : abs_path) =
  let verb = if created then "fetched" else "updated" in
  Printf.printf "  %s the package registry '%s'\n"
    verb
    (AbsPath.to_string absdir_registry_repo)


let initialize_file (spec : logging_spec) (abspath_doc : abs_path) =
  Printf.printf "  created '%s'\n" (show_path spec abspath_doc)


let initialize_package_config (spec : logging_spec) (abspath_package_config : abs_path) =
  Printf.printf "  created a package config '%s'\n" (show_path spec abspath_package_config)


let downloading_lock (lock_name : lock_name) (absdir : abs_path) =
  Printf.printf "  downloading '%s' to '%s'...\n" lock_name (AbsPath.to_string absdir)


let report_canonicalized_url ~(url : string) ~(canonicalized_url : string) ~(hash_value : registry_hash_value) =
  Printf.printf "  registry URL: '%s'\n" url;
  Printf.printf "    canonicalized: '%s'\n" canonicalized_url;
  Printf.printf "    hash value: '%s'\n" hash_value
