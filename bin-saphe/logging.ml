
open MyUtil
open PackageSystemBase


let show_package_dependency_before_solving (dependencies_with_flags : (dependency_flag * package_dependency) list) =
  Printf.printf "  package dependencies to solve:\n";
  dependencies_with_flags |> List.iter (fun (flag, dep) ->
    let PackageDependency{ used_as; spec } = dep in
    match spec with
    | RegisteredDependency{ package_id; version_requirement; _ } ->
        let PackageId.{ package_name; _ } = package_id in
        let s_restr = SemanticVersion.requirement_to_string version_requirement in
        let s_test_only =
          match flag with
          | SourceDependency   -> ""
          | TestOnlyDependency -> ", test_only"
        in
        Printf.printf "  - %s (%s%s) used as %s\n" package_name s_restr s_test_only used_as;
  )


let show_package_dependency_solutions (solutions : package_solution list) =
  Printf.printf "  package dependency solutions:\n";
    solutions |> List.iter (fun solution ->
      let Lock.{ package_id; locked_version; _ } = solution.lock in
      let PackageId.{ package_name; _ } = package_id in
      Printf.printf "  - %s %s\n" package_name (SemanticVersion.to_string locked_version)
  )


let end_lock_config_output (abspath_lock_config : abs_path) =
  Printf.printf "  lock config written on '%s'.\n" (get_abs_path_string abspath_lock_config)


let end_envelope_config_output (abspath_envelope_config : abs_path) =
  Printf.printf "  envelope config written on '%s'.\n" (get_abs_path_string abspath_envelope_config)


let end_deps_config_output (abspath_deps_config : abs_path) =
  Printf.printf "  deps config written on '%s'.\n" (get_abs_path_string abspath_deps_config)


let lock_already_installed (lock_name : lock_name) (absdir : abs_path) =
  Printf.printf "  '%s': already installed at '%s'\n" lock_name (get_abs_path_string absdir)


let lock_cache_exists (lock_name : lock_name) (abspath_tarball : abs_path) =
  Printf.printf "  cache for '%s' exists at '%s'\n" lock_name (get_abs_path_string abspath_tarball)


let downloading_lock (lock_name : lock_name) (absdir : abs_path) =
  Printf.printf "  downloading '%s' to '%s'...\n" lock_name (get_abs_path_string absdir)


let report_canonicalized_url ~(url : string) ~(canonicalized_url : string) ~(hash_value : registry_hash_value) =
  Printf.printf "  registry URL: '%s'\n" url;
  Printf.printf "    canonicalized: '%s'\n" canonicalized_url;
  Printf.printf "    hash value: '%s'\n" hash_value
