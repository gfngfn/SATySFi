
open MyUtil
open PackageSystemBase
open ConfigError


type collection = (package_dependency list) LocalFixedPackageIdMap.t


let get_dependencies (absdir_package : abs_path) : (package_dependency list, config_error) result =
  let open ResultMonad in
  let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
  let*
    PackageConfig.{
      language_requirement = _; (* TODO: check that this requirement meets the selected language version *)
      package_contents;
      registry_remotes = _; (* TODO: append to the target's `registry_remotes` *)
      _
    } = PackageConfig.load abspath_package_config
  in
  match package_contents with
  | PackageConfig.Library{ dependencies; _ } ->
    (* Ignores `test_dependencies` here. *)
      return dependencies

  | PackageConfig.Font(_) ->
      return []

  | PackageConfig.Document(_) ->
      err @@ NotALibraryLocalFixed{ dir = absdir_package }


let rec aux (gained : collection) (deps : package_dependency list) : (collection, config_error) result =
  let open ResultMonad in
  deps |> foldM (fun gained dep ->
    let PackageDependency{ spec; _ } = dep in
    match spec with
    | RegisteredDependency(_) ->
        return gained

    | LocalFixedDependency{ absolute_path } ->
        if gained |> LocalFixedPackageIdMap.mem absolute_path then
          return gained
        else
          let* deps_sub = get_dependencies absolute_path in
          let gained = gained |> LocalFixedPackageIdMap.add absolute_path deps_sub in
          aux gained deps_sub
  ) gained


let main (deps : package_dependency list) : (collection, config_error) result =
  aux LocalFixedPackageIdMap.empty deps
    (* TODO: construct local_fixed_dependencies by traversing local fixed dependencies *)
