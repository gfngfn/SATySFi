
open MyUtil
open PackageSystemBase
open ConfigError


type collection = (package_dependency list) LocalFixedPackageIdMap.t


let get_dependencies ~(language_version : SemanticVersion.t) (absdir_package : abs_path) : (package_dependency list, config_error) result =
  let open ResultMonad in
  let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
  let*
    PackageConfig.{
      language_requirement;
      package_contents;
      registry_remotes = _; (* TODO: append to the target's `registry_remotes` *)
      _
    } = PackageConfig.load abspath_package_config
  in
  let* () =
    if language_version |> SemanticVersion.fulfill language_requirement then
      return ()
    else
      err @@ LocalFixedDoesNotSupportLanguageVersion{
        dir = absdir_package;
        language_version;
        language_requirement;
      }
  in
  match package_contents with
  | PackageConfig.Library{ dependencies; _ } ->
    (* Ignores `test_dependencies` here, because we do not run the tests of depended packages. *)
      return dependencies

  | PackageConfig.Font(_) ->
      return []

  | PackageConfig.Document(_) ->
      err @@ NotALibraryLocalFixed{ dir = absdir_package }


let rec aux ~(language_version : SemanticVersion.t) (gained : collection) (deps : package_dependency list) : (collection, config_error) result =
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
          let* deps_sub = get_dependencies ~language_version absolute_path in
          let gained = gained |> LocalFixedPackageIdMap.add absolute_path deps_sub in
          aux ~language_version gained deps_sub
  ) gained


let main ~(language_version : SemanticVersion.t) (deps : package_dependency list) : (collection, config_error) result =
  aux ~language_version  LocalFixedPackageIdMap.empty deps
    (* TODO: construct local_fixed_dependencies by traversing local fixed dependencies *)
