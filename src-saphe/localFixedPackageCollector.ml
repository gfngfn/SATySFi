
open MyUtil
open EnvelopeSystemBase
open PackageSystemBase
open ConfigError


type collection = (package_dependency list * envelope_contents) LocalFixedPackageIdMap.t


let get_dependencies ~(language_version : SemanticVersion.t) (absdir_package : abs_path) : (package_dependency list * envelope_contents * registry_remote list, config_error) result =
  let open ResultMonad in
  let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
  let*
    PackageConfig.{
      language_requirement;
      package_contents;
      registry_remotes;
      source_dependencies;
      _
    } = PackageConfig.load abspath_package_config
      (* Ignores `test_dependencies` here, because we do not run the tests of depended packages. *)
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
  let* envelope_contents =
    match package_contents with
    | Library{
        main_module_name;
        source_directories;
        test_directories;
        markdown_conversion;
        _
      } ->
        return @@ EnvelopeSystemBase.Library{
          main_module_name;
          source_directories;
          test_directories;
          markdown_conversion;
        }

    | Font{
        main_module_name;
        font_file_descriptions;
      } ->
        return @@ EnvelopeSystemBase.Font{
          main_module_name;
          font_file_descriptions;
        }

    | Document ->
        err @@ NotALibraryLocalFixed{ dir = absdir_package }
  in
  return (source_dependencies, envelope_contents, registry_remotes)


let rec aux ~(language_version : SemanticVersion.t) (gained : collection) (deps : package_dependency list) (registry_remote_acc : registry_remote Alist.t) : (collection * registry_remote Alist.t, config_error) result =
  let open ResultMonad in
  deps |> foldM (fun (gained, registry_remote_acc) dep ->
    let PackageDependency{ spec; _ } = dep in
    match spec with
    | RegisteredDependency(_) ->
        return (gained, registry_remote_acc)

    | LocalFixedDependency{ absolute_path } ->
        if gained |> LocalFixedPackageIdMap.mem absolute_path then
          return (gained, registry_remote_acc)
        else
          let* (deps_sub, envelope_contents, registry_remotes_sub) =
            get_dependencies ~language_version absolute_path
          in
          let gained = gained |> LocalFixedPackageIdMap.add absolute_path (deps_sub, envelope_contents) in
          let registry_remote_acc = Alist.append registry_remote_acc registry_remotes_sub in
          aux ~language_version gained deps_sub registry_remote_acc
  ) (gained, registry_remote_acc)


let main ~(language_version : SemanticVersion.t) (deps : package_dependency list) : (collection * registry_remote list, config_error) result =
  let open ResultMonad in
  let* (gained, registry_remote_acc) =
    aux ~language_version LocalFixedPackageIdMap.empty deps Alist.empty
  in
  return (gained, Alist.to_list registry_remote_acc)
