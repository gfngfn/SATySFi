
open MyUtil
open LoggingUtil
open EnvelopeSystemBase
open PackageSystemBase
open ConfigError


let version =
  Printf.sprintf "Saphe version %s"
    (SemanticVersion.to_string Constant.current_ecosystem_version)


let make_logging_spec ~(show_full_path : bool) ~(verbosity : verbosity) ~current_dir:(absdir_current : abs_path) =
  let path_display_setting =
    if show_full_path then
      FullPath
    else
      RelativeToCwd(absdir_current)
  in
  Logging.{ path_display_setting; verbosity }


type solve_input =
  | PackageSolveInput of {
      root     : abs_path; (* The absolute path of a directory used as the package root *)
      config   : abs_path; (* The absolute path to the package config file *)
      lock     : abs_path; (* A path for writing a resulting lock config file *)
      envelope : abs_path; (* A path for writing a resulting envelope config file *)
    }
  | DocumentSolveInput of {
      doc    : abs_path; (* The absolute path to the document file *)
      config : abs_path; (* The absolute path to the package config file *)
      lock   : abs_path; (* A path for writing a resulting lock file *)
    }


let update_store_root_config_if_needed (registries : registry_remote RegistryHashValueMap.t) (registry_hash_value : registry_hash_value) (registry_remote : registry_remote) (abspath_store_root : abs_path) : (unit, config_error) result =
  let open ResultMonad in
  match
    registries |> RegistryHashValueMap.find_opt registry_hash_value
  with
  | None ->
      let store_root_config =
        StoreRootConfig.{
          registries = registries |> RegistryHashValueMap.add registry_hash_value registry_remote;
        }
      in
      StoreRootConfig.write abspath_store_root store_root_config

  | Some(_registry_remote) ->
      return ()


let make_lock_name ~seen_from:(absdir_seen_from : abs_path) (lock : Lock.t) : lock_name =
  match lock with
  | Lock.Registered({ registered_package_id; locked_version }) ->
      let RegisteredPackageId.{ registry_hash_value; package_name } = registered_package_id in
      Printf.sprintf "registered.%s.%s.%s"
        registry_hash_value
        package_name
        (SemanticVersion.to_string locked_version)

  | Lock.LocalFixed{ absolute_path } ->
      Printf.sprintf "local.%s"
        (AbsPath.to_relative_string ~from:absdir_seen_from absolute_path)


let make_lock_dependency ~(seen_from : abs_path) (dep : locked_dependency) : LockConfig.lock_dependency =
  {
    depended_lock_name = make_lock_name ~seen_from dep.depended_lock;
    used_as            = dep.dependency_used_as;
  }


let convert_solutions_to_lock_config ~(seen_from : abs_path) (solutions : package_solution list) : LockConfig.t * implementation_spec list =
  let (locked_package_acc, impl_spec_acc) =
    solutions |> List.fold_left (fun (locked_package_acc, impl_spec_acc) solution ->
      let { lock; locked_source; _ } = solution in
      let locked_package =
        LockConfig.{
          lock_name         = make_lock_name ~seen_from lock;
          lock_contents     = lock;
          lock_dependencies = solution.locked_dependencies |> List.map (make_lock_dependency ~seen_from);
          test_only_lock    = solution.used_in_test_only;
        }
      in
      let impl_spec = ImplSpec{ lock; source = locked_source } in
      (Alist.extend locked_package_acc locked_package, Alist.extend impl_spec_acc impl_spec)
    ) (Alist.empty, Alist.empty)
  in
  let explicit_dependencies =
    solutions |> List.filter_map (fun solution ->
      solution.explicitly_depended |> Option.map (fun used_as ->
        LockConfig.{
          depended_lock_name = make_lock_name ~seen_from solution.lock;
          used_as;
        }
      )
    )
  in
  let explicit_test_dependencies =
    solutions |> List.filter_map (fun solution ->
      solution.explicitly_test_depended |> Option.map (fun used_as ->
        LockConfig.{
          depended_lock_name = make_lock_name ~seen_from solution.lock;
          used_as;
        }
      )
    )
  in
  let lock_config =
    LockConfig.{
      locked_packages = Alist.to_list locked_package_acc;
      explicit_dependencies;
      explicit_test_dependencies;
    }
  in
  (lock_config, Alist.to_list impl_spec_acc)


let make_envelope_config (abspath_package_config : abs_path) (package_contents : package_contents) : (EnvelopeConfig.t, config_error) result =
  let open ResultMonad in
  match package_contents with
  | Library{
      main_module_name;
      source_directories;
      test_directories;
      markdown_conversion;
      _
    } ->
      return {
        envelope_contents =
          Library{
            main_module_name;
            source_directories;
            test_directories;
            markdown_conversion;
          };
      }

  | Font{
      main_module_name;
      font_file_descriptions;
    } ->
      return { envelope_contents = Font{ main_module_name; font_file_descriptions } }

  | Document ->
      err @@ NotAPackageButADocument(abspath_package_config)


let get_minimum_language_version (language_requirement : SemanticVersion.requirement) : SemanticVersion.t =
  match language_requirement with
  | SemanticVersion.CompatibleWith(semver) -> semver


let get_store_root () : (abs_path, config_error) result =
  let open ResultMonad in
  let (opt, env_var_home) = AbsPathIo.get_home_directory () in
  match opt with
  | None             -> err @@ CannotDetermineStoreRoot { envvar = env_var_home }
  | Some(absdir_home) -> return @@ AbsPath.append_to_directory absdir_home ".saphe"


type package_init_input =
  | PackageInitInput of {
      doc    : abs_path;
      config : abs_path;
    }


let write_package_config (logging_spec : logging_spec) (abspath_package_config : abs_path) ~(data : string) =
  let open ResultMonad in
  let* () =
    AbsPathIo.write_file abspath_package_config data
      |> Result.map_error (fun message -> FailedToWriteFile{ path = abspath_package_config; message })
  in
  Logging.initialize_package_config logging_spec abspath_package_config;
  return ()


let write_initial_file (logging_spec : logging_spec) (abspath : abs_path) ~(data : string) =
  let open ResultMonad in
  let* () =
    AbsPathIo.write_file abspath data
      |> Result.map_error (fun message -> FailedToWriteFile{ path = abspath; message })
  in
  Logging.initialize_file logging_spec abspath;
  return ()


let assert_nonexistence (abspath : abs_path) =
  let open ResultMonad in
  if AbsPathIo.file_exists abspath then
    err @@ FileAlreadyExists{ path = abspath }
  else
    return ()


let init_document
  ~(fpath_in : string)
  ~(show_full_path : bool)
  ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Constructs the input: *)
    let abspath_doc = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_package_config = Constant.document_package_config_path ~doc:abspath_doc in
    let absdir = AbsPath.dirname abspath_doc in

    let* () = assert_nonexistence abspath_doc in
    let* () = assert_nonexistence abspath_package_config in

    match Filename.extension (AbsPath.to_string abspath_doc) with
    | ".saty" ->
        ShellCommand.mkdir_p absdir;
        let* () = write_package_config logging_spec abspath_package_config ~data:InitData.document_package_config_contents in
        let* () = write_initial_file logging_spec abspath_doc ~data:InitData.document_contents in
        return ()

    | ".md" ->
        ShellCommand.mkdir_p absdir;
        let* () = write_package_config logging_spec abspath_package_config ~data:InitData.markdown_package_config_contents in
        let* () = write_initial_file logging_spec abspath_doc ~data:InitData.markdown_contents in
        return ()

    | extension ->
        err @@ InvalidExtensionForDocument{ path = abspath_doc; extension }
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> ErrorReporting.report_config_error e; exit 1


let init_library
  ~(fpath_in : string)
  ~(show_full_path : bool)
  ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Constructs the input: *)
    let absdir_package = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
    let abspath_source = AbsPath.append_to_directory absdir_package "src/calc.satyh" in
    let abspath_test = AbsPath.append_to_directory absdir_package "test/calc-test.satyh" in

    let* () = assert_nonexistence abspath_package_config in
    let* () = assert_nonexistence abspath_source in
    let* () = assert_nonexistence abspath_test in

    ShellCommand.mkdir_p absdir_package;
    ShellCommand.mkdir_p (AbsPath.append_to_directory absdir_package "src");
    ShellCommand.mkdir_p (AbsPath.append_to_directory absdir_package "test");
    let* () = write_package_config logging_spec abspath_package_config ~data:InitData.library_package_config_contents in
    let* () = write_initial_file logging_spec abspath_source ~data:InitData.library_source_contents in
    let* () = write_initial_file logging_spec abspath_test ~data:InitData.library_test_contents in

    return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> ErrorReporting.report_config_error e; exit 1


let make_solve_input ~current_dir:(absdir_current : abs_path) ~(fpath_in : string) : solve_input =
  let abspath_in = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
  if AbsPathIo.is_directory abspath_in then
  (* If the input is a directory that forms a package: *)
    let abspath_package_config = Constant.library_package_config_path ~dir:abspath_in in
    let abspath_lock_config = Constant.library_lock_config_path ~dir:abspath_in in
    let abspath_envelope_config = Constant.envelope_config_path ~dir:abspath_in in
    PackageSolveInput{
      root     = abspath_in;
      config   = abspath_package_config;
      lock     = abspath_lock_config;
      envelope = abspath_envelope_config;
    }
  else
    let abspath_package_config = Constant.document_package_config_path ~doc:abspath_in in
    let abspath_lock_config = Constant.document_lock_config_path ~doc:abspath_in in
    DocumentSolveInput{
      doc    = abspath_in;
      config = abspath_package_config;
      lock   = abspath_lock_config;
    }


let solve
    ~(fpath_in : string)
    ~(show_full_path : bool)
    ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Constructs the input: *)
    let solve_input =
      make_solve_input ~current_dir:absdir_current ~fpath_in
    in

    let* (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes) =
      match solve_input with
      | PackageSolveInput{
          root     = _absdir_package;
          config   = abspath_package_config;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
        } ->
          (* Loads the package config: *)
          let*
            PackageConfig.{
              language_requirement;
              package_contents;
              registry_remotes;
              source_dependencies;
              test_dependencies;
              _
            } = PackageConfig.load abspath_package_config
          in

          (* Selects the minimum version according to the user's designation: *)
          (* TODO: consider taking dependencies into account when selecting a language version *)
          let language_version = get_minimum_language_version language_requirement in

          (* Writes the envelope config: *)
          let* envelope_config = make_envelope_config abspath_package_config package_contents in
          let* () =
            EnvelopeConfig.write abspath_envelope_config envelope_config
              |> Result.map_error (fun message ->
                CannotWriteEnvelopeConfig{ message; path = abspath_envelope_config }
              )
          in
          Logging.end_envelope_config_output logging_spec abspath_envelope_config;

          let dependencies_with_flags =
            List.append
              (source_dependencies |> List.map (fun dep -> (SourceDependency, dep)))
              (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))
          in
          return (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes)

      | DocumentSolveInput{
          doc    = _abspath_doc;
          config = abspath_package_config;
          lock   = abspath_lock_config;
        } ->
          (* Loads the package config: *)
          let*
            PackageConfig.{
              language_requirement;
              registry_remotes;
              source_dependencies;
              test_dependencies;
              _
            } = PackageConfig.load abspath_package_config
          in

          (* Selects the minimum version according to the user's designation: *)
          (* TODO: consider taking dependencies into account when selecting a language version *)
          let language_version = get_minimum_language_version language_requirement in

          let dependencies_with_flags =
            List.append
              (source_dependencies |> List.map (fun dep -> (SourceDependency, dep)))
              (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))
          in
          return (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes)
    in

    Logging.show_package_dependency_before_solving logging_spec dependencies_with_flags;

    (* Collects the local fixed packages used by the target: *)
    let* (local_fixed_package_map, registry_remotes_sub) =
      LocalFixedPackageCollector.main ~language_version (List.map Stdlib.snd dependencies_with_flags)
    in
    let registry_remotes = List.append registry_remotes registry_remotes_sub in

    (* Creates the envelope config for each local fixed packages,
       while extracting `local_fixed_dependencies` from `local_fixed_package_map`: *)
    let* local_fixed_dependencies =
      LocalFixedPackageIdMap.fold (fun absdir_package (deps, envelope_contents) res ->
        let* local_fixed_dependencies = res in
        let abspath_envelope_config = Constant.envelope_config_path ~dir:absdir_package in
        let* () =
          EnvelopeConfig.write abspath_envelope_config { envelope_contents }
            |> Result.map_error (fun message -> FailedToWriteFile{ path = abspath_envelope_config; message })
        in
        Logging.end_envelope_config_output logging_spec abspath_envelope_config;
        return (local_fixed_dependencies |> LocalFixedPackageIdMap.add absdir_package deps)
      ) local_fixed_package_map (return LocalFixedPackageIdMap.empty)
    in

    (* Arranges the store root config: *)
    let* absdir_store_root = get_store_root () in
    let abspath_store_root_config = Constant.store_root_config_path ~store_root:absdir_store_root in
    ShellCommand.mkdir_p absdir_store_root;
    let* (store_root_config, created) = StoreRootConfig.load_or_initialize abspath_store_root_config in
    begin
      if created then Logging.store_root_config_updated logging_spec ~created:true abspath_store_root_config
    end;

    (* Constructs a map that associates a package with its implementations: *)
    let* registered_package_impls =
      PackageRegistryArranger.main
        ~err:(fun e -> CanonicalRegistryUrlError(e))
        (fun registered_package_impls canonical_registry_remote ->
          let* registry_hash_value = ConfigUtil.make_registry_hash_value canonical_registry_remote in

          (* Manupulates the store root config: *)
          let* () =
            update_store_root_config_if_needed
              store_root_config.StoreRootConfig.registries
              registry_hash_value
              canonical_registry_remote
              abspath_store_root_config
          in

          (* Loads the registry config: *)
          let absdir_registry_repo =
            Constant.registry_root_directory_path ~store_root:absdir_store_root registry_hash_value
          in
          let git_command = "git" in (* TODO: make this changeable *)
          let* created =
            PackageRegistryFetcher.main ~do_update:false ~git_command absdir_registry_repo canonical_registry_remote
              |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
          in
          begin
            if created then Logging.package_registry_updated logging_spec ~created:true absdir_registry_repo
          end;

          (* Loads the registry config: *)
          let* () =
            let abspath_registry_config =
              Constant.package_registry_config_path ~registry_dir:absdir_registry_repo
            in
            PackageRegistryConfig.load abspath_registry_config
          in

          (* Reads the registry and grows `registered_package_impls`,
             and through this traversal, collects `new_registry_remotes`,
             which are other registies on which the packages in the current registry depend: *)
          let* packages = PackageRegistryReader.main absdir_registry_repo in
          let* (registry_remote_acc, registered_package_impls) =
            packages |> foldM (fun (registry_remote_acc, registered_package_impls) (package_name, impls_with_remotes) ->
              let registered_package_id = RegisteredPackageId.{ registry_hash_value; package_name } in
              if registered_package_impls |> RegisteredPackageIdMap.mem registered_package_id then
                err @@ MultiplePackageDefinition{ package_name }
              else
                let registry_remotess = List.map fst impls_with_remotes in
                let impls = List.map snd impls_with_remotes in
                let registry_remote_acc = Alist.append registry_remote_acc (List.concat registry_remotess) in
                let registered_package_impls =
                  registered_package_impls |> RegisteredPackageIdMap.add registered_package_id impls
                in
                return (registry_remote_acc, registered_package_impls)
            ) (Alist.empty, registered_package_impls)
          in
          let new_registry_remotes = Alist.to_list registry_remote_acc in

          return (new_registry_remotes, registered_package_impls)
        )
        registry_remotes
        RegisteredPackageIdMap.empty
    in

    let package_context = { language_version; registered_package_impls; local_fixed_dependencies } in
    let solutions_opt = PackageConstraintSolver.solve package_context dependencies_with_flags in
    begin
      match solutions_opt with
      | None ->
          err CannotSolvePackageConstraints

      | Some(solutions) ->

          Logging.show_package_dependency_solutions logging_spec solutions;

          let (lock_config, impl_specs) =
            convert_solutions_to_lock_config
              ~seen_from:(AbsPath.dirname abspath_lock_config)
              solutions
          in

          let wget_command = "wget" in (* TODO: make this changeable *)
          let tar_command = "tar" in (* TODO: make this changeable *)
          let unzip_command = "unzip" in (* TODO: make this changeable *)
          let* () =
            impl_specs |> foldM (fun () impl_spec ->
              LockFetcher.main logging_spec
                ~wget_command ~tar_command ~unzip_command ~store_root:absdir_store_root impl_spec
            ) ()
          in
          let* () = LockConfig.write abspath_lock_config lock_config in
          Logging.end_lock_config_output logging_spec abspath_lock_config;
          return ()
    end
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> ErrorReporting.report_config_error e; exit 1


let update
  ~(fpath_in : string)
  ~(show_full_path : bool)
  ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Constructs the input: *)
    let solve_input = make_solve_input ~current_dir:absdir_current ~fpath_in in

    (* Loads the package config: *)
    let* registry_remotes =
      match solve_input with
      | PackageSolveInput{ root = absdir_package; _ } ->
          let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
          let* PackageConfig.{ registry_remotes; _ } = PackageConfig.load abspath_package_config in
          return registry_remotes

      | DocumentSolveInput{ config = abspath_package_config; _ } ->
          let* PackageConfig.{ registry_remotes; _ } = PackageConfig.load abspath_package_config in
          return registry_remotes
    in

    (* Arranges the store root config: *)
    let* absdir_store_root = get_store_root () in
    let abspath_store_root_config = Constant.store_root_config_path ~store_root:absdir_store_root in
    ShellCommand.mkdir_p absdir_store_root;
    let* (store_root_config, created) = StoreRootConfig.load_or_initialize abspath_store_root_config in
    Logging.store_root_config_updated logging_spec ~created abspath_store_root_config;

    PackageRegistryArranger.main
      ~err:(fun e -> CanonicalRegistryUrlError(e))
      (fun () canonical_registry_remote ->
        let* registry_hash_value = ConfigUtil.make_registry_hash_value canonical_registry_remote in

        (* Manupulates the store root config: *)
        let* () =
          update_store_root_config_if_needed
            store_root_config.StoreRootConfig.registries
            registry_hash_value
            canonical_registry_remote
            abspath_store_root_config
        in

        (* Loads the registry config: *)
        let absdir_registry_repo =
          Constant.registry_root_directory_path ~store_root:absdir_store_root registry_hash_value
        in
        let git_command = "git" in (* TODO: make this changeable *)
        let* created =
          PackageRegistryFetcher.main ~do_update:true ~git_command absdir_registry_repo canonical_registry_remote
            |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
        in
        Logging.package_registry_updated logging_spec ~created absdir_registry_repo;

        return ([], ())
      )
      registry_remotes
      ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> ErrorReporting.report_config_error e; exit 1


type build_input =
  | PackageBuildInput of {
      root     : abs_path;
      config   : abs_path;
      lock     : abs_path;
      envelope : abs_path;
      options  : SatysfiCommand.package_build_option;
    }
  | DocumentBuildInput of {
      root    : abs_path;
      doc     : abs_path;
      config  : abs_path;
      lock    : abs_path;
      out     : abs_path;
      options : SatysfiCommand.document_build_option;
    }


let make_envelope_dependency (lock_dep : LockConfig.lock_dependency) : envelope_dependency =
  {
    dependency_name    = lock_dep.depended_lock_name;
    dependency_used_as = lock_dep.used_as;
  }


let make_envelope_spec ~(store_root : abs_path) (locked_package : LockConfig.locked_package) : envelope_spec =
  let
    LockConfig.{
      lock_name;
      lock_dependencies;
      lock_contents;
      test_only_lock;
    } = locked_package
  in
  let envelope_path =
    match lock_contents with
    | Lock.Registered(reglock) ->
        Constant.registered_lock_envelope_config ~store_root reglock

    | Lock.LocalFixed{ absolute_path } ->
        Constant.envelope_config_path ~dir:absolute_path
  in
  let envelope_dependencies = lock_dependencies |> List.map make_envelope_dependency in
  {
    envelope_name = lock_name;
    envelope_path;
    envelope_dependencies;
    test_only_envelope = test_only_lock;
  }


let make_deps_config ~(store_root : abs_path) (lock_config : LockConfig.t) : DepsConfig.t =
  let LockConfig.{ locked_packages; explicit_dependencies; explicit_test_dependencies } = lock_config in
  let envelopes = locked_packages |> List.map (make_envelope_spec ~store_root) in
  let explicit_dependencies = explicit_dependencies |> List.map make_envelope_dependency in
  let explicit_test_dependencies = explicit_test_dependencies |> List.map make_envelope_dependency in
  { envelopes; explicit_dependencies; explicit_test_dependencies }


let build
    ~(fpath_in : string)
    ~(fpath_out_opt : string option)
    ~(text_mode_formats_str_opt : string option)
    ~(page_number_limit : int)
    ~(max_repeats : int)
    ~(show_full_path : bool)
    ~(verbosity : verbosity)
    ~(debug_show_bbox : bool)
    ~(debug_show_space : bool)
    ~(debug_show_block_bbox : bool)
    ~(debug_show_block_space : bool)
    ~(debug_show_overfull : bool)
    ~(type_check_only : bool)
    ~(bytecomp : bool)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Constructs the input: *)
    let build_input =
      let abspath_in = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
      if AbsPathIo.is_directory abspath_in then
        let options =
          SatysfiCommand.PackageBuildOption{
            show_full_path;
            verbosity;
          }
        in
        let abspath_package_config = Constant.library_package_config_path ~dir:abspath_in in
        let abspath_lock_config = Constant.library_lock_config_path ~dir:abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path ~dir:abspath_in in
        PackageBuildInput{
          root     = abspath_in;
          config   = abspath_package_config;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
          options;
        }
      else
        let options =
          SatysfiCommand.DocumentBuildOption{
            show_full_path;
            verbosity;
            page_number_limit;
            debug_show_bbox;
            debug_show_space;
            debug_show_block_bbox;
            debug_show_block_space;
            debug_show_overfull;
            type_check_only;
            bytecomp;
          }
        in
        let absdir_doc_root = AbsPath.dirname abspath_in in
        let abspath_package_config = Constant.document_package_config_path ~doc:abspath_in in
        let abspath_lock_config = Constant.document_lock_config_path ~doc:abspath_in in
        let abspath_out =
          match fpath_out_opt with
          | None ->
              Constant.default_output_path ~doc:abspath_in

          | Some(fpath_out) ->
              AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_out
        in
        DocumentBuildInput{
          root    = absdir_doc_root;
          doc     = abspath_in;
          config  = abspath_package_config;
          lock    = abspath_lock_config;
          out     = abspath_out;
          options;
        }
    in

    let* absdir_store_root = get_store_root () in

    match build_input with
    | PackageBuildInput{
        root     = absdir_package;
        config   = abspath_package_config;
        lock     = abspath_lock_config;
        envelope = abspath_envelope_config;
        options;
      } ->
        (* Loads the package config: *)
        let*
          PackageConfig.{
            intermediate_directory;
            _
          } = PackageConfig.load abspath_package_config
        in

        (* Sets the path for the deps config: *)
        let absdir_intermediate =
          let intermediate_directory =
            Option.value
              ~default:Constant.default_intermediate_directory_name
              intermediate_directory
          in
          AbsPath.append_to_directory absdir_package intermediate_directory
        in
        let abspath_deps_config = Constant.library_deps_config_path ~dir:absdir_intermediate in

        (* Updates the deps config: *)
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config ~store_root:absdir_store_root lock_config in
        ShellCommand.mkdir_p absdir_intermediate;
        let* () = DepsConfig.write abspath_deps_config deps_config in
        Logging.end_deps_config_output logging_spec abspath_deps_config;

        (* Builds the package by invoking `satysfi`: *)
        let SatysfiCommand.{ exit_status; command = _ } =
          SatysfiCommand.build_package
            ~envelope:abspath_envelope_config
            ~deps:abspath_deps_config
            ~mode:text_mode_formats_str_opt
            ~options
        in
        return exit_status

    | DocumentBuildInput{
        root   = absdir_doc_root;
        doc    = abspath_doc;
        config = abspath_package_config;
        lock   = abspath_lock_config;
        out    = abspath_out;
        options;
      } ->
        (* Loads the package config: *)
        let*
          PackageConfig.{
            intermediate_directory;
            _
          } = PackageConfig.load abspath_package_config
        in

        (* Sets the paths for the deps config and the dump file: *)
        let absdir_intermediate =
          let intermediate_directory =
            Option.value ~default:Constant.default_intermediate_directory_name
              intermediate_directory
          in
          AbsPath.append_to_directory absdir_doc_root intermediate_directory
        in
        let doc_basename = AbsPath.basename abspath_doc in
        let abspath_deps_config =
          Constant.document_deps_config_path ~dir:absdir_intermediate ~doc_basename
        in
        let abspath_dump =
          Constant.dump_path ~dir:absdir_intermediate ~doc_basename
        in

        (* Updates the deps config: *)
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config ~store_root:absdir_store_root lock_config in
        ShellCommand.mkdir_p absdir_intermediate;
        let* () = DepsConfig.write abspath_deps_config deps_config in
        Logging.end_deps_config_output logging_spec abspath_deps_config;

        (* Builds the document by invoking `satysfi`: *)
        let SatysfiCommand.{ exit_status; command = _ } =
          SatysfiCommand.build_document
            ~doc:abspath_doc
            ~out:abspath_out
            ~dump:abspath_dump
            ~deps:abspath_deps_config
            ~mode:text_mode_formats_str_opt
            ~options
        in
        return exit_status
  in
  match res with
  | Ok(exit_status) -> exit exit_status
  | Error(e)        -> ErrorReporting.report_config_error e; exit 1


type test_input =
  | PackageTestInput of {
      root     : abs_path;
      config   : abs_path;
      lock     : abs_path;
      envelope : abs_path;
      options  : SatysfiCommand.package_build_option;
    }


let test
    ~(fpath_in : string)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
    ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    let* test_input =
      let abspath_in = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
      if AbsPathIo.is_directory abspath_in then
        let options =
          SatysfiCommand.PackageBuildOption{
            show_full_path;
            verbosity;
          }
        in
        let abspath_package_config = Constant.library_package_config_path ~dir:abspath_in in
        let abspath_lock_config = Constant.library_lock_config_path ~dir:abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path ~dir:abspath_in in
        return @@ PackageTestInput{
          root     = abspath_in;
          config   = abspath_package_config;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
          options;
        }
      else
        err @@ CannotTestDocument
    in

    let* absdir_store_root = get_store_root () in

    match test_input with
    | PackageTestInput{
        root     = absdir_package;
        config   = abspath_package_config;
        lock     = abspath_lock_config;
        envelope = abspath_envelope_config;
        options;
      } ->
        (* Loads the package config: *)
        let*
          PackageConfig.{
            intermediate_directory;
            _
          } = PackageConfig.load abspath_package_config
        in

        (* Sets the path for the deps config: *)
        let absdir_intermediate =
          let intermediate_directory =
            Option.value
              ~default:Constant.default_intermediate_directory_name
              intermediate_directory
          in
          AbsPath.append_to_directory absdir_package intermediate_directory
        in
        let abspath_deps_config = Constant.library_deps_config_path ~dir:absdir_intermediate in

        (* Updates the deps config: *)
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config ~store_root:absdir_store_root lock_config in
        ShellCommand.mkdir_p absdir_intermediate;
        let* () = DepsConfig.write abspath_deps_config deps_config in
        Logging.end_deps_config_output logging_spec abspath_deps_config;

        (* Builds the package by invoking `satysfi`: *)
        let SatysfiCommand.{ exit_status; command = _ } =
          SatysfiCommand.test_package
            ~envelope:abspath_envelope_config
            ~deps:abspath_deps_config
            ~mode:text_mode_formats_str_opt
            ~options
        in
        return exit_status

  in
  match res with
  | Ok(exit_status) -> exit exit_status
  | Error(e)        -> ErrorReporting.report_config_error e; exit 1


let continue_if_ok res f =
  match res with
  | Ok(v)    -> f v
  | Error(_) -> ()


let cache_list
  ~(show_full_path : bool)
  ~(verbosity : verbosity)
=
  let res =
    let open ResultMonad in

    let absdir_current = AbsPathIo.getcwd () in
    let logging_spec = make_logging_spec ~show_full_path ~verbosity ~current_dir:absdir_current in

    (* Loads the store root config: *)
    let* absdir_store_root = get_store_root () in
    let abspath_store_root_config = Constant.store_root_config_path ~store_root:absdir_store_root in
    let* (store_root_config, created) = StoreRootConfig.load_or_initialize abspath_store_root_config in
    begin
      if created then Logging.store_root_config_updated logging_spec ~created:true abspath_store_root_config
    end;

    let StoreRootConfig.{ registries } = store_root_config in

    print_endline "Fetched package locks:";
    RegistryHashValueMap.fold (fun registry_hash_value (GitRegistry { url; branch }) () ->
      Printf.printf "- %s (Git URL: %s, branch: %s)\n" registry_hash_value url branch;
      let absdir_lock_tarball_cache =
        Constant.lock_tarball_cache_directory ~store_root:absdir_store_root registry_hash_value
      in
      let res = AbsPathIo.readdir absdir_lock_tarball_cache in
      continue_if_ok res (fun tarball_filenames ->
        tarball_filenames |> List.sort String.compare |> List.iter (fun tarball_filename ->
          Printf.printf "  - %s\n" tarball_filename
        )
      )
    ) registries ();

    print_endline "External resource archives:";
    RegistryHashValueMap.fold (fun registry_hash_value (GitRegistry { url; branch }) () ->
      Printf.printf "- %s (Git URL: %s, branch: %s)\n" registry_hash_value url branch;
      let absdir_external_resource_cache =
        Constant.external_resource_cache_directory ~store_root:absdir_store_root registry_hash_value
      in
      let res = AbsPathIo.readdir absdir_external_resource_cache in
      continue_if_ok res (fun dirs ->
        dirs |> List.sort String.compare |> List.iter (fun dir ->
          let abspath = AbsPath.append_to_directory absdir_external_resource_cache dir in
          let abspath_archives = AbsPath.append_to_directory abspath "archives" in
          if AbsPathIo.is_directory abspath then
            if AbsPathIo.is_directory abspath_archives then
              let res = AbsPathIo.readdir abspath_archives in
              continue_if_ok res (fun archive_filenames ->
                match archive_filenames with
                | [] ->
                    ()

                | _ :: _ ->
                    Printf.printf "  - %s\n" dir;
                    archive_filenames |> List.sort String.compare |> List.iter (fun archive_filename ->
                      Printf.printf "    - %s\n" archive_filename
                    )
              )
            else
              () (* TODO (error): report that `archives` is not a directory *)
          else
            () (* TODO (warning): warn the existence of (non-directory) files, if any *)
        )
      )
    ) registries ();

    print_endline "External resource extractions:";
    RegistryHashValueMap.fold (fun registry_hash_value (GitRegistry { url; branch }) () ->
      Printf.printf "- %s (Git URL: %s, branch: %s)\n" registry_hash_value url branch;
      let absdir_external_resource_cache =
        Constant.external_resource_cache_directory ~store_root:absdir_store_root registry_hash_value
      in
      let res = AbsPathIo.readdir absdir_external_resource_cache in
      continue_if_ok res (fun dirs ->
        dirs |> List.sort String.compare |> List.iter (fun dir ->
          let abspath = AbsPath.append_to_directory absdir_external_resource_cache dir in
          let abspath_extractions = AbsPath.append_to_directory abspath "extractions" in
          if AbsPathIo.is_directory abspath then
            if AbsPathIo.is_directory abspath_extractions then
              let res = AbsPathIo.readdir abspath_extractions in
              continue_if_ok res (fun extraction_dirs ->
                match extraction_dirs with
                | [] ->
                    ()

                | _ :: _ ->
                    Printf.printf "  - %s\n" dir;
                    extraction_dirs |> List.sort String.compare |> List.iter (fun extraction_dir ->
                      Printf.printf "    - %s\n" extraction_dir
                    )
              )
            else
              () (* TODO (error): report that `extractions` is not a directory *)
          else
            () (* TODO (warning): warn the existence of (non-directory) files, if any *)
        )
      )
    ) registries ();

    return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> ErrorReporting.report_config_error e; exit 1
