
open MyUtil
open PackageSystemBase
open ConfigError


let version =
  Printf.sprintf "Saphe version %s alpha"
    (SemanticVersion.to_string Constant.current_ecosystem_version)


type line =
  | NormalLine  of string
  | DisplayLine of string


let report_error (lines : line list) : unit =
  print_string "! ";
  lines |> List.fold_left (fun (is_first : bool) (line : line) ->
    begin
      match line with
      | NormalLine(s) ->
          if is_first then
            print_endline s
          else
            print_endline ("    " ^ s)

      | DisplayLine(s) ->
          if is_first then
            print_endline ("\n      " ^ s)
          else
            print_endline ("      " ^ s)
    end;
    false
  ) true |> ignore


let show_yaml_context (context : YamlDecoder.context) : string =
  match context with
  | [] ->
      ""

  | _ :: _ ->
      let s_context =
        let open YamlDecoder in
        context |> List.map (function
        | Field(field) -> Printf.sprintf ".%s" field
        | Index(index) -> Printf.sprintf ".[%d]" index
        ) |> String.concat ""
      in
      Printf.sprintf " (context: %s)" s_context


let make_yaml_error_lines : yaml_error -> line list = function
  | ParseError(s) ->
      [ NormalLine(Printf.sprintf "parse error: %s" s) ]

  | FieldNotFound(yctx, field) ->
      [ NormalLine(Printf.sprintf "field '%s' not found%s" field (show_yaml_context yctx)) ]

  | NotAFloat(yctx) ->
      [ NormalLine(Printf.sprintf "not a float value%s" (show_yaml_context yctx)) ]

  | NotAString(yctx) ->
      [ NormalLine(Printf.sprintf "not a string value%s" (show_yaml_context yctx)) ]

  | NotABool(yctx) ->
      [ NormalLine(Printf.sprintf "not a Boolean value%s" (show_yaml_context yctx)) ]

  | NotAnArray(yctx) ->
      [ NormalLine(Printf.sprintf "not an array%s" (show_yaml_context yctx)) ]

  | NotAnObject(yctx) ->
      [ NormalLine(Printf.sprintf "not an object%s" (show_yaml_context yctx)) ]

  | UnexpectedTag(yctx, tag) ->
      [ NormalLine(Printf.sprintf "unexpected type tag '%s'%s" tag (show_yaml_context yctx)) ]

  | BreaksVersionRequirement(yctx, requirement) ->
      [ NormalLine(Printf.sprintf "breaks the requrement '%s'%s" (SemanticVersion.requirement_to_string requirement)(show_yaml_context yctx)) ]

  | NotASemanticVersion(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a semantic version: '%s'%s" s (show_yaml_context yctx)) ]

  | NotAVersionRequirement(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a version requirement: '%s'%s" s (show_yaml_context yctx)) ]

  | InvalidPackageName(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a package name: '%s'%s" s (show_yaml_context yctx)) ]

  | DuplicateRegistryLocalName{ context = yctx; registry_local_name } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry local name '%s'%s" registry_local_name (show_yaml_context yctx)) ]

  | DuplicateRegistryHashValue{ context = yctx; registry_hash_value } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry hash value '%s'%s" registry_hash_value (show_yaml_context yctx)) ]

  | CannotBeUsedAsAName(yctx, s) ->
      [ NormalLine(Printf.sprintf "'%s' cannot be used as a name%s" s (show_yaml_context yctx)) ]

  | UnsupportedConfigFormat(format) ->
      [ NormalLine(Printf.sprintf "unsupported config format '%s'" format) ]

  | NotACommand{ context = yctx; prefix = _; string = s } ->
      [ NormalLine(Printf.sprintf "not a command: '%s'%s" s (show_yaml_context yctx)) ]


let report_config_error = function
  | CannotDetermineStoreRoot{ envvar } ->
      report_error [
        NormalLine("cannot determine where the store root is;");
        NormalLine(Printf.sprintf "set environment variable '%s'." envvar);
      ]

  | PackageDirectoryNotFound(candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun path ->
          DisplayLine(Printf.sprintf "- %s" path)
        )
      in
      report_error
        (NormalLine("cannot find package directory. candidates:") :: lines)

  | PackageConfigNotFound(abspath_package_config) ->
      report_error [
        NormalLine("cannot find a package config:");
        DisplayLine(get_abs_path_string abspath_package_config);
      ]

  | PackageConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: package config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a lock config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LockConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: lock config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | RegistryConfigNotFound(abspath_registry_config) ->
      report_error [
        NormalLine("cannot find a registry config:");
        DisplayLine(get_abs_path_string abspath_registry_config);
      ]

  | RegistryConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: registry config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | StoreRootConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a store root config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | StoreRootConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: store root config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockNameConflict(lock_name) ->
      report_error [
        NormalLine(Printf.sprintf "lock name conflict: '%s'" lock_name);
      ]

  | LockedPackageNotFound(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error
        (NormalLine(Printf.sprintf "package '%s' not found. candidates:" (get_lib_path_string libpath)) :: lines)

  | DependencyOnUnknownLock{ depending; depended } ->
      report_error [
        NormalLine(Printf.sprintf "unknown depended lock '%s' of '%s'." depended depending);
      ]
(*
  | CyclicLockDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      let lines =
        pairs |> List.map (fun (modnm, _lock) ->
          DisplayLine(Printf.sprintf "- '%s'" modnm)
        )
      in
      report_error
        (NormalLine("the following packages are cyclic:") :: lines)
*)
  | CannotFindLibraryFile(libpath, candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | CannotSolvePackageConstraints ->
      report_error [
        NormalLine("cannot solve package constraints.");
      ]
(*
  | DocumentAttributeError(e) ->
      report_document_attribute_error e

  | MarkdownClassNotFound(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' not found; required for converting Markdown documents." modnm);
      ]

  | NoMarkdownConversion(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' contains no Markdown conversion rule." modnm);
      ]

  | MoreThanOneMarkdownConversion(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' contains more than one Markdown conversion rule." modnm);
      ]

  | MarkdownError(e) ->
      begin
        match e with
        | InvalidHeaderComment ->
            report_error [
              NormalLine("invalid or missing header comment of a Markdown document.");
            ]

        | InvalidExtraExpression ->
            report_error [
              NormalLine("cannot parse an extra expression in a Markdown document.");
            ]

        | FailedToMakeDocumentAttribute(de) ->
            report_document_attribute_error de
      end
*)
  | FailedToFetchTarball{ lock_name; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to fetch '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToExtractTarball{ lock_name; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to extract the tarball of '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToFetchExternalZip{ url; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to fetch file from '%s' (exit status: %d). command:" url exit_status);
        DisplayLine(command);
      ]

  | ExternalZipChecksumMismatch{ url; path; expected; got } ->
      report_error [
        NormalLine("checksum mismatch of an external zip file.");
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | TarGzipChecksumMismatch{ lock_name; url; path; expected; got } ->
      report_error [
        NormalLine("checksum mismatch of a tarball.");
        DisplayLine(Printf.sprintf "- lock name: '%s'" lock_name);
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | FailedToExtractExternalZip{ exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to extract a zip file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | FailedToCopyFile{ exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to copy a file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | PackageRegistryFetcherError(e) ->
      begin
        match e with
        | FailedToUpdateGitRegistry{ exit_status; command } ->
            report_error [
              NormalLine(Printf.sprintf "failed to update registry (exit status: %d). command:" exit_status);
              DisplayLine(command);
            ]
      end

  | CanonicalRegistryUrlError(e) ->
      begin
        match e with
        | ContainsQueryParameter{ url } ->
            report_error [
              NormalLine("registry URLs must not contain query parameters:");
              DisplayLine(url);
            ]

        | NoUriScheme{ url } ->
            report_error [
              NormalLine("the registry URL does not contain a scheme:");
              DisplayLine(url);
            ]

        | UnexpectedUrlScheme{ url; scheme } ->
            report_error [
              NormalLine(Printf.sprintf "unexpected scheme '%s' in a registry URL:" scheme);
              DisplayLine(url);
            ]
      end

  | CannotWriteEnvelopeConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write an envelope config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | CannotWriteLockConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a lock config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | MultiplePackageDefinition{ package_name } ->
      report_error [
        NormalLine(Printf.sprintf "More than one definition for package '%s'." package_name)
      ]


type solve_input =
  | PackageSolveInput of {
      root     : abs_path; (* The absolute path of a directory used as the package root *)
      lock     : abs_path; (* A path for writing a resulting lock config file *)
      envelope : abs_path; (* A path for writing a resulting envelope config file *)
    }
  | DocumentSolveInput of {
      doc    : abs_path; (* The absolute path to the document file *)
      config : abs_path; (* The absolute path to the config file *)
      lock   : abs_path; (* A path for writing a resulting lock file *)
    }


let make_registry_hash_value (registry_remote : registry_remote) : (registry_hash_value, config_error) result =
  let open ResultMonad in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      let* canonicalized_url =
        CanonicalRegistryUrl.make url
          |> Result.map_error (fun e -> CanonicalRegistryUrlError(e))
      in
      let hash_value =
        Digest.to_hex (Digest.string (Printf.sprintf "git#%s#%s" canonicalized_url branch))
      in
      Logging.report_canonicalized_url ~url ~canonicalized_url ~hash_value;
      return hash_value


let update_store_root_config_if_needed (registries : registry_remote RegistryHashValueMap.t) (registry_hash_value : registry_hash_value) (registry_remote : registry_remote) (abspath_store_root : abs_path) : unit =
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
      ()


let make_lock_name (lock : Lock.t) : lock_name =
  let Lock.{ package_id; locked_version } = lock in
  let PackageId.{ registry_hash_value; package_name } = package_id in
  Printf.sprintf "registered.%s.%s.%s"
    registry_hash_value
    package_name
    (SemanticVersion.to_string locked_version)


let make_lock_dependency (dep : locked_dependency) : LockConfig.lock_dependency =
  {
    depended_lock_name = make_lock_name dep.depended_lock;
    used_as            = dep.dependency_used_as;
  }


let convert_solutions_to_lock_config (solutions : package_solution list) : LockConfig.t * implementation_spec list =
  let (locked_package_acc, impl_spec_acc) =
    solutions |> List.fold_left (fun (locked_package_acc, impl_spec_acc) solution ->
      let lock = solution.lock in
      let Lock.{ package_id; locked_version = version } = lock in
      let PackageId.{ registry_hash_value; package_name } = package_id in
      let locked_package =
        LockConfig.{
          lock_name         = make_lock_name lock;
          lock_contents     = RegisteredLock{ registry_hash_value; package_name; version };
          lock_dependencies = solution.locked_dependencies |> List.map make_lock_dependency;
          test_only_lock    = solution.used_in_test_only;
        }
      in
      let impl_spec =
        ImplSpec{
          lock   = solution.lock;
          source = solution.locked_source;
        }
      in
      (Alist.extend locked_package_acc locked_package, Alist.extend impl_spec_acc impl_spec)
    ) (Alist.empty, Alist.empty)
  in
  let explicit_dependencies =
    solutions |> List.filter_map (fun solution ->
      solution.explicitly_depended |> Option.map (fun used_as ->
        LockConfig.{
          depended_lock_name = make_lock_name solution.lock;
          used_as;
        }
      )
    )
  in
  let lock_config =
    LockConfig.{
      locked_packages = Alist.to_list locked_package_acc;
      explicit_dependencies;
    }
  in
  (lock_config, Alist.to_list impl_spec_acc)


(*
let extract_attributes_from_document_file (display_config : Logging.config) (input_kind : input_kind) (abspath_in : abs_path) : (DocumentAttribute.t, config_error) result =
  let open ResultMonad in
  Logging.begin_to_parse_file display_config abspath_in;
  match input_kind with
  | InputSatysfi ->
      let* utsrc =
        ParserInterface.process_file abspath_in
          |> Result.map_error (fun rng -> FailedToParse(rng))
      in
      let* (attrs, _header, _utast) =
        match utsrc with
        | UTLibraryFile(_)      -> err @@ DocumentLacksWholeReturnValue(abspath_in)
        | UTDocumentFile(utdoc) -> return utdoc
      in
      DocumentAttribute.make attrs
        |> Result.map_error (fun e -> DocumentAttributeError(e))

  | InputMarkdown ->
      let* (docattr, _main_module_name, _md) =
        match read_file abspath_in with
        | Ok(data)   -> MarkdownParser.decode data |> Result.map_error (fun e -> MarkdownError(e))
        | Error(msg) -> err (CannotReadFileOwingToSystem(msg))
      in
      return docattr
*)


let make_envelope_config (package_contents : PackageConfig.package_contents) : EnvelopeConfig.t =
  match package_contents with
  | PackageConfig.Library{
      main_module_name;
      source_directories;
      test_directories;
      _
    } ->
      EnvelopeConfig.{
        envelope_contents =
          Library{
            main_module_name;
            source_directories;
            test_directories;
            conversion_specs = []; (* TODO *)
          };
      }

  | PackageConfig.Font{
      main_module_name;
      font_file_descriptions = descrs;
    } ->
      let font_file_descriptions =
        descrs |> List.map (fun descr ->
          let
            PackageConfig.{
              font_file_path;
              font_file_contents;
            } = descr
          in
          EnvelopeConfig.{
            font_file_path;
            font_file_contents;
          }
        )
      in
      EnvelopeConfig.{
        envelope_contents =
          Font{
            main_module_name;
            font_file_descriptions;
          };
      }


let get_store_root () : (abs_path, config_error) result =
  let open ResultMonad in
  let envvar_home =
    if String.equal Sys.os_type "Win32" then
      "userprofile"
    else
      "HOME"
  in
  match Sys.getenv_opt envvar_home with
  | None       -> err @@ CannotDetermineStoreRoot { envvar = envvar_home }
  | Some(home) -> return @@ make_abs_path (Filename.concat home ".saphe")


let solve ~(fpath_in : string) =
  let res =
    let open ResultMonad in

    (* Constructs the input: *)
    let solve_input =
      let absdir_current = Sys.getcwd () in
      let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
      if is_directory abspath_in then
      (* If the input is a directory that forms a package: *)
        let abspath_lock_config = Constant.library_lock_config_path abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path abspath_in in
        PackageSolveInput{
          root     = abspath_in;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
        }
      else
        let abspath_package_config = Constant.document_package_config_path abspath_in in
        let abspath_lock_config = Constant.document_lock_config_path abspath_in in
        DocumentSolveInput{
          doc    = abspath_in;
          config = abspath_package_config;
          lock   = abspath_lock_config;
        }
    in

    let* (language_version, dependencies_with_flags, abspath_lock_config, registry_specs) =
      match solve_input with
      | PackageSolveInput{
          root     = absdir_package;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
        } ->
          let abspath_package_config = Constant.library_package_config_path absdir_package in
          let*
            PackageConfig.{
              language_requirement;
              package_contents;
              registry_specs;
              _
            } = PackageConfig.load abspath_package_config
          in
          let language_version =
            match language_requirement with
            | SemanticVersion.CompatibleWith(semver) -> semver
                (* Selects the minimum version according to the user's designation for the moment.
                   TODO: take dependencies into account when selecting a language version *)
          in

          (* Writes the envelope config: *)
          let envelope_config = make_envelope_config package_contents in
          let* () = EnvelopeConfig.write abspath_envelope_config envelope_config in
          Logging.end_envelope_config_output abspath_envelope_config;

          begin
            match package_contents with
            | PackageConfig.Library{ dependencies; test_dependencies; _ } ->
                let dependencies_with_flags =
                  List.append
                    (dependencies |> List.map (fun dep -> (SourceDependency, dep)))
                    (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))
                in
                return (language_version, dependencies_with_flags, abspath_lock_config, registry_specs)

            | PackageConfig.Font(_) ->
                return (language_version, [], abspath_lock_config, registry_specs)
          end

      | DocumentSolveInput{
          doc    = _abspath_doc;
          config = _abspath_package_config;
          lock   = _abspath_lock_config;
        } ->
          failwith "TODO: DocumentSolveInput"
(*
          let* DocumentAttribute.{ registry_specs; dependencies } =
            extract_attributes_from_document_file display_config input_kind abspath_in
          in
          let dependencies_with_flags = dependencies |> List.map (fun dep -> (SourceDependency, dep)) in
          return (dependencies_with_flags, abspath_lock_config, registry_specs)
*)
    in

    Logging.show_package_dependency_before_solving dependencies_with_flags;

    let* absdir_store_root = get_store_root () in
    let abspath_store_root_config = Constant.store_root_config_path absdir_store_root in
    let* store_root_config = StoreRootConfig.load abspath_store_root_config in

    let* registries =
      RegistryLocalNameMap.fold (fun registry_local_name registry_remote res ->
        let* registries = res in
        let* registry_hash_value = make_registry_hash_value registry_remote in

        (* Manupulates the store root config: *)
        update_store_root_config_if_needed
          store_root_config.StoreRootConfig.registries
          registry_hash_value
          registry_remote
          abspath_store_root_config;

        (* Fetches registry configs: *)
        let absdir_registry_repo =
          Constant.registry_root_directory_path absdir_store_root registry_hash_value
        in
        let git_command = "git" in (* TODO: make this changeable *)
        let* () =
          PackageRegistryFetcher.main ~git_command absdir_registry_repo registry_remote
            |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
        in

        let* PackageRegistryConfig.{ packages = packages } =
          let abspath_registry_config =
            make_abs_path
              (Filename.concat
                (get_abs_path_string absdir_registry_repo)
                Constant.package_registry_config_file_name)
          in
          PackageRegistryConfig.load abspath_registry_config
        in
        let* packages_in_registry =
          packages |> List.fold_left (fun res (package_name, impls) ->
            let* map = res in
            let package_id = PackageId.{ registry_hash_value; package_name } in
            if map |> PackageIdMap.mem package_id then
              err @@ MultiplePackageDefinition{ package_name }
            else
              return (map |> PackageIdMap.add package_id impls)
          ) (return PackageIdMap.empty)
        in
        let registry_spec = { packages_in_registry; registry_hash_value } in
        return (registries |> RegistryLocalNameMap.add registry_local_name registry_spec)

      ) registry_specs (return RegistryLocalNameMap.empty)
    in

    let package_context = { registries; language_version } in
    let solutions_opt = PackageConstraintSolver.solve package_context dependencies_with_flags in
    begin
      match solutions_opt with
      | None ->
          err CannotSolvePackageConstraints

      | Some(solutions) ->

          Logging.show_package_dependency_solutions solutions;

          let (lock_config, impl_specs) = convert_solutions_to_lock_config solutions in

          let wget_command = "wget" in (* TODO: make this changeable *)
          let tar_command = "tar" in (* TODO: make this changeable *)
          let unzip_command = "unzip" in (* TODO: make this changeable *)
          let* () =
            impl_specs |> foldM (fun () impl_spec ->
              LockFetcher.main
                ~wget_command ~tar_command ~unzip_command ~store_root:absdir_store_root impl_spec
            ) ()
          in
          let* () = LockConfig.write abspath_lock_config lock_config in
          Logging.end_lock_config_output abspath_lock_config;
          return ()
    end
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> report_config_error e; exit 1


type build_option = {
    text_mode              : string option;
    page_number_limit      : int;
    show_full_path         : bool;
    debug_show_bbox        : bool;
    debug_show_space       : bool;
    debug_show_block_bbox  : bool;
    debug_show_block_space : bool;
    debug_show_overfull    : bool;
    type_check_only        : bool;
    bytecomp               : bool;
}


type build_input =
  | PackageBuildInput of {
      root     : abs_path;
      lock     : abs_path;
      deps     : abs_path;
      envelope : abs_path;
      options  : build_option;
    }
  | DocumentBuildInput of {
      lock    : abs_path;
      deps    : abs_path;
      out     : abs_path;
      dump    : abs_path;
      options : build_option;
    }


let make_envelope_dependency (lock_dep : LockConfig.lock_dependency) : DepsConfig.envelope_dependency =
  DepsConfig.{
    dependency_name    = lock_dep.depended_lock_name;
    dependency_used_as = lock_dep.used_as;
  }


let make_envelope_spec (locked_package : LockConfig.locked_package) : DepsConfig.envelope_spec =
  let envelope_dependencies =
    locked_package.lock_dependencies |> List.map make_envelope_dependency
  in
  DepsConfig.{
    envelope_name = locked_package.lock_name;
    envelope_path = failwith "TODO: make_envelope_spec, envelope_path";
    envelope_dependencies;
  }


let make_deps_config (lock_config : LockConfig.t) : DepsConfig.t =
  let envelopes =
    lock_config.LockConfig.locked_packages |> List.map make_envelope_spec
  in
  let explicit_dependencies =
    lock_config.LockConfig.explicit_dependencies
      |> List.map make_envelope_dependency
  in
  DepsConfig.{ envelopes; explicit_dependencies }


let build
    ~(fpath_in : string)
    ~(fpath_out_opt : string option)
    ~(text_mode_formats_str_opt : string option)
    ~(page_number_limit : int)
    ~(show_full_path : bool)
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

    (* Constructs the input: *)
    let build_input =
      let build_option =
        {
          text_mode   = text_mode_formats_str_opt;
          page_number_limit;
          show_full_path;
          debug_show_bbox;
          debug_show_space;
          debug_show_block_bbox;
          debug_show_block_space;
          debug_show_overfull;
          type_check_only;
          bytecomp;
        }
      in
      let absdir_current = Sys.getcwd () in
      let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
      if is_directory abspath_in then
        let abspath_lock_config = Constant.library_lock_config_path abspath_in in
        let abspath_deps_config = Constant.library_deps_config_path abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path abspath_in in
        PackageBuildInput{
          root     = abspath_in;
          lock     = abspath_lock_config;
          deps     = abspath_deps_config;
          envelope = abspath_envelope_config;
          options  = build_option;
        }
      else
        let abspath_lock_config = Constant.document_lock_config_path abspath_in in
        let abspath_deps_config = Constant.document_deps_config_path abspath_in in
        let abspath_out =
          match fpath_out_opt with
          | None ->
              Constant.default_output_path abspath_in

          | Some(fpath_out) ->
              make_absolute_if_relative ~origin:absdir_current fpath_out
        in
        let abspath_dump = Constant.dump_path abspath_in in
        DocumentBuildInput{
          lock    = abspath_lock_config;
          deps    = abspath_deps_config;
          out     = abspath_out;
          dump    = abspath_dump;
          options = build_option;
        }
    in

    match build_input with
    | PackageBuildInput{
        root     = _absdir_package;
        lock     = abspath_lock_config;
        deps     = abspath_deps_config;
        envelope = _abspath_envelope_config;
        options  = _build_option;
      } ->
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config lock_config in

        let* () = DepsConfig.write abspath_deps_config deps_config in
        failwith "TODO: PackageBuildInput"

    | DocumentBuildInput{
        lock    = abspath_lock_config;
        deps    = abspath_deps_config;
        out     = _abspath_out;
        dump    = _abspath_dump;
        options = _build_option;
      } ->
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config lock_config in

        let* () = DepsConfig.write abspath_deps_config deps_config in
        failwith "TODO: DocumentBuildInput"
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> report_config_error e; exit 1
