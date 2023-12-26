
open MyUtil
open PackageSystemBase
open ConfigError


exception ConfigError of config_error


let version =
  "Saphe version 0.0.1 alpha"


type line =
  | NormalLine  of string
  | DisplayLine of string

let report_error (lines : line list) =
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
  ) true |> ignore;
  exit 1


let show_yaml_context (context : YamlDecoder.context) =
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

  | UnexpectedLanguage(s_language_version) ->
      [ NormalLine(Printf.sprintf "unexpected language version '%s'" s_language_version) ]

  | NotASemanticVersion(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a semantic version: '%s'%s" s (show_yaml_context yctx)) ]

  | NotAVersionRequirement(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a version requirement: '%s'%s" s (show_yaml_context yctx)) ]

  | InvalidPackageName(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a package name: '%s'%s" s (show_yaml_context yctx)) ]

  | MultiplePackageDefinition{ context = yctx; package_name } ->
      [ NormalLine(Printf.sprintf "More than one definition for package '%s'%s" package_name (show_yaml_context yctx)) ]

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
  | PackageDirectoryNotFound(candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun path ->
          DisplayLine(Printf.sprintf "- %s" path)
        )
      in
      report_error
        (NormalLine("cannot find package directory. candidates:") :: lines)

  | PackageConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a package config at:");
        DisplayLine(get_abs_path_string abspath);
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

  | RegistryConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a registry config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | RegistryConfigNotFoundIn(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "cannot find a registry config '%s'. candidates:" (get_lib_path_string libpath)) ];
        lines;
      ])

  | RegistryConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: registry config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LibraryRootConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a library root config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LibraryRootConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: library root config error;" (get_abs_path_string abspath)) ];
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


let error_log_environment (suspended : unit -> unit) : unit =
  try
    suspended ()
  with
  | ConfigError(e) ->
      report_config_error e


let make_package_lock_config_path (abspathstr_in : string) =
  make_abs_path (Printf.sprintf "%s/package.satysfi-lock" abspathstr_in)


let make_document_lock_config_path (basename_without_extension : string) =
  make_abs_path (Printf.sprintf "%s.satysfi-lock" basename_without_extension)


type solve_input =
  | PackageSolveInput of {
      root : abs_path; (* The absolute path of a directory used as the package root *)
      lock : abs_path; (* A path for writing a resulting lock file *)
    }
  | DocumentSolveInput of {
      path : abs_path; (* The absolute path to the document file *)
      lock : abs_path; (* A path for writing a resulting lock file *)
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


let update_library_root_config_if_needed (registries : registry_remote RegistryHashValueMap.t) (registry_hash_value : registry_hash_value) (registry_remote : registry_remote) (abspath_library_root : abs_path) : unit =
  match
    registries |> RegistryHashValueMap.find_opt registry_hash_value
  with
  | None ->
      let library_root_config =
        LibraryRootConfig.{
          registries = registries |> RegistryHashValueMap.add registry_hash_value registry_remote;
        }
      in
      LibraryRootConfig.write abspath_library_root library_root_config

  | Some(_registry_remote) ->
      ()


let make_lock_name (lock : Lock.t) : lock_name =
  let Lock.{ registry_hash_value; package_name; locked_version } = lock in
  Printf.sprintf "registered.%s.%s.%s"
    registry_hash_value
    package_name
    (SemanticVersion.to_string locked_version)


let convert_solutions_to_lock_config (solutions : package_solution list) : LockConfig.t * implementation_spec list =
  let (locked_package_acc, impl_spec_acc) =
    solutions |> List.fold_left (fun (locked_package_acc, impl_spec_acc) solution ->
      let lock = solution.lock in
      let Lock.{ registry_hash_value; package_name; locked_version = version } = lock in
      let locked_package =
        LockConfig.{
          lock_name         = make_lock_name lock;
          lock_contents     = RegisteredLock{ registry_hash_value; package_name; version };
          lock_dependencies = solution.locked_dependencies |> List.map make_lock_name;
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
  let lock_config = LockConfig.{ locked_packages = Alist.to_list locked_package_acc } in
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

let solve
    ~(fpath_in : string)
    ~show_full_path:(_ : bool)
    ~config_paths_str_opt:(_ : string option)
    ~no_default_config:(_ : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let library_root =
      if String.equal Sys.os_type "Win32" then
        match Sys.getenv_opt "userprofile" with
        | None    -> failwith "TODO: userprofile is not set"
        | Some(s) -> make_abs_path (Filename.concat s ".saphe")
      else
        match Sys.getenv_opt "HOME" with
        | None    -> failwith "TODO: HOME is not set"
        | Some(s) -> make_abs_path (Filename.concat s ".saphe")
    in
    let abspath_in = make_absolute_if_relative ~origin:curdir fpath_in in
    let solve_input =
      let abspathstr_in = get_abs_path_string abspath_in in
      if Sys.is_directory abspathstr_in then
      (* If the input is a package directory: *)
        let abspath_lock_config = make_package_lock_config_path abspathstr_in in
        PackageSolveInput{
          root = abspath_in;
          lock = abspath_lock_config;
        }
      else
        let abspathstr_in = get_abs_path_string abspath_in in
        let basename_without_extension = Filename.remove_extension abspathstr_in in
        let abspath_lock_config = make_document_lock_config_path basename_without_extension in
        DocumentSolveInput{
          path = abspath_in;
          lock = abspath_lock_config;
        }
    in

    let res =
      let open ResultMonad in
      let abspath_library_root_config =
        make_abs_path
          (Filename.concat
            (get_abs_path_string library_root)
            (get_lib_path_string Constant.library_root_config_file))
      in
      let* library_root_config = LibraryRootConfig.load abspath_library_root_config in
      let* (dependencies_with_flags, abspath_lock_config, registry_specs) =
        match solve_input with
        | PackageSolveInput{
            root = absdir_package;
            lock = abspath_lock_config;
          } ->
            let* PackageConfig.{ package_contents; registry_specs; _ } = PackageConfig.load absdir_package in
            begin
              match package_contents with
              | PackageConfig.Library{ dependencies; test_dependencies; _ } ->
                  let dependencies_with_flags =
                    List.append
                      (dependencies |> List.map (fun dep -> (SourceDependency, dep)))
                      (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))
                  in
                  return (dependencies_with_flags, abspath_lock_config, registry_specs)

              | PackageConfig.Font(_) ->
                  return ([], abspath_lock_config, registry_specs)
            end

        | DocumentSolveInput{
            path = _abspath_in;
            lock = _abspath_lock_config;
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

      let* registries =
        RegistryLocalNameMap.fold (fun registry_local_name registry_remote res ->
          let* registries = res in
          let* registry_hash_value = make_registry_hash_value registry_remote in

          (* Manupulates the library root config: *)
          update_library_root_config_if_needed
            library_root_config.LibraryRootConfig.registries
            registry_hash_value
            registry_remote
            abspath_library_root_config;

          (* Fetches registry configs: *)
          let absdir_registry_repo =
            let libpath_registry_root = Constant.registry_root_directory registry_hash_value in
            make_abs_path
              (Filename.concat
                (get_abs_path_string library_root)
                (get_lib_path_string libpath_registry_root))
          in
          let git_command = "git" in (* TODO: make this changeable *)
          let* () =
            PackageRegistryFetcher.main ~git_command absdir_registry_repo registry_remote
              |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
          in

          let* PackageRegistryConfig.{ packages = packages_in_registry } =
            let abspath_registry_config =
              make_abs_path
                (Filename.concat
                  (get_abs_path_string absdir_registry_repo)
                  Constant.package_registry_config_file_name)
            in
            PackageRegistryConfig.load abspath_registry_config
          in
          let registry_spec = { packages_in_registry; registry_hash_value } in
          return (registries |> RegistryLocalNameMap.add registry_local_name registry_spec)

        ) registry_specs (return RegistryLocalNameMap.empty)
      in

      let package_context = { registries } in
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
                  ~wget_command ~tar_command ~unzip_command ~library_root impl_spec
              ) ()
            in
            LockConfig.write abspath_lock_config lock_config;
            return ()
      end
    in
    begin
      match res with
      | Ok(())   -> ()
      | Error(e) -> raise (ConfigError(e))
    end
  )
