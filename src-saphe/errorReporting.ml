
open MyUtil
open ConfigError


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


let show_yaml_context (yctx : YamlDecoder.context) =
  Printf.sprintf "(context: %s)" (YamlDecoder.show_yaml_context yctx)


let make_yaml_error_lines : yaml_error -> line list = function
  | ParseError(s) ->
      [ NormalLine(Printf.sprintf "parse error: %s" s) ]

  | FieldNotFound(yctx, field) ->
      [ NormalLine(Printf.sprintf "field '%s' not found %s" field (show_yaml_context yctx)) ]

  | NotAFloat(yctx) ->
      [ NormalLine(Printf.sprintf "not a float value %s" (show_yaml_context yctx)) ]

  | NotAString(yctx) ->
      [ NormalLine(Printf.sprintf "not a string value %s" (show_yaml_context yctx)) ]

  | NotABool(yctx) ->
      [ NormalLine(Printf.sprintf "not a Boolean value %s" (show_yaml_context yctx)) ]

  | NotAnArray(yctx) ->
      [ NormalLine(Printf.sprintf "not an array %s" (show_yaml_context yctx)) ]

  | NotAnObject(yctx) ->
      [ NormalLine(Printf.sprintf "not an object %s" (show_yaml_context yctx)) ]

  | BreaksVersionRequirement(yctx, requirement) ->
      [ NormalLine(Printf.sprintf "breaks the requrement '%s' %s" (SemanticVersion.requirement_to_string requirement)(show_yaml_context yctx)) ]

  | NotASemanticVersion(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a semantic version: '%s' %s" s (show_yaml_context yctx)) ]

  | NotAVersionRequirement(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a version requirement: '%s' %s" s (show_yaml_context yctx)) ]

  | InvalidPackageName(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a package name: '%s' %s" s (show_yaml_context yctx)) ]

  | InvalidRegistryHashValue{ context = yctx; got } ->
      [ NormalLine(Printf.sprintf "invalid string '%s' for registry hash value %s" got (show_yaml_context yctx))]

  | DuplicateRegistryHashValue{ context = yctx; registry_hash_value } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry hash value '%s' %s" registry_hash_value (show_yaml_context yctx)) ]

  | CannotBeUsedAsAName(yctx, s) ->
      [ NormalLine(Printf.sprintf "'%s' cannot be used as a name %s" s (show_yaml_context yctx)) ]

  | UnsupportedRegistryFormat(format) ->
      [ NormalLine(Printf.sprintf "unsupported registry format '%s'" format) ]

  | NotACommand{ context = yctx; prefix = _; string = s } ->
      [ NormalLine(Printf.sprintf "not a command: '%s' %s" s (show_yaml_context yctx)) ]

  | BranchNotFound{ context = yctx; expected_tags; got_tags } ->
      [
        NormalLine(Printf.sprintf "expected tags not found; should contain exactly one of:");
        DisplayLine(expected_tags |> String.concat ", ");
        NormalLine("but only contains:");
        DisplayLine(got_tags |> String.concat ", ");
        NormalLine(Printf.sprintf "%s" (show_yaml_context yctx));
      ]

  | MoreThanOneBranchFound{ context = yctx; expected_tags; got_tags } ->
      [
        NormalLine(Printf.sprintf "more than one expected tag found:");
        DisplayLine(got_tags |> String.concat ", ");
        NormalLine("should be exactly one of:");
        DisplayLine(expected_tags |> String.concat ", ");
        NormalLine(Printf.sprintf "%s" (show_yaml_context yctx));
      ]



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

  | NotAPackageButADocument(abspath_package_config) ->
      report_error [
        NormalLine(Printf.sprintf "in %s:" (get_abs_path_string abspath_package_config));
        NormalLine("this file is expected to be a config for a package,");
        NormalLine("but is for a document.");
      ]

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

  | ReleaseConfigNotFound(abspath_release_config) ->
      report_error [
        NormalLine("cannot find a release config:");
        DisplayLine(get_abs_path_string abspath_release_config);
      ]

  | ReleaseConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: release config error;" (get_abs_path_string abspath)) ];
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
        | FailedToFetchGitRegistry{ exit_status; command } ->
            report_error [
              NormalLine(Printf.sprintf "failed to fetch registry (exit status: %d). command:" exit_status);
              DisplayLine(command);
            ]

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

  | CannotWriteDepsConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a deps config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | CannotWriteStoreRootConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a store root config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | MultiplePackageDefinition{ package_name } ->
      report_error [
        NormalLine(Printf.sprintf "More than one definition for package '%s'." package_name)
      ]

  | DuplicateRegistryLocalName{ registry_local_name } ->
      report_error [
        NormalLine(Printf.sprintf "more than one definition for registry local name '%s'" registry_local_name)
      ]

  | UndefinedRegistryLocalName{ registry_local_name } ->
      report_error [
        NormalLine(Printf.sprintf "undefined registry local name '%s'" registry_local_name)
      ]

  | CannotTestDocument ->
      report_error [
        NormalLine("cannot run tests for documents (at least so far)");
      ]

  | FileAlreadyExists{ path } ->
      report_error [
        NormalLine(Printf.sprintf "file already exists: '%s'" (get_abs_path_string path));
      ]

  | InvalidExtensionForDocument{ path; extension } ->
      report_error [
        NormalLine(Printf.sprintf "invalid extension '%s' for a document: '%s'" extension (get_abs_path_string path));
      ]

  | FailedToWriteFile{ path; message } ->
      report_error [
        NormalLine(Printf.sprintf "failed to write file '%s':" (get_abs_path_string path));
        DisplayLine(message);
      ]

  | NotALibraryLocalFixed{ dir = absdir_package } ->
      report_error [
        NormalLine(Printf.sprintf "the following local package is not a library:");
        DisplayLine(get_abs_path_string absdir_package);
      ]

  | LocalFixedDoesNotSupportLanguageVersion{ dir = absdir_package; language_version; language_requirement } ->
      let s_version = SemanticVersion.to_string language_version in
      let s_req = SemanticVersion.requirement_to_string language_requirement in
      report_error [
        NormalLine(Printf.sprintf "the local package");
        DisplayLine(get_abs_path_string absdir_package);
        NormalLine(Printf.sprintf "requires the language version to be %s," s_req);
        NormalLine(Printf.sprintf "but we are using %s." s_version);
      ]

  | PackageNameMismatchOfRelease{ path; from_filename; from_content } ->
      report_error [
        NormalLine(Printf.sprintf "the release config");
        DisplayLine(get_abs_path_string path);
        NormalLine("is inconsistent as package name;");
        NormalLine(Printf.sprintf "its filename says the package name is '%s'," from_filename);
        NormalLine(Printf.sprintf "but the content says '%s'." from_content);
      ]

  | PackageVersionMismatchOfRelease{ path; from_filename; from_content } ->
      let s_version1 = SemanticVersion.to_string from_filename in
      let s_version2 = SemanticVersion.to_string from_content in
      report_error [
        NormalLine(Printf.sprintf "the release config");
        DisplayLine(get_abs_path_string path);
        NormalLine("is inconsistent as package version;");
        NormalLine(Printf.sprintf "its filename says the package version is '%s'," s_version1);
        NormalLine(Printf.sprintf "but the content says '%s'." s_version2);
      ]

  | CannotReadDirectory{ path; message } ->
      report_error [
        NormalLine(Printf.sprintf "cannot read directory '%s':" (get_abs_path_string path));
        DisplayLine(message);
      ]
