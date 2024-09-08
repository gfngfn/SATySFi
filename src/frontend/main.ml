
open MyUtil
open EnvelopeSystemBase
open ErrorReporting
open Types
open StaticEnv
open ConfigError


let version =
  Printf.sprintf "SATySFi version %s"
    (SemanticVersion.to_string Constant.current_language_version)


(* Initialization that should be performed before typechecking *)
let initialize ~(is_bytecomp_mode : bool) (output_mode : output_mode) (runtime_config : runtime_config) : Typeenv.t * environment =
  FreeID.initialize ();
  BoundID.initialize ();
  EvalVarID.initialize ();
  StoreID.initialize ();
  FontInfo.initialize ();
  let (tyenv, env) =
    match output_mode with
    | TextMode(_) ->
        Primitives.make_text_mode_environments runtime_config

    | PdfMode ->
        Primitives.make_pdf_mode_environments runtime_config
  in
  begin
    if is_bytecomp_mode then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env)


let eval_library_file (display_config : Logging.config) ~(is_bytecomp_mode : bool) ~(run_tests : bool) (env : environment) (abspath : abs_path) (binds : binding list) : environment =
  Logging.begin_to_eval_file display_config abspath;
  if is_bytecomp_mode then
    failwith "TODO: eval_libary_file, Bytecomp"
(*
    let (value, _) = Bytecomp.compile_and_exec_0 env ast in
    add_to_environment env evid (ref value)
*)
  else
    let (env, _) = Evaluator.interpret_bindings_0 ~run_tests env binds in
    env


(* Performs preprecessing. the evaluation is run by the naive interpreter
   regardless of whether `--bytecomp` was specified. *)
let preprocess_bindings (display_config : Logging.config) ~(run_tests : bool) (env : environment) (libs : (abs_path * binding list) list) : environment * (abs_path * code_rec_or_nonrec list) list =
  let (env, codebindacc) =
    libs |> List.fold_left (fun (env, codebindacc) (abspath, binds) ->
      Logging.begin_to_preprocess_file display_config abspath;
      let (env, cd_rec_or_nonrecs) = Evaluator.interpret_bindings_0 ~run_tests env binds in
      (env, Alist.extend codebindacc (abspath, cd_rec_or_nonrecs))
    ) (env, Alist.empty)
  in
  let codebinds = Alist.to_list codebindacc in
  (env, codebinds)


(* Performs evaluation and returns the resulting environment. *)
let evaluate_bindings (display_config : Logging.config) ~(is_bytecomp_mode : bool) ~(run_tests : bool) (env : environment) (codebinds : (abs_path * code_rec_or_nonrec list) list) : environment =
  codebinds |> List.fold_left (fun env (abspath, cd_rec_or_nonrecs) ->
    let binds =
      cd_rec_or_nonrecs |> List.map (fun cd_rec_or_nonrec ->
        Bind(Stage0, unlift_rec_or_nonrec cd_rec_or_nonrec)
      )
    in
    eval_library_file display_config ~is_bytecomp_mode ~run_tests env abspath binds
  ) env


let preprocess_and_evaluate (display_config : Logging.config) (pdf_config : HandlePdf.config) ~(page_number_limit : int) ~(is_bytecomp_mode : bool) (output_mode : output_mode) ~(run_tests : bool) (env : environment) (libs : (abs_path * binding list) list) (ast_doc : abstract_tree) (_abspath_in : abs_path) (abspath_out : abs_path) (abspath_dump : abs_path) =
  (* Performs preprocessing: *)
  let (env, codebinds) = preprocess_bindings display_config ~run_tests env libs in
  let code_doc = Evaluator.interpret_1 env ast_doc in

  (* Performs evaluation: *)
  let env = evaluate_bindings display_config ~is_bytecomp_mode ~run_tests env codebinds in
  let ast_doc = unlift_code code_doc in
  BuildDocument.main output_mode pdf_config ~page_number_limit display_config ~is_bytecomp_mode env ast_doc abspath_out abspath_dump


let get_candidate_file_extensions (output_mode : output_mode) =
  match output_mode with
  | PdfMode           -> [ ".satyh"; ".satyg" ]
  | TextMode(formats) -> List.append (formats |> List.map (fun s -> ".satyh-" ^ s)) [ ".satyg" ]


let get_input_kind_from_extension (abspath_doc : abs_path) =
  match Filename.extension (AbsPath.to_string abspath_doc) with
  | ".saty" -> Ok(InputSatysfi)
  | ".md"   -> Ok(InputMarkdown)
  | ext     -> Error(UnexpectedExtension(ext))


let make_used_as_map (envelope_dependencies : envelope_dependency list) : envelope_name ModuleNameMap.t =
  envelope_dependencies |> List.fold_left (fun used_as_map envelope_dependency ->
    let { dependency_name; dependency_used_as } = envelope_dependency in
    used_as_map |> ModuleNameMap.add dependency_used_as dependency_name
  ) ModuleNameMap.empty


let make_used_as_map_for_checking_dependency (deps_config : DepsConfig.t) (envelope_name_dep : envelope_name) =
  let open ResultMonad in
  let opt =
    deps_config.envelopes |> List.find_map (fun envelope_spec ->
      let { envelope_name; envelope_dependencies; _ } = envelope_spec in
      if String.equal envelope_name envelope_name_dep then
        Some(make_used_as_map envelope_dependencies)
      else
        None
    )
  in
  match opt with
  | None              -> err @@ DependedEnvelopeNotFound(envelope_name_dep)
  | Some(used_as_map) -> return used_as_map


let check_depended_envelopes (display_config : Logging.config) (typecheck_config : typecheck_config) ~(use_test_only_envelope : bool) ~(extensions : string list) (tyenv_prim : Typeenv.t) (deps_config : DepsConfig.t) =
  let open ResultMonad in
  (* Resolves dependency among envelopes: *)
  let* sorted_envelopes =
    ClosedEnvelopeDependencyResolver.main display_config ~use_test_only_envelope ~extensions deps_config
  in

  (* Typechecks every depended envelope (Note: We must ignore `#[test-only]` in dependencies): *)
  let* (genv, configenv, libacc) =
    sorted_envelopes |> foldM (fun (genv, configenv, libacc) (envelope_name, (config, envelope)) ->
      let* used_as_map = make_used_as_map_for_checking_dependency deps_config envelope_name in
      let* (ssig, libs) =
        EnvelopeChecker.main ~testing:false display_config typecheck_config tyenv_prim genv ~used_as_map envelope
      in
      let genv = genv |> GlobalTypeenv.add (EnvelopeName.EN(envelope_name)) ssig in
      let configenv = configenv |> GlobalTypeenv.add (EnvelopeName.EN(envelope_name)) config in
      let libacc = Alist.append libacc libs in
      return (genv, configenv, libacc)
    ) (GlobalTypeenv.empty, GlobalTypeenv.empty, Alist.empty)
  in
  return (genv, configenv, Alist.to_list libacc)


let make_output_mode text_mode_formats_str_opt =
  match text_mode_formats_str_opt with
  | None    -> PdfMode
  | Some(s) -> TextMode(String.split_on_char ',' s)


let make_display_config ~(show_full_path : bool) ~current_dir:(absdir_current : abs_path) =
  let path_display_setting =
    if show_full_path then
      Logging.FullPath
    else
      Logging.RelativeToCwd(absdir_current)
  in
  Logging.{ path_display_setting }


(* TODO: discard `job_directory` *)
let get_job_directory (abspath : abs_path) : string =
  AbsPath.to_string (AbsPath.dirname abspath)


let build_package
    ~(fpath_in : string)
    ~(fpath_deps : string)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
=
  let open ResultMonad in
  let absdir_current = AbsPathIo.getcwd () in
  let display_config = make_display_config ~show_full_path ~current_dir:absdir_current in
  error_log_environment display_config (fun () ->
    let abspath_envelope_config = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_deps_config = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let is_text_mode =
      match output_mode with
      | PdfMode     -> false
      | TextMode(_) -> true
    in
    let typecheck_config = { testing = false; is_text_mode } in
    let extensions = get_candidate_file_extensions output_mode in
    let job_directory = get_job_directory abspath_envelope_config in
    let runtime_config = { job_directory } in

    (* Gets the initial type environment, which consists only of primitives: *)
    let (tyenv_prim, _env) =
      initialize ~is_bytecomp_mode:false output_mode runtime_config
    in

    (* Loads the deps config: *)
    Logging.deps_config_file display_config abspath_deps_config;
    let* deps_config = DepsConfig.load abspath_deps_config in
    let used_as_map = make_used_as_map deps_config.explicit_dependencies in

    (* Parses the main envelope: *)
    let* (_config, envelope) =
      EnvelopeReader.main display_config ~use_test_files:false ~extensions
        ~envelope_config:abspath_envelope_config
    in

    (* Typechecks each depended envelope in the topological order: *)
    let* (genv, _configenv, _libs_dep) =
      check_depended_envelopes
        display_config typecheck_config
        ~use_test_only_envelope:false ~extensions
        tyenv_prim deps_config
    in

    (* Typechecks the main envelope: *)
    let* (_ssig, _libs_target) =
      EnvelopeChecker.main
        ~testing:false
        display_config typecheck_config tyenv_prim genv ~used_as_map envelope
    in
    return ()

  ) |> function
  | Ok(())   -> ()
  | Error(e) -> report_and_exit (make_config_error_message display_config e)


let build_document
    ~(fpath_in : string)
    ~(fpath_out : string)
    ~(fpath_dump : string)
    ~(fpath_deps : string)
    ~(text_mode_formats_str_opt : string option)
    ~(page_number_limit : int)
    ~(max_repeats : int)
    ~(show_full_path : bool)
    ~(debug_show_bbox : bool)
    ~(debug_show_space : bool)
    ~(debug_show_block_bbox : bool)
    ~(debug_show_block_space : bool)
    ~(debug_show_overfull : bool)
    ~(type_check_only : bool)
    ~bytecomp:(is_bytecomp_mode : bool)
=
let open ResultMonad in
  let absdir_current = AbsPathIo.getcwd () in
  let display_config = make_display_config ~show_full_path ~current_dir:absdir_current in
  error_log_environment display_config (fun () ->
    let abspath_in = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_out = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_out in
    let abspath_dump = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_dump in
    let abspath_deps_config = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let is_text_mode =
      match output_mode with
      | PdfMode     -> false
      | TextMode(_) -> true
    in
    let typecheck_config = { testing = false; is_text_mode } in
    let pdf_config =
      HandlePdf.{
        debug_show_bbox;
        debug_show_space;
        debug_show_block_bbox;
        debug_show_block_space;
        debug_show_overfull;
      }
    in
    let job_directory = get_job_directory abspath_in in
    let runtime_config = { job_directory } in
    let* input_kind = get_input_kind_from_extension abspath_in in
    let extensions = get_candidate_file_extensions output_mode in

    (* Gets the initial type environment, which consists only of primitives: *)
    let (tyenv_prim, env) =
      initialize ~is_bytecomp_mode output_mode runtime_config
    in

    (* Loads the deps config: *)
    Logging.deps_config_file display_config abspath_deps_config;
    let* deps_config = DepsConfig.load abspath_deps_config in
    let used_as_map = make_used_as_map deps_config.explicit_dependencies in

    Logging.target_file display_config abspath_out;

    (* Initializes the dump file: *)
    let* dump_file_exists = CrossRef.initialize abspath_dump in
    Logging.dump_file display_config ~already_exists:dump_file_exists abspath_dump;

    (* Typechecks each depended envelope in the topological order: *)
    let* (genv, configenv, libs_dep) =
      check_depended_envelopes
        display_config
        typecheck_config
        ~use_test_only_envelope:false
        ~extensions
        tyenv_prim
        deps_config
    in

    (* Resolve dependency of the document and the local source files: *)
    let* (sorted_locals, utdoc) =
      OpenFileDependencyResolver.main display_config ~extensions input_kind configenv ~used_as_map abspath_in
    in

    (* Typechecking and elaboration: *)
    let* (libs_local, ast_doc) =
      EnvelopeChecker.main_document
        ~testing:false
        display_config typecheck_config tyenv_prim genv ~used_as_map sorted_locals (abspath_in, utdoc)
    in

    (* Evaluation: *)
    if type_check_only then
      return ()
    else
      let libs = List.append libs_dep libs_local in
      preprocess_and_evaluate
        display_config
        pdf_config
        ~page_number_limit
        ~max_repeats
        ~is_bytecomp_mode
        output_mode
        ~run_tests:false
        env libs ast_doc abspath_in abspath_out abspath_dump

  ) |> function
  | Ok(())   -> ()
  | Error(e) -> report_and_exit (make_config_error_message display_config e)


let test_package
    ~(fpath_in : string)
    ~(fpath_deps : string)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
=
  let open ResultMonad in
  let absdir_current = AbsPathIo.getcwd () in
  let display_config = make_display_config ~show_full_path ~current_dir:absdir_current in
  error_log_environment display_config (fun () ->
    let abspath_in = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_deps_config = AbsPath.make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let is_text_mode =
      match output_mode with
      | PdfMode     -> false
      | TextMode(_) -> true
    in
    let typecheck_config_deps = { testing = false; is_text_mode } in (* Ignores `#[test-only]`. *)
    let typecheck_config_main = { testing = true; is_text_mode } in (* Uses `#[test-only]`. *)
    let extensions = get_candidate_file_extensions output_mode in
    let job_directory = get_job_directory abspath_in in
    let runtime_config = { job_directory } in

    (* Gets the initial type environment, which consists only of pritmives: *)
    let (tyenv_prim, env) =
      initialize ~is_bytecomp_mode:false output_mode runtime_config
    in

    (* Loads the deps config: *)
    Logging.deps_config_file display_config abspath_deps_config;
    let* deps_config = DepsConfig.load abspath_deps_config in

    (* Parses the main envelope: *)
    let* (_config, package) =
      EnvelopeReader.main display_config ~use_test_files:true ~extensions
        ~envelope_config:abspath_in
    in

    (* Typechecks each depended envelope in the topological order: *)
    let* (genv, _configenv, libs_dep) =
      check_depended_envelopes
        display_config
        typecheck_config_deps
        ~use_test_only_envelope:true
        ~extensions
        tyenv_prim
        deps_config
    in

    (* Typechecks the main envelope: *)
    let used_as_map =
      let { explicit_dependencies; explicit_test_dependencies; _ } = deps_config in
      make_used_as_map
        (List.append explicit_dependencies explicit_test_dependencies)
    in
    let* (_ssig, libs_target) =
      EnvelopeChecker.main
        ~testing:true
        display_config typecheck_config_main tyenv_prim genv ~used_as_map package
    in

    (* Runs tests (Note: we do not run the tests in dependencies): *)
    let (env, codebinds_dep) = preprocess_bindings display_config ~run_tests:false env libs_dep in
    let (env, codebinds_target) = preprocess_bindings display_config ~run_tests:true env libs_target in
    let env = evaluate_bindings display_config ~run_tests:false ~is_bytecomp_mode:false env codebinds_dep in
    let _env = evaluate_bindings display_config ~run_tests:true ~is_bytecomp_mode:false env codebinds_target in

    let test_results = State.get_all_test_results () in
    let failure_found =
      test_results |> List.fold_left (fun failure_found test_result ->
        match test_result with
        | State.Pass{ test_name }          -> Logging.report_passed_test ~test_name; failure_found
        | State.Fail{ test_name; message } -> Logging.report_failed_test ~test_name ~message; true
      ) false
    in
    return failure_found

  ) |> function
  | Ok(failure_found) ->
      if failure_found then begin
        Logging.some_test_failed ();
        exit 1
      end else begin
        Logging.all_tests_passed ();
        ()
      end

  | Error(e) ->
      report_and_exit (make_config_error_message display_config e)
