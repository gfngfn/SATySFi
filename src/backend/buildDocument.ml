
open MyUtil
open LoggingUtil
open ConfigError
open Types


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = {
  frozen_main      : location EvalVarIDMap.t;
  frozen_store_ref : (syntactic_value StoreIDHashTable.t) ref;
  frozen_store_map : syntactic_value StoreIDMap.t;
  frozen_config    : runtime_config;
}


let freeze_environment (env : environment) : frozen_environment =
  let
    {
      env_main   = valenv;
      env_store  = stenvref;
      env_config = runtime_config;
    } = env
  in
  let stmap =
    StoreIDMap.empty |> StoreIDHashTable.fold (fun stid value stmap ->
      stmap |> StoreIDMap.add stid value
    ) (!stenvref)
  in
  {
    frozen_main      = valenv;
    frozen_store_ref = stenvref;
    frozen_store_map = stmap;
    frozen_config    = runtime_config;
  }


let unfreeze_environment (frenv : frozen_environment) : environment =
  let
    {
      frozen_main = valenv;
      frozen_store_ref = stenvref;
      frozen_store_map = stmap;
      frozen_config    = runtime_config;
    } = frenv
  in
  let stenv = StoreIDHashTable.create 128 in
  stmap |> StoreIDMap.iter (fun stid value -> StoreIDHashTable.add stenv stid value);
  stenvref := stenv;
  {
    env_main   = valenv;
    env_store  = ref stenv;
    env_config = runtime_config;
  }


let transform_pdf (logging_spec : logging_spec) (pdf_config : HandlePdf.config) ~(page_number_limit : int) = function
  | BaseConstant(BCDocument(paper_size, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
      begin
        Logging.start_page_break logging_spec;
        State.start_page_break ();
        match pbstyle with
        | SingleColumn ->
            PageBreak.main pdf_config ~paper_size
              columnhookf pagecontf pagepartsf imvblst

        | MultiColumn(origin_shifts) ->
            PageBreak.main_multicolumn pdf_config ~page_number_limit ~paper_size
              origin_shifts columnhookf columnendhookf pagecontf pagepartsf imvblst
      end

  | value ->
      EvalUtil.report_bug_value "main; not a DocumentValue(...)" value


let transform_text =
  EvalUtil.get_string


let output_pdf (logging_spec : logging_spec) (abspath_out : abs_path) (pdfret : HandlePdf.t) : (unit, config_error) result =
  let open ResultMonad in
  try
    HandlePdf.write_to_file logging_spec abspath_out pdfret;
    return ()
  with
  | _ -> err @@ CannotOutputResult{ path = abspath_out; message = "HandlePdf.write_to_file failed" }


let output_text (abspath_out : abs_path) (data : string) : (unit, config_error) result =
  AbsPathIo.write_file abspath_out data
    |> Result.map_error (fun message -> CannotOutputResult{ path = abspath_out; message })


(* Initialization that should be performed before every cross-reference-solving loop *)
let reset_pdf () =
  ImageInfo.initialize ();
  NamedDest.initialize ();
  ()


let evaluate (logging_spec : logging_spec) (reset : unit -> unit) ~(is_bytecomp_mode : bool) (i : int) (env_freezed : frozen_environment) (ast : abstract_tree) : (syntactic_value, config_error) result =
  let open ResultMonad in
  Logging.start_evaluation logging_spec i;
  reset ();
  let env = unfreeze_environment env_freezed in
  let value =
    if is_bytecomp_mode then
      let (value, _) = Bytecomp.compile_and_exec_0 env ast in
      value
    else
      Evaluator.interpret_0 env ast
  in
  Logging.end_evaluation logging_spec;
  return value


let build_document ~(max_repeats : int) (transform : syntactic_value -> 'a) (reset : unit -> unit) (output : abs_path -> 'a -> (unit, config_error) result) (logging_spec : logging_spec) ~(is_bytecomp_mode : bool) (env : environment) (ast : abstract_tree) (abspath_out : abs_path) (abspath_dump : abs_path) =
  let open ResultMonad in
  let env_freezed = freeze_environment env in
  let rec aux (i : int) =
    CrossRef.reset ();
    let* value = evaluate logging_spec reset ~is_bytecomp_mode i env_freezed ast in
    let document = transform value in
    match CrossRef.judge_termination () with
    | CrossRef.NeedsAnotherTrial ->
        if i >= max_repeats then
          begin
            Logging.achieve_count_max logging_spec;
            return document
          end
        else
          begin
            Logging.needs_another_trial logging_spec;
            aux (i + 1)
          end

    | CrossRef.CanTerminate unresolved_crossrefs ->
        Logging.achieve_fixpoint logging_spec unresolved_crossrefs;
        return document
  in
  let* document = aux 1 in
  let* () = output abspath_out document in
  let* () = CrossRef.write_dump_file abspath_dump in
  Logging.end_output logging_spec abspath_out;
  return ()


let main (output_mode : output_mode) (pdf_config : HandlePdf.config) ~(page_number_limit : int) ~(max_repeats : int) (logging_spec : logging_spec) =
  match output_mode with
  | PdfMode ->
      build_document
        ~max_repeats
        (transform_pdf logging_spec pdf_config ~page_number_limit)
        reset_pdf
        (output_pdf logging_spec)
        logging_spec

  | TextMode(_) ->
      build_document
        ~max_repeats
        transform_text
        Fun.id
        output_text
        logging_spec
