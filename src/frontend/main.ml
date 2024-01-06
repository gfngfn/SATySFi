
open MyUtil
open EnvelopeSystemBase
open Types
open StaticEnv
open ConfigError
open FontError
open TypeError


let version =
  Printf.sprintf "SATySFi version %s alpha"
    (SemanticVersion.to_string Constant.current_language_version)


(* Initialization that should be performed before every cross-reference-solving loop *)
let reset (output_mode : output_mode) =
  let open ResultMonad in
  match output_mode with
  | TextMode(_) ->
      return ()

  | PdfMode ->
      ImageInfo.initialize ();
      NamedDest.initialize ();
      return ()


(* Initialization that should be performed before typechecking *)
let initialize ~(base_dir : abs_path) ~(is_bytecomp_mode : bool) (output_mode : output_mode) (runtime_config : runtime_config) : Typeenv.t * environment =
  FreeID.initialize ();
  BoundID.initialize ();
  EvalVarID.initialize ();
  StoreID.initialize ();
  FontInfo.initialize ~base_dir;
  let (tyenv, env) =
    match output_mode with
    | TextMode(_) ->
        Primitives.make_text_mode_environments runtime_config

    | PdfMode ->
        Primitives.make_pdf_mode_environments ~base_dir runtime_config
  in
  begin
    if is_bytecomp_mode then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env)


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


let output_pdf (pdfret : HandlePdf.t) : unit =
  HandlePdf.write_to_file pdfret


let output_text (abspath_out : abs_path) (data : string) : unit =
  Core.Out_channel.write_all (get_abs_path_string abspath_out) ~data


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


let eval_main ~(is_bytecomp_mode : bool) (output_mode : output_mode) (i : int) (env_freezed : frozen_environment) (ast : abstract_tree) : (syntactic_value, config_error) result =
  let open ResultMonad in
  Logging.start_evaluation i;
  let* () = reset output_mode in
  let env = unfreeze_environment env_freezed in
  let value =
    if is_bytecomp_mode then
      let (value, _) = Bytecomp.compile_and_exec_0 env ast in
      value
    else
      Evaluator.interpret_0 env ast
  in
  Logging.end_evaluation ();
  return value


let eval_document_file (display_config : Logging.config) (pdf_config : HandlePdf.config) ~(page_number_limit : int) ~(is_bytecomp_mode : bool) (output_mode : output_mode) (env : environment) (ast : abstract_tree) (abspath_out : abs_path) (abspath_dump : abs_path) =
  let open ResultMonad in
  let env_freezed = freeze_environment env in
  match output_mode with
  | TextMode(_) ->
      let rec aux (i : int) =
        let* value_str = eval_main ~is_bytecomp_mode output_mode i env_freezed ast in
        let s = EvalUtil.get_string value_str in
        match CrossRef.needs_another_trial abspath_dump with
        | CrossRef.NeedsAnotherTrial ->
            Logging.needs_another_trial ();
            aux (i + 1)

        | CrossRef.CountMax ->
            Logging.achieve_count_max ();
            output_text abspath_out s;
            Logging.end_output display_config abspath_out;
            return ()

        | CrossRef.CanTerminate unresolved_crossrefs ->
            Logging.achieve_fixpoint unresolved_crossrefs;
            output_text abspath_out s;
            Logging.end_output display_config abspath_out;
            return ()
      in
      aux 1

  | PdfMode ->
      let rec aux (i : int) =
        let* value_doc = eval_main ~is_bytecomp_mode output_mode i env_freezed ast in
        match value_doc with
        | BaseConstant(BCDocument(paper_size, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
            Logging.start_page_break ();
            State.start_page_break ();
            let pdf =
              match pbstyle with
              | SingleColumn ->
                  PageBreak.main pdf_config abspath_out ~paper_size
                    columnhookf pagecontf pagepartsf imvblst

              | MultiColumn(origin_shifts) ->
                  PageBreak.main_multicolumn pdf_config ~page_number_limit abspath_out ~paper_size
                    origin_shifts columnhookf columnendhookf pagecontf pagepartsf imvblst
            in
            begin
              match CrossRef.needs_another_trial abspath_dump with
              | CrossRef.NeedsAnotherTrial ->
                  Logging.needs_another_trial ();
                  aux (i + 1)

              | CrossRef.CountMax ->
                  Logging.achieve_count_max ();
                  output_pdf pdf;
                  Logging.end_output display_config abspath_out;
                  return ()

              | CrossRef.CanTerminate unresolved_crossrefs ->
                  Logging.achieve_fixpoint unresolved_crossrefs;
                  output_pdf pdf;
                  Logging.end_output display_config abspath_out;
                  return ()
            end

        | _ ->
            EvalUtil.report_bug_value "main; not a DocumentValue(...)" value_doc
      in
      aux 1


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
  eval_document_file display_config pdf_config ~page_number_limit ~is_bytecomp_mode output_mode env ast_doc abspath_out abspath_dump


type line =
  | NormalLine  of string
  | DisplayLine of string

type error_category =
  | Lexer
  | Parser
  | Typechecker
  | Evaluator
  | Interface
  | System


let show_error_category = function
  | Lexer       -> "Syntax Error at Lexer"
  | Parser      -> "Syntax Error at Parser"
  | Typechecker -> "Type Error"
  | Evaluator   -> "Error during Evaluation"
  | Interface   -> "Error"
  | System      -> "Error"


let report_error (cat : error_category) (lines : line list) =
  print_string (Printf.sprintf "! [%s] " (show_error_category cat));
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


let make_candidates_message (candidates : string list) =
  let quote s = Printf.sprintf "'%s'" s in
  let aux (rev_rest : string list) (last : string) =
    match rev_rest with
    | []     -> quote last
    | _ :: _ -> Printf.sprintf "%s or %s" (String.concat ", " (List.map quote (List.rev rev_rest))) (quote last)
  in
  match List.rev candidates with
  | []               -> None
  | last :: rev_rest -> Some(Printf.sprintf "Did you mean %s?" (aux rev_rest last))


let make_unification_error_message (dispmap : DisplayMap.t) (ue : unification_error) =
  match ue with
  | TypeContradiction(ty1_sub, ty2_sub) ->
      let dispmap =
        dispmap
          |> Display.collect_ids_mono ty1_sub
          |> Display.collect_ids_mono ty2_sub
      in
      let str_ty1_sub = Display.show_mono_type_by_map dispmap ty1_sub in
      let str_ty2_sub = Display.show_mono_type_by_map dispmap ty2_sub in
      [
        NormalLine("Type");
        DisplayLine(str_ty1_sub);
        NormalLine("is not compatible with");
        DisplayLine(Printf.sprintf "%s." str_ty2_sub);
      ]

  | TypeVariableInclusion(fid, ty) ->
      let dispmap = dispmap |> Display.collect_ids_mono ty in
      let (dispmap, str_fid) = dispmap |> DisplayMap.add_free_id fid in
      let str_ty = Display.show_mono_type_by_map dispmap ty in
      [
        NormalLine(Printf.sprintf "Type variable %s occurs in" str_fid);
        DisplayLine(Printf.sprintf "%s." str_ty);
      ]

  | RowContradiction(row1, row2) ->
      let dispmap =
        dispmap
          |> Display.collect_ids_mono_row row1
          |> Display.collect_ids_mono_row row2
      in
      let str_row1 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      let str_row2 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      [
        NormalLine("Row");
        DisplayLine(str_row1);
        NormalLine("is not compatible with");
        DisplayLine(Printf.sprintf "%s." str_row2);
      ]

  | RowVariableInclusion(frid, row) ->
      let labset = FreeRowID.get_label_set frid in
      let (dispmap, str_frid) = dispmap |> DisplayMap.add_free_row_id frid labset in
      let dispmap = dispmap |> Display.collect_ids_mono_row row in
      let str_row = Display.show_mono_row_by_map dispmap row |> Option.value ~default:"" in
      [
        NormalLine(Printf.sprintf "Row variable %s occurs in" str_frid);
        DisplayLine(Printf.sprintf "%s." str_row);
      ]

  | CommandArityMismatch(len1, len2) ->
      [
        NormalLine(Printf.sprintf "The command type has %d type argument(s), but is expected to have %d." len1 len2);
      ]

  | CommandOptionalLabelMismatch(label) ->
      [
        NormalLine(Printf.sprintf "Label '%s' in a command type makes the contradiction." label);
      ]

  | BreaksRowDisjointness(label) ->
      [
        NormalLine(Printf.sprintf "The row must not contain label '%s'." label);
      ]

  | BreaksLabelMembershipByFreeRowVariable(_frid, label, _labset) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ] (* TODO (error): detailed report *)

  | BreaksLabelMembershipByBoundRowVariable(_mbbrid, label) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ] (* TODO (error): detailed report *)

  | BreaksLabelMembershipByEmptyRow(label) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ]

  | InsufficientRowVariableConstraint(_mbbrid, _labset_expected, _labset_actual) ->
      [] (* TODO (error): detailed report *)


let report_parse_error = function
  | CannotProgressParsing(rng) ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
      ]

  | IllegalItemDepth{ range = rng; before; current } ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "illegal item depth %d after %d" before current);
      ]

  | EmptyInputFile(rng) ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("empty input.");
      ]


let report_type_error = function
  | UndefinedVariable(rng, varnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      report_error Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined variable '%s'." varnm);
        ];
        candidates_message_lines;
      ])

  | UndefinedConstructor(rng, constrnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      report_error Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined constructor '%s'." constrnm);
        ];
        candidates_message_lines;
      ])

  | UndefinedTypeName(rng, tynm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type '%s'." tynm);
      ]

  | UndefinedTypeVariable(rng, tyvarnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type variable '%s'." tyvarnm);
      ]

  | UndefinedRowVariable(rng, rowvarnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined row variable '%s'." rowvarnm);
      ]

  | UndefinedKindName(rng, kdnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined kind '%s'." kdnm);
      ]

  | UndefinedModuleName(rng, modnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined module '%s'." modnm);
      ]

  | UndefinedSignatureName(rng, signm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined signature '%s'." signm);
      ]
  | UndefinedMacro(rng, csnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined macro '%s'." csnm);
      ]

  | InvalidNumberOfMacroArguments(rng, macparamtys) ->
      report_error Typechecker (List.append [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid number of macro arguments; types expected on arguments are:");
      ] (macparamtys |> List.map (function
        | LateMacroParameter(ty)  -> DisplayLine(Printf.sprintf "* %s" (Display.show_mono_type ty))
        | EarlyMacroParameter(ty) -> DisplayLine(Printf.sprintf "* ~%s" (Display.show_mono_type ty))
      )))

  | LateMacroArgumentExpected(rng, ty) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an early macro argument is given, but a late argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | EarlyMacroArgumentExpected(rng, ty) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a late macro argument is given, but an early argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | UnknownUnitOfLength(rng, unitnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined unit of length '%s'." unitnm);
      ]

  | InlineCommandInMath(rng) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an inline command is used as a math command.");
      ]

  | MathCommandInInline(rng) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a math command is used as an inline command.");
      ]

  | BreaksValueRestriction(rng) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression breaks the value restriction;");
        NormalLine("it should be a syntactic function.");
      ]

  | MultiplePatternVariable(rng1, rng2, varnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and at %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "pattern variable '%s' is bound more than once." varnm);
      ]

  | LabelUsedMoreThanOnce(rng, label) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is used more than once." label);
      ]

  | InvalidExpressionAsToStaging(rng, stage) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid expression as to stage;");
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | InvalidOccurrenceAsToStaging(rng, varnm, stage) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "invalid occurrence of variable '%s' as to stage;" varnm);
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | ApplicationOfNonFunction(rng, ty) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression has type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]


  | MultiCharacterMathScriptWithoutBrace(rng) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("more than one character is used as a math sub/superscript without braces;");
        NormalLine("use braces for making association explicit.");
      ]

  | IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is expected to have %d type argument(s)," tynm lenexp);
        NormalLine(Printf.sprintf "but it has %d type argument(s) here." lenerr);
      ]

  | TypeUnificationError(((rng1, _) as ty1), ((rng2, _) as ty2), ue) ->
      let dispmap =
        DisplayMap.empty
          |> Display.collect_ids_mono ty1
          |> Display.collect_ids_mono ty2
      in
      let strty1 = Display.show_mono_type_by_map dispmap ty1 in
      let strty2 = Display.show_mono_type_by_map dispmap ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true) ->
            (Printf.sprintf "(cannot report position; '%s', '%s')" (Range.message rng1) (Range.message rng2),
                strty1, strty2, [])

        | (true, false) ->
            (Printf.sprintf "at %s:" strrng2, strty2, strty1, [])

        | (false, true) ->
            (Printf.sprintf "at %s:" strrng1, strty1, strty2, [])

        | (false, false) ->
            (Printf.sprintf "at %s:" strrng1, strty1, strty2,
                [
                  NormalLine("This constraint is required by the expression");
                  NormalLine(Printf.sprintf "at %s." strrng2);
                ])
      in
      let detail = make_unification_error_message dispmap ue in
      report_error Typechecker (List.concat [
        [
          NormalLine(posmsg);
          NormalLine("this expression has type");
          DisplayLine(Printf.sprintf "%s," strtyA);
          NormalLine("but is expected of type");
          DisplayLine(Printf.sprintf "%s." strtyB);
        ];
        detail;
        additional;
      ])

  | RowUnificationError(rng, row1, row2, ue) ->
      let dispmap =
        DisplayMap.empty
          |> Display.collect_ids_mono_row row1
          |> Display.collect_ids_mono_row row2
      in
      let str_row1 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      let str_row2 = Display.show_mono_row_by_map dispmap row2 |> Option.value ~default:"" in
      let detail = make_unification_error_message dispmap ue in
      report_error Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine("the option row is");
          DisplayLine(str_row1);
          NormalLine("and");
          DisplayLine(Printf.sprintf "%s," str_row2);
          NormalLine("at the same time, but these are incompatible.");
        ];
        detail;
      ])

  | TypeParameterBoundMoreThanOnce(rng, tyvarnm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "type variable %s is bound more than once." tyvarnm);
      ]

  | ConflictInSignature(rng, member) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is declared more than once in a signature." member);
      ]

  | NotAStructureSignature(rng, _fsig) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a structure signature (TODO (enhance): detailed report)");
      ]

  | NotAFunctorSignature(rng, _ssig) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a functor signature (TODO (enhance): detailed report)");
      ]

  | MissingRequiredValueName(rng, x, pty) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required value '%s' of type" x);
        DisplayLine(Display.show_poly_type pty);
      ]

  | MissingRequiredMacroName(rng, csnm, pmacty) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required macro '%s' of type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty);
      ]

  | MissingRequiredConstructorName(rng, ctornm, _centry) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required constructor '%s' (TODO (enhance): detailed report)" ctornm);
      ]

  | MissingRequiredTypeName(rng, tynm, arity) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required type '%s' of arity %d" tynm arity);
      ]

  | MissingRequiredModuleName(rng, modnm, _modsig) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required module '%s' (TODO (enhance): detailed report)" modnm);
      ]

  | MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required signature '%s' (TODO (enhance): detailed report)" signm);
      ]

  | NotASubtypeAboutValue(rng, x, pty1, pty2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about value '%s'; type" x);
        DisplayLine(Display.show_poly_type pty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_type pty2);
      ]

  | NotASubtypeAboutValueStage(rng, x, stage1, stage2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about the stage of value '%s';" x);
        DisplayLine(string_of_stage stage1);
        NormalLine("is not consistent with");
        DisplayLine(string_of_stage stage2);
      ]

  | NotASubtypeAboutMacro(rng, csnm, pmacty1, pmacty2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about macro '%s'; type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_macro_type pmacty2);
      ]

  | NotASubtypeAboutConstructor(rng, ctornm, _tyscheme1, _tyscheme2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about constructor '%s' (TODO (enhance): detailed report)" ctornm);
      ]

  | NotASubtypeAboutType(rng, tynm, tentry1, tentry2) ->
      Format.printf "1: %a,@ 2: %a@," pp_type_entry tentry1 pp_type_entry tentry2; (* TODO: remove this *)
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about type '%s' (TODO (enhance): detailed report)" tynm);
      ]

  | NotASubtypeSignature(rng, _modsig1, _modsig2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a subtype signature (TODO (enhance): detailed report)");
      ]

  | UnexpectedOptionalLabel(rng, label, ty_cmd) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "unexpected application of label '%s';" label);
        NormalLine(Printf.sprintf "the command used here has type");
        DisplayLine(Display.show_mono_type ty_cmd);
      ]

  | InvalidArityOfCommandApplication(rng, arity_expected, arity_actual) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "this command expects %d argument(s)," arity_expected);
        NormalLine(Printf.sprintf "but is applied to %d argument(s) here." arity_actual);
      ]

  | CannotRestrictTransparentType(rng, tynm) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot restrict transparent type '%s'." tynm);
      ]

  | KindContradiction(rng, tynm, kd_expected, kd_actual) ->
      let Kind(bkds_expected) = kd_expected in
      let Kind(bkds_actual) = kd_actual in
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "type '%s' expects %d type argument(s)," tynm (List.length bkds_expected));
        NormalLine(Printf.sprintf "but is applied to %d type argument(s)." (List.length bkds_actual));
      ]

  | CyclicSynonymTypeDefinition(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      let lines =
        pairs |> List.map (fun (tynm, data) ->
          let rng = data.SynonymDependencyGraph.position in
          DisplayLine(Printf.sprintf "- '%s' (%s)" tynm (Range.to_string rng))
        )
      in
      report_error Typechecker
        (NormalLine("the following synonym types are cyclic:") :: lines)

  | MultipleSynonymTypeDefinition(tynm, rng1, rng2) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "synonym type '%s' is defined more than once." tynm);
      ]

  | ValueAttributeError(ValueAttribute.Unexpected(rng)) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("unexpected value attributes.");
      ]

  | TestMustBeStage1NonRec(rng) ->
      report_error Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("tests must be stage-1 non-recursive bindings.");
      ]


(*
let report_document_attribute_error : DocumentAttribute.error -> unit = function
  | NoConfigArgument(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("no config argument is given.");
      ]

  | DuplicateConfigAttribute(rng1, rng2) ->
      report_error Interface [
        NormalLine("More than one attribute defines the config:");
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng1));
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng2));
      ]

  | NotAVersionRequirement(rng, s) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a version requirement: '%s'" s);
      ]

  | NotAPackageDependency(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a package dependency description.");
      ]

  | NotARegistry(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a registry description.");
      ]

  | NotARegistryRemote(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a registry remote description.");
      ]

  | LabelNotFound{ record_range; label } ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string record_range));
        NormalLine(Printf.sprintf "this record does not have label '%s'." label);
      ]

  | DuplicateLabel{ record_range; label } ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string record_range));
        NormalLine(Printf.sprintf "this record has more than one value for label '%s'." label);
      ]

  | NotAStringLiteral(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a string literal.");
      ]

  | NotAListLiteral(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a list literal.");
      ]

  | DuplicateRegistryLocalName{ list_range; registry_local_name } ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string list_range));
        NormalLine(Printf.sprintf "this list has more than one registy named '%s'." registry_local_name);
      ]
*)


let module_name_chain_to_string (((_, modnm0), modidents) : module_name_chain) =
  let modidents = modidents |> List.map (fun (_, modnm) -> modnm) in
  let modidents = modnm0 :: modidents in
  modidents |> String.concat "."


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


let report_config_error (display_config : Logging.config) : config_error -> unit = function
  | UnexpectedExtension(ext) ->
      report_error Interface [
        NormalLine(Printf.sprintf "unexpected file extension '%s'." ext);
      ]

  | NotALibraryFile(abspath) ->
      report_error Typechecker [
        NormalLine("the following file is expected to be a library file, but is not:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | NotADocumentFile(abspath_in, ty) ->
      let fname = Logging.show_path display_config abspath_in in
      report_error Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a document file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAStringFile(abspath_in, ty) ->
      let fname = Logging.show_path display_config abspath_in in
      report_error Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a file for generating text; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | FileModuleNotFound(rng, modnm) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot find a source file that defines module '%s'." modnm);
      ]

  | FileModuleNameConflict(modnm, abspath1, abspath2) ->
      report_error Interface [
        NormalLine(Printf.sprintf "more than one file defines module '%s':" modnm);
        DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath1));
        DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath2));
      ]

  | NoMainModule(modnm) ->
      report_error Interface [
        NormalLine(Printf.sprintf "no main module '%s'." modnm);
      ]

  | UnknownPackageDependency(rng, modnm) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "dependency on unknown package '%s'" modnm);
      ]

  | TypeError(tyerr) ->
      report_type_error tyerr

  | CyclicFileDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      report_error Interface (
        (NormalLine("cyclic dependency detected:")) ::
          (pairs |> List.map (fun (abspath, _) -> DisplayLine(get_abs_path_string abspath)))
      )

  | CannotReadFileOwingToSystem(msg) ->
      report_error Interface [
        NormalLine("cannot read file:");
        DisplayLine(msg);
      ]

  | LibraryContainsWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "file '%s' is not a library; it has a return value." fname);
      ]

  | DocumentLacksWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "file '%s' is not a document; it lacks a return value." fname);
      ]

  | CannotUseHeaderUse((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s' here; use 'use %s of ...' instead." modnm modnm);
      ]

  | CannotUseHeaderUseOf((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s of ...' here; use 'use %s' instead." modnm modnm);
      ]

  | FailedToParse(e) ->
      report_parse_error e

  | MainModuleNameMismatch{ expected; got } ->
      report_error Interface [
        NormalLine(Printf.sprintf "main module name mismatch; expected '%s' but got '%s'." expected got);
      ]

  | EnvelopeNameConflict(envelope_name) ->
      report_error Interface [
        NormalLine(Printf.sprintf "envelope name conflict: '%s'" envelope_name);
      ]

  | DependencyOnUnknownEnvelope{ depending; depended } ->
      report_error Interface [
        NormalLine(Printf.sprintf "unknown depended envelope '%s' of '%s'." depended depending);
      ]

  | CyclicEnvelopeDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      let lines =
        pairs |> List.map (fun (modnm, _envelope) ->
          DisplayLine(Printf.sprintf "- '%s'" modnm)
        )
      in
      report_error Interface
        (NormalLine("the following envelopes are cyclic:") :: lines)

  | LibraryRootConfigNotFoundIn(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface (List.concat [
        [ NormalLine(Printf.sprintf "cannot find a library root config '%s'. candidates:" (get_lib_path_string libpath)) ];
        lines;
      ])

  | LocalFileNotFound{ relative; candidates } ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface
        (NormalLine(Printf.sprintf "cannot find local file '%s'. candidates:" relative) :: lines)

  | DepsConfigNotFound(abspath_deps_config) ->
      report_error Interface [
        NormalLine("cannot find a deps config at:");
        DisplayLine(get_abs_path_string abspath_deps_config);
      ]

  | DepsConfigError(abspath_deps_config, e) ->
      report_error Interface (List.append [
        NormalLine("failed to load a deps config:");
        DisplayLine(get_abs_path_string abspath_deps_config);
      ] (make_yaml_error_lines e))

  | EnvelopeConfigNotFound(abspath_envelope_config) ->
      report_error Interface [
        NormalLine("cannot find an envelope config at:");
        DisplayLine(get_abs_path_string abspath_envelope_config);
      ]

  | EnvelopeConfigError(abspath_envelope_config, e) ->
      report_error Interface (List.append [
        NormalLine("failed to load an envelope config:");
        DisplayLine(get_abs_path_string abspath_envelope_config);
      ] (make_yaml_error_lines e))

  | DependedEnvelopeNotFound(envelope_name) ->
      report_error Interface [
        NormalLine(Printf.sprintf "unknown depended envelope '%s'" envelope_name);
      ]


let report_font_error (display_config : Logging.config) = function
  | FailedToReadFont(abspath, msg) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load font file '%s';" fname);
        DisplayLine(msg);
      ]

  | FailedToDecodeFont(abspath, e) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot decode font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Decode.Error.pp e);
      ]

  | FailedToMakeSubset(abspath, e) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot make a subset of font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Subset.Error.pp e);
      ]

  | NotASingleFont(abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "the font file '%s' is not a single font file." fname);
      ]

  | NotAFontCollectionElement(abspath, index) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "the font file '%s' (used with index %d) is not a collection." fname index);
      ]

  | CannotFindLibraryFileAsToFont(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | NoMathTable(abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'MATH' table." fname);
      ]

  | PostscriptNameNotFound(abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a PostScript name." fname);
      ]

  | CannotFindUnicodeCmap(abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'cmap' subtable for Unicode code points." fname);
      ]

  | CollectionIndexOutOfBounds{ path; index; num_elements } ->
      let fname = Logging.show_path display_config path in
      report_error Interface [
        NormalLine(Printf.sprintf "%d: index out of bounds;" index);
        NormalLine(Printf.sprintf "font file '%s' has %d elements." fname num_elements);
      ]


let error_log_environment (display_config : Logging.config) (suspended : unit -> ('a, config_error) result) : ('a, config_error) result =
  try
    suspended ()
  with
  | RemainsToBeImplemented(msg) ->
      report_error Interface [
        NormalLine("remains to be supported:");
        DisplayLine(msg);
      ]

  | LoadHyph.InvalidPatternElement(rng) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid string for hyphenation pattern.");
      ]

  | HorzBox.FontIsNotSet{ raw; normalized } ->
      report_error Interface [
        NormalLine("font is not set;");
        DisplayLine(Printf.sprintf "- raw script: %s" (CharBasis.show_script raw));
        DisplayLine(Printf.sprintf "- normalized script: %s" (CharBasis.show_script normalized));
      ]

  | HorzBox.MathFontIsNotSet ->
      report_error Interface [
        NormalLine("math font is not set.");
      ]

  | FontInfo.FontInfoError(e) ->
      report_font_error display_config e

  | ImageHashTable.CannotLoadPdf(msg, abspath, pageno) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load PDF file '%s' page #%d;" fname pageno);
        DisplayLine(msg);
      ]

  | ImageHashTable.CannotLoadImage(msg, abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine(msg);
      ]

  | ImageHashTable.ImageOfWrongFileType(abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This file format is not supported.");
      ]

  | ImageHashTable.UnsupportedColorModel(_, abspath) ->
      let fname = Logging.show_path display_config abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This color model is not supported.");
      ]

  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(s);
      ]

  | MyYojsonUtil.SyntaxError(fname, msg) ->
      report_error System [
        NormalLine(Printf.sprintf "in '%s':" fname);
        NormalLine(msg);
      ]

  | MyYojsonUtil.MultipleDesignation(rng, key) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "multiple designation for key '%s'." key);
      ]

  | Yojson.SafePos.Util.Type_error(msg, (pos, _)) ->
      let rng = MyYojsonUtil.make_range pos in
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(msg);
      ]

  | MyYojsonUtil.MissingRequiredKey(rng, key) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required key '%s'." key);
      ]

  | Evaluator.EvalError(s)
  | Vm.ExecError(s)
      -> report_error Evaluator [ NormalLine(s); ]

  | State.NotDuringPageBreak ->
      report_error Evaluator [
        NormalLine("a primitive as to PDF annotation was called before page breaking starts.");
      ]

  | PageBreak.PageNumberLimitExceeded(m) ->
      report_error Evaluator [
        NormalLine(Printf.sprintf "page number limit (= %d) exceeded." m);
        NormalLine(Printf.sprintf "If you really want to output more than %d pages, use '--page-number-limit'." m);
      ]

  | Sys_error(s) ->
      report_error System [ NormalLine(s); ]


let get_candidate_file_extensions (output_mode : output_mode) =
  match output_mode with
  | PdfMode           -> [ ".satyh"; ".satyg" ]
  | TextMode(formats) -> List.append (formats |> List.map (fun s -> ".satyh-" ^ s)) [ ".satyg" ]


let get_input_kind_from_extension (abspath_doc : abs_path) =
  match Filename.extension (get_abs_path_string abspath_doc) with
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

  (* Typechecks every depended envelope: *)
  let* (genv, configenv, libacc) =
    sorted_envelopes |> foldM (fun (genv, configenv, libacc) (envelope_name, (config, envelope)) ->
      let* used_as_map = make_used_as_map_for_checking_dependency deps_config envelope_name in
      let* (ssig, libs) =
        EnvelopeChecker.main display_config typecheck_config tyenv_prim genv ~used_as_map envelope
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


let get_job_directory (abspath : abs_path) : string =
  Filename.dirname (get_abs_path_string abspath)


let build_package
    ~(fpath_in : string)
    ~(fpath_deps : string)
    ~(fpath_base : string)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
=
  let open ResultMonad in
  let display_config = Logging.{ show_full_path } in
  error_log_environment display_config (fun () ->
    let absdir_current = Sys.getcwd () in

    let abspath_envelope_config = make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_deps_config = make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let absdir_base = make_absolute_if_relative ~origin:absdir_current fpath_base in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let typecheck_config =
      {
        is_text_mode =
          match output_mode with
          | PdfMode     -> false
          | TextMode(_) -> true
      }
    in
    let extensions = get_candidate_file_extensions output_mode in
    let job_directory = get_job_directory abspath_envelope_config in
    let runtime_config = { job_directory } in

    (* Gets the initial type environment, which consists only of primitives: *)
    let (tyenv_prim, _env) =
      initialize ~base_dir:absdir_base ~is_bytecomp_mode:false output_mode runtime_config
    in

    (* Loads the deps config: *)
    Logging.deps_config_file display_config abspath_deps_config;
    let* deps_config = DepsConfig.load abspath_deps_config in

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
    let used_as_map = make_used_as_map deps_config.explicit_dependencies in
    let* (_ssig, _libs) =
      EnvelopeChecker.main display_config typecheck_config tyenv_prim genv ~used_as_map envelope
    in
    return ()

  ) |> function
  | Ok(())   -> ()
  | Error(e) -> report_config_error display_config e; exit 1


let build_document
    ~(fpath_in : string)
    ~(fpath_out : string)
    ~(fpath_dump : string)
    ~(fpath_deps : string)
    ~(fpath_base : string)
    ~(text_mode_formats_str_opt : string option)
    ~(page_number_limit : int)
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
  let display_config = Logging.{ show_full_path } in
  error_log_environment display_config (fun () ->
    let absdir_current = Sys.getcwd () in

    let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_out = make_absolute_if_relative ~origin:absdir_current fpath_out in
    let abspath_dump = make_absolute_if_relative ~origin:absdir_current fpath_dump in
    let abspath_deps_config = make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let absdir_base = make_absolute_if_relative ~origin:absdir_current fpath_base in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let typecheck_config =
      {
        is_text_mode =
          match output_mode with
          | PdfMode     -> false
          | TextMode(_) -> true
      }
    in
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
      initialize ~base_dir:absdir_base ~is_bytecomp_mode output_mode runtime_config
    in

    (* Loads the deps config: *)
    Logging.deps_config_file display_config abspath_deps_config;
    let* deps_config = DepsConfig.load abspath_deps_config in

    Logging.target_file display_config abspath_out;

    (* Initializes the dump file: *)
    let dump_file_exists = CrossRef.initialize abspath_dump in
    Logging.dump_file display_config ~already_exists:dump_file_exists abspath_dump;

    (* Typechecks each depended envelope in the topological order: *)
    let* (genv, configenv, libs) =
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
      OpenFileDependencyResolver.main display_config ~extensions input_kind configenv abspath_in
    in

    (* Typechecking and elaboration: *)
    let used_as_map = make_used_as_map deps_config.explicit_dependencies in
    let* (libs_local, ast_doc) =
      EnvelopeChecker.main_document
        display_config typecheck_config tyenv_prim genv ~used_as_map sorted_locals (abspath_in, utdoc)
    in
    let libs = List.append libs libs_local in

    (* Evaluation: *)
    if type_check_only then
      return ()
    else
      preprocess_and_evaluate
        display_config
        pdf_config
        ~page_number_limit
        ~is_bytecomp_mode
        output_mode
        ~run_tests:false
        env libs ast_doc abspath_in abspath_out abspath_dump

  ) |> function
  | Ok(())   -> ()
  | Error(e) -> report_config_error display_config e



let test_package
    ~(fpath_in : string)
    ~(fpath_deps : string)
    ~(fpath_base : string)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
=
  let open ResultMonad in
  let display_config = Logging.{ show_full_path } in
  error_log_environment display_config (fun () ->
    let absdir_current = Sys.getcwd () in

    let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
    let abspath_deps_config = make_absolute_if_relative ~origin:absdir_current fpath_deps in
    let absdir_base = make_absolute_if_relative ~origin:absdir_current fpath_base in
    let output_mode = make_output_mode text_mode_formats_str_opt in

    let typecheck_config =
      {
        is_text_mode =
          match output_mode with
          | PdfMode     -> false
          | TextMode(_) -> true
      }
    in
    let extensions = get_candidate_file_extensions output_mode in
    let job_directory = get_job_directory abspath_in in
    let runtime_config = { job_directory } in

    (* Gets the initial type environment, which consists only of pritmives: *)
    let (tyenv_prim, env) =
      initialize ~base_dir:absdir_base ~is_bytecomp_mode:false output_mode runtime_config
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
    let* (genv, _configenv, _libs_dep) =
      check_depended_envelopes
        display_config
        typecheck_config
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
    let* (_ssig, libs) =
      EnvelopeChecker.main display_config typecheck_config tyenv_prim genv ~used_as_map package
    in

    (* Runs tests: *)
    let (env, codebinds) = preprocess_bindings display_config ~run_tests:true env libs in
    let _env = evaluate_bindings display_config ~run_tests:true env codebinds in
(*
      | DocumentTestInput{
          kind = input_kind;
          deps = abspath_deps_config;
        } ->
          Logging.deps_config_file display_config abspath_deps_config;
          let deps_config = load_deps_config abspath_deps_config in

          let (genv, configenv, libs) =
            check_depended_envelopes display_config typecheck_config ~use_test_only_envelope:true (* ~library_root *) ~extensions tyenv_prim deps_config
          in

          (* Resolve dependency of the document and the local source files: *)
          let (sorted_locals, utdoc) =
            match OpenFileDependencyResolver.main display_config ~extensions input_kind configenv abspath_in with
            | Ok(pair) -> pair
            | Error(e) -> raise (ConfigError(e))
          in

          (* Typechecking and elaboration: *)
          let (libs_local, _ast_doc) =
            match EnvelopeChecker.main_document display_config typecheck_config tyenv_prim genv sorted_locals (abspath_in, utdoc) with
            | Ok(pair) -> pair
            | Error(e) -> raise (ConfigError(e))
          in
          let libs = List.append libs libs_local in
          let (env, codebinds) = preprocess_bindings display_config ~run_tests:true env libs in
          let _env = evaluate_bindings display_config ~run_tests:true env codebinds in
          ()
*)
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
      report_config_error display_config e
