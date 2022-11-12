
open MyUtil
open Types
open StaticEnv
open PackageSystemBase
open ConfigError
open FontError
open TypeError


exception NoLibraryRootDesignation
exception ShouldSpecifyOutputFile
exception ConfigError of config_error


(* Initialization that should be performed before every cross-reference-solving loop *)
let reset () =
  let open ResultMonad in
  if OptionState.is_text_mode () then
    return ()
  else begin
    ImageInfo.initialize ();
    NamedDest.initialize ();
    return ()
  end


(* Initialization that should be performed before typechecking *)
let initialize () : Typeenv.t * environment =
  FreeID.initialize ();
  BoundID.initialize ();
  EvalVarID.initialize ();
  StoreID.initialize ();
  FontInfo.initialize ();
  let res =
    if OptionState.is_text_mode () then
      Primitives.make_text_mode_environments ()
    else
      Primitives.make_pdf_mode_environments ()
  in
  let (tyenv, env) =
    match res with
    | Ok(pair) -> pair
    | Error(e) -> raise (ConfigError(e))
  in
  begin
    if OptionState.is_bytecomp_mode () then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env)


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref * syntactic_value StoreIDMap.t


let freeze_environment (env : environment) : frozen_environment =
  let (valenv, stenvref) = env in
  let stmap =
    StoreIDMap.empty |> StoreIDHashTable.fold (fun stid value stmap ->
      stmap |> StoreIDMap.add stid value
    ) (!stenvref)
  in
  (valenv, stenvref, stmap)


let unfreeze_environment ((valenv, stenvref, stmap) : frozen_environment) : environment =
  let stenv = StoreIDHashTable.create 128 in
  stmap |> StoreIDMap.iter (fun stid value -> StoreIDHashTable.add stenv stid value);
  stenvref := stenv;
  (valenv, ref stenv)


let output_pdf (pdfret : HandlePdf.t) : unit =
  HandlePdf.write_to_file pdfret


let output_text (abspath_out : abs_path) (data : string) : unit =
  Core.Out_channel.write_all (get_abs_path_string abspath_out) ~data


let eval_library_file (env : environment) (abspath : abs_path) (binds : binding list) : environment =
  Logging.begin_to_eval_file abspath;
  if OptionState.is_bytecomp_mode () then
    failwith "TODO: eval_libary_file, Bytecomp"
(*
    let (value, _) = Bytecomp.compile_and_exec_0 env ast in
    add_to_environment env evid (ref value)
*)
  else
    let (env, _) = Evaluator.interpret_bindings_0 env binds in
    env


let eval_main (i : int) (env_freezed : frozen_environment) (ast : abstract_tree) : syntactic_value =
  Logging.start_evaluation i;
  let res = reset () in
  begin
    match res with
    | Ok(())   -> ()
    | Error(e) -> raise (ConfigError(e))
  end;
  let env = unfreeze_environment env_freezed in
  let value =
    if OptionState.is_bytecomp_mode () then
      let (value, _) = Bytecomp.compile_and_exec_0 env ast in
      value
    else
      Evaluator.interpret_0 env ast
  in
  Logging.end_evaluation ();
  value


let eval_document_file (env : environment) (ast : abstract_tree) (abspath_out : abs_path) (abspath_dump : abs_path) =
  let env_freezed = freeze_environment env in
  if OptionState.is_text_mode () then
    let rec aux (i : int) =
      let value_str = eval_main i env_freezed ast in
      let s = EvalUtil.get_string value_str in
      match CrossRef.needs_another_trial abspath_dump with
      | CrossRef.NeedsAnotherTrial ->
          Logging.needs_another_trial ();
          aux (i + 1);

      | CrossRef.CountMax ->
          Logging.achieve_count_max ();
          output_text abspath_out s;
          Logging.end_output abspath_out;

      | CrossRef.CanTerminate unresolved_crossrefs ->
          Logging.achieve_fixpoint unresolved_crossrefs;
          output_text abspath_out s;
          Logging.end_output abspath_out;
    in
    aux 1
  else
    let rec aux (i : int) =
      let value_doc = eval_main i env_freezed ast in
      match value_doc with
      | BaseConstant(BCDocument(paper_size, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
          Logging.start_page_break ();
          State.start_page_break ();
          let pdf =
            match pbstyle with
            | SingleColumn ->
                PageBreak.main abspath_out ~paper_size
                  columnhookf pagecontf pagepartsf imvblst

            | MultiColumn(origin_shifts) ->
                PageBreak.main_multicolumn abspath_out ~paper_size
                  origin_shifts columnhookf columnendhookf pagecontf pagepartsf imvblst
          in
          begin
            match CrossRef.needs_another_trial abspath_dump with
            | CrossRef.NeedsAnotherTrial ->
                Logging.needs_another_trial ();
                aux (i + 1);

            | CrossRef.CountMax ->
                Logging.achieve_count_max ();
                output_pdf pdf;
                Logging.end_output abspath_out;

            | CrossRef.CanTerminate unresolved_crossrefs ->
                Logging.achieve_fixpoint unresolved_crossrefs;
                output_pdf pdf;
                Logging.end_output abspath_out;
          end

      | _ ->
          EvalUtil.report_bug_value "main; not a DocumentValue(...)" value_doc
    in
    aux 1


let preprocess_and_evaluate (env : environment) (libs : (abs_path * binding list) list) (ast_doc : abstract_tree) (_abspath_in : abs_path) (abspath_out : abs_path) (abspath_dump : abs_path) =

  (* Performs preprecessing:
       each evaluation called in `preprocess` is run by the naive interpreter
       regardless of whether `--bytecomp` was specified. *)
  let (env, codebindacc) =
    libs |> List.fold_left (fun (env, codebindacc) (abspath, binds) ->
      Logging.begin_to_preprocess_file abspath;
      let (env, cd_rec_or_nonrecs) = Evaluator.interpret_bindings_0 env binds in
      (env, Alist.extend codebindacc (abspath, cd_rec_or_nonrecs))
    ) (env, Alist.empty)
  in
  let codebinds = Alist.to_list codebindacc in
  let code_doc = Evaluator.interpret_1 env ast_doc in

  (* Performs evaluation: *)
  let env =
    codebinds |> List.fold_left (fun env (abspath, cd_rec_or_nonrecs) ->
      let binds =
        cd_rec_or_nonrecs |> List.map (fun cd_rec_or_nonrec ->
          Bind(Stage0, unlift_rec_or_nonrec cd_rec_or_nonrec)
        )
      in
      eval_library_file env abspath binds
    ) env
  in
  let ast_doc = unlift_code code_doc in
  eval_document_file env ast_doc abspath_out abspath_dump


let convert_abs_path_to_show (abspath : abs_path) : string =
  let abspathstr = get_abs_path_string abspath in
  if OptionState.does_show_full_path () then
    abspathstr
  else
    Filename.basename abspathstr


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

  | MultiplePackageDefinition{ context = yctx; package_name } ->
      [ NormalLine(Printf.sprintf "More than one definition for package '%s'%s" package_name (show_yaml_context yctx)) ]


let report_document_attribute_error : DocumentAttribute.error -> unit = function
  | MoreThanOneDependencyAttribute(rng1, rng2) ->
      report_error Interface [
        NormalLine("More than one attribute defines dependencies:");
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng1));
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng2));
      ]

  | NotASemanticVersion(rng, s) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a semantic version: '%s'" s);
      ]

  | NotAPackageDependency(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a package dependency description.");
      ]

  | NotAListLiteral(rng) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a list literal.");
      ]


let report_config_error : config_error -> unit = function
  | NotADocumentFile(abspath_in, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a document file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAStringFile(abspath_in, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
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

  | CannotUseHeaderUse((rng, modnm)) ->
      report_error Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s' here; use 'use %s of ...' instead." modnm modnm);
      ]

  | CannotUseHeaderUseOf((rng, modnm)) ->
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

  | PackageDirectoryNotFound(candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun path ->
          DisplayLine(Printf.sprintf "- %s" path)
        )
      in
      report_error Interface
        (NormalLine("cannot find package directory. candidates:") :: lines)

  | PackageConfigNotFound(abspath) ->
      report_error Interface [
        NormalLine("cannot find a package config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | PackageConfigError(abspath, e) ->
      report_error Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: package config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockConfigNotFound(abspath) ->
      report_error Interface [
        NormalLine("cannot find a lock config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LockConfigError(abspath, e) ->
      report_error Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: lock config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | RegistryConfigNotFound(abspath) ->
      report_error Interface [
        NormalLine("cannot find a registry config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | RegistryConfigNotFoundIn(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface (List.concat [
        [ NormalLine(Printf.sprintf "cannot find a registry config '%s'. candidates:" (get_lib_path_string libpath)) ];
        lines;
      ])

  | RegistryConfigError(abspath, e) ->
      report_error Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: registry config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockNameConflict(lock_name) ->
      report_error Interface [
        NormalLine(Printf.sprintf "lock name conflict: '%s'" lock_name);
      ]

  | LockedPackageNotFound(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface
        (NormalLine(Printf.sprintf "package '%s' not found. candidates:" (get_lib_path_string libpath)) :: lines)

  | DependencyOnUnknownLock{ depending; depended } ->
      report_error Interface [
        NormalLine(Printf.sprintf "unknown depended lock '%s' of '%s'." depended depending);
      ]

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
      report_error Interface
        (NormalLine("the following packages are cyclic:") :: lines)

  | NotALibraryFile(abspath) ->
      report_error Interface [
        NormalLine("the following file is expected to be a library file, but is not:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | CannotFindLibraryFile(libpath, candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | LocalFileNotFound{ relative; candidates } ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error Interface
        (NormalLine(Printf.sprintf "cannot find local file '%s'. candidates:" relative) :: lines)

  | CannotSolvePackageConstraints ->
      report_error Interface [
        NormalLine("cannot solve package constraints.");
      ]

  | DocumentAttributeError(e) ->
      report_document_attribute_error e


let report_font_error : font_error -> unit = function
  | FailedToReadFont(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load font file '%s';" fname);
        DisplayLine(msg);
      ]

  | FailedToDecodeFont(abspath, e) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot decode font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Decode.Error.pp e);
      ]

  | FailedToMakeSubset(abspath, e) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot make a subset of font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Subset.Error.pp e);
      ]

  | NotASingleFont(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "the font file '%s' is not a single font file." fname);
      ]

  | NotAFontCollectionElement(abspath, index) ->
      let fname = convert_abs_path_to_show abspath in
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
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'MATH' table." fname);
      ]

  | PostscriptNameNotFound(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a PostScript name." fname);
      ]

  | CannotFindUnicodeCmap(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'cmap' subtable for Unicode code points." fname);
      ]

  | CollectionIndexOutOfBounds{ path; index; num_elements } ->
      let fname = convert_abs_path_to_show path in
      report_error Interface [
        NormalLine(Printf.sprintf "%d: index out of bounds;" index);
        NormalLine(Printf.sprintf "font file '%s' has %d elements." fname num_elements);
      ]


let error_log_environment (suspended : unit -> unit) : unit =
  try
    suspended ()
  with
  | RemainsToBeImplemented(msg) ->
      report_error Interface [
        NormalLine("remains to be supported:");
        DisplayLine(msg);
      ]

  | NoLibraryRootDesignation ->
      report_error Interface [
        NormalLine("cannot determine where the SATySFi library root is;");
        NormalLine("set appropriate environment variables");
        NormalLine("or specify configuration search paths with -C option.");
      ]

  | ShouldSpecifyOutputFile ->
      report_error Interface [
        NormalLine("should specify output file for text mode.");
      ]

  | LoadHyph.InvalidPatternElement(rng) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid string for hyphenation pattern.");
      ]

  | ConfigError(e) ->
      report_config_error e

  | FontInfo.FontInfoError(e) ->
      report_font_error e

  | ImageHashTable.CannotLoadPdf(msg, abspath, pageno) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load PDF file '%s' page #%d;" fname pageno);
        DisplayLine(msg);
      ]

  | ImageHashTable.CannotLoadImage(msg, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine(msg);
      ]

  | ImageHashTable.ImageOfWrongFileType(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This file format is not supported.");
      ]

  | ImageHashTable.UnsupportedColorModel(_, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This color model is not supported.");
      ]

  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(s);
      ]

  | LoadMDSetting.MultipleCodeNameDesignation(rng, s) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "multiple designation for key '%s'." s);
      ]

  | LoadMDSetting.NotAnInlineCommand(rng, s) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is not an inline command name." s);
      ]

  | LoadMDSetting.NotABlockCommand(rng, s) ->
      report_error System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is not a block command name." s);
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


let setup_root_dirs ~(no_default_config : bool) ~(extra_config_paths : (string list) option) (curdir : string) =
  let runtime_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "SATYSFI_RUNTIME" with
      | None    -> []
      | Some(s) -> [ s ]
    else
      [ "/usr/local/share/satysfi"; "/usr/share/satysfi" ]
  in
  let home_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "userprofile" with
      | None    -> []
      | Some(s) -> [ Filename.concat s ".satysfi" ]
    else
      match Sys.getenv_opt "HOME" with
      | None    -> []
      | Some(s) -> [ Filename.concat s ".satysfi" ]
  in
  let default_dirs =
    if no_default_config then
      []
    else
      List.concat [ home_dirs; runtime_dirs ]
  in
  let extra_dirs =
    match extra_config_paths with
    | None             -> [ Filename.concat curdir ".satysfi" ]
    | Some(extra_dirs) -> extra_dirs
  in
  let dirs = List.concat [ extra_dirs; default_dirs ] in
  match dirs with
  | []     -> raise NoLibraryRootDesignation
  | _ :: _ -> Config.initialize dirs


let make_absolute_if_relative ~(origin : string) (s : string) : abs_path =
  let abspath_str = if Filename.is_relative s then Filename.concat origin s else s in
  make_abs_path abspath_str


let get_candidate_file_extensions () =
  match OptionState.get_output_mode () with
  | PdfMode           -> [ ".satyh"; ".satyg" ]
  | TextMode(formats) -> List.append (formats |> List.map (fun s -> ".satyh-" ^ s)) [ ".satyg" ]


type build_input =
  | PackageBuildInput of {
      lock : abs_path;
    }
  | DocumentBuildInput of {
      lock : abs_path;
      out  : abs_path;
      dump : abs_path;
    }


let check_depended_packages ~(lock_config_dir : abs_path) ~(extensions : string list) (tyenv_prim : Typeenv.t) (lock_config : LockConfig.t) =
  (* Resolve dependency among locked packages: *)
  let sorted_packages =
    match ClosedLockDependencyResolver.main ~lock_config_dir ~extensions lock_config with
    | Ok(sorted_packages) -> sorted_packages
    | Error(e)            -> raise (ConfigError(e))
  in

  (* Typecheck every locked package: *)
  let (genv, libacc) =
    sorted_packages |> List.fold_left (fun (genv, libacc) (_lock_name, package) ->
      let main_module_name =
        match package with
        | UTLibraryPackage{ main_module_name; _ } -> main_module_name
        | UTFontPackage{ main_module_name; _ }    -> main_module_name
      in
      let (ssig, libs) =
        match PackageChecker.main tyenv_prim genv package with
        | Ok(pair) -> pair
        | Error(e) -> raise (ConfigError(e))
      in
      let genv = genv |> GlobalTypeenv.add main_module_name ssig in
      let libacc = Alist.append libacc libs in
      (genv, libacc)
    ) (GlobalTypeenv.empty, Alist.empty)
  in
  (genv, Alist.to_list libacc)


let make_package_lock_config_path (abspathstr_in : string) =
  make_abs_path (Printf.sprintf "%s/package.satysfi-lock" abspathstr_in)


let make_document_lock_config_path (basename_without_extension : string) =
  make_abs_path (Printf.sprintf "%s.satysfi-lock" basename_without_extension)


let build
    ~(fpath_in : string)
    ~(fpath_out_opt : string option)
    ~(config_paths_str_opt : string option)
    ~(text_mode_formats_str_opt : string option)
    ~(markdown_style_str_opt : string option)
    ~(page_number_limit : int)
    ~(show_full_path : bool)
    ~(debug_show_bbox : bool)
    ~(debug_show_space : bool)
    ~(debug_show_block_bbox : bool)
    ~(debug_show_block_space : bool)
    ~(debug_show_overfull : bool)
    ~(type_check_only : bool)
    ~(bytecomp : bool)
    ~(show_fonts : bool)
    ~(no_default_config : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let input_file = make_absolute_if_relative ~origin:curdir fpath_in in
    let output_file = fpath_out_opt |> Option.map (make_absolute_if_relative ~origin:curdir) in
    let extra_config_paths = config_paths_str_opt |> Option.map (String.split_on_char ':') in
    let output_mode =
      match text_mode_formats_str_opt with
      | None    -> OptionState.PdfMode
      | Some(s) -> OptionState.TextMode(String.split_on_char ',' s)
    in
    let input_kind =
      match markdown_style_str_opt with
      | None          -> OptionState.SATySFi
      | Some(setting) -> OptionState.Markdown(setting)
    in
    OptionState.set OptionState.{
      command_state =
        BuildState{
          input_file;
          output_file;
          output_mode;
          input_kind;
          page_number_limit;
          debug_show_bbox;
          debug_show_space;
          debug_show_block_bbox;
          debug_show_block_space;
          debug_show_overfull;
          type_check_only;
          bytecomp;
        };
      extra_config_paths;
      show_full_path;
      show_fonts;
      no_default_config;
    };

    setup_root_dirs ~no_default_config ~extra_config_paths curdir;
    let abspath_in = input_file in
    let build_input =
      let abspathstr_in = get_abs_path_string abspath_in in
      if Sys.is_directory abspathstr_in then
        (* If the input is a package directory: *)
          let abspath_lock_config = make_package_lock_config_path abspathstr_in in
          PackageBuildInput{
            lock = abspath_lock_config;
          }
      else
        (* If the input is a document file: *)
        let basename_without_extension = Filename.remove_extension abspathstr_in in
        let abspath_lock_config = make_document_lock_config_path basename_without_extension in
        let abspath_out =
          match (output_mode, output_file) with
          | (_, Some(abspath_out)) -> abspath_out
          | (TextMode(_), None)    -> raise ShouldSpecifyOutputFile
          | (PdfMode, None)        -> make_abs_path (Printf.sprintf "%s.pdf" basename_without_extension)
        in
        let abspath_dump = make_abs_path (Printf.sprintf "%s.satysfi-aux" basename_without_extension) in
        DocumentBuildInput{
          lock = abspath_lock_config;
          out  = abspath_out;
          dump = abspath_dump;
        }
    in

    let extensions = get_candidate_file_extensions () in
    let (tyenv_prim, env) = initialize () in

    match build_input with
    | PackageBuildInput{
        lock = abspath_lock_config;
      } ->
        Logging.lock_config_file abspath_lock_config;
        let lock_config =
          match LockConfig.load abspath_lock_config with
          | Ok(lock_config) -> lock_config
          | Error(e)        -> raise (ConfigError(e))
        in

        let package =
          match PackageReader.main ~extensions abspath_in with
          | Ok(package) -> package
          | Error(e)    -> raise (ConfigError(e))
        in

        let (genv, _libs_dep) =
          let lock_config_dir = make_abs_path (Filename.dirname (get_abs_path_string abspath_lock_config)) in
          check_depended_packages ~lock_config_dir ~extensions tyenv_prim lock_config
        in

        begin
          match PackageChecker.main tyenv_prim genv package with
          | Ok((_ssig, _libs)) -> ()
          | Error(e)           -> raise (ConfigError(e))
        end

    | DocumentBuildInput{
        lock = abspath_lock_config;
        out  = abspath_out;
        dump = abspath_dump;
      } ->
        Logging.lock_config_file abspath_lock_config;
        let lock_config =
          match LockConfig.load abspath_lock_config with
          | Ok(lock_config) -> lock_config
          | Error(e)        -> raise (ConfigError(e))
        in

        Logging.target_file abspath_out;

        let dump_file_exists = CrossRef.initialize abspath_dump in
        Logging.dump_file ~already_exists:dump_file_exists abspath_dump;

        (* Resolve dependency of the document and the local source files: *)
        let (_dep_main_module_names, sorted_locals, utdoc) =
          match OpenFileDependencyResolver.main ~extensions abspath_in with
          | Ok(triple) -> triple
          | Error(e)   -> raise (ConfigError(e))
        in

        let (genv, libs) =
          let lock_config_dir = make_abs_path (Filename.dirname (get_abs_path_string abspath_lock_config)) in
          check_depended_packages ~lock_config_dir ~extensions tyenv_prim lock_config
        in

        (* Typechecking and elaboration: *)
        let (libs_local, ast_doc) =
          match PackageChecker.main_document tyenv_prim genv sorted_locals (abspath_in, utdoc) with
          | Ok(pair) -> pair
          | Error(e) -> raise (ConfigError(e))
        in
        let libs = List.append libs libs_local in
        if type_check_only then
          ()
        else
          preprocess_and_evaluate env libs ast_doc abspath_in abspath_out abspath_dump
  )


type solve_input =
  | PackageSolveInput of {
      root : abs_path; (* The absolute path of a directory used as the package root *)
      lock : abs_path; (* A path for writing a resulting lock file *)
    }
  | DocumentSolveInput of {
      path : abs_path; (* The absolute path to the document file *)
      lock : abs_path; (* A path for writing a resulting lock file *)
    }


let make_lock_name (package_name : package_name) (semver : SemanticVersion.t) : lock_name =
  Printf.sprintf "%s.%s" package_name (SemanticVersion.to_string semver)


let convert_solutions_to_lock_config (solutions : package_solution list) : LockConfig.t =
  let locked_packages =
    solutions |> List.map (fun solution ->
      let package_name = solution.package_name in
      let lock_name = make_lock_name package_name solution.locked_version in
      let lock_location =
        LockConfig.GlobalLocation{
          path = Printf.sprintf "./dist/packages/%s/%s/" package_name lock_name;
        }
      in
      let lock_dependencies =
        solution.locked_dependencies |> List.map (fun (package_name_dep, semver_dep) ->
          make_lock_name package_name_dep semver_dep
        )
      in
      LockConfig.{ lock_name; lock_location; lock_dependencies }
    )
  in
  LockConfig.{ locked_packages }


let extract_attributes_from_document_file (abspath_in : abs_path) : (DocumentAttribute.t, config_error) result =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
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


let solve
    ~(fpath_in : string)
    ~(show_full_path : bool)
    ~(config_paths_str_opt : string option)
    ~(no_default_config : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let extra_config_paths = config_paths_str_opt |> Option.map (String.split_on_char ':') in

    OptionState.set OptionState.{
      command_state = SolveState;
      extra_config_paths;
      show_full_path;
      show_fonts = false;
      no_default_config;
    };

    setup_root_dirs ~no_default_config ~extra_config_paths curdir;

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
        let basename_without_extension = Filename.remove_extension (get_abs_path_string abspath_in) in
        let abspath_lock_config = make_document_lock_config_path basename_without_extension in
        DocumentSolveInput{
          path = abspath_in;
          lock = abspath_lock_config;
        }
    in

    let res =
      let open ResultMonad in
      let* abspath_registry_config =
        let libpath = make_lib_path "dist/cache/registry.yaml" in
        Config.resolve_lib_file libpath
          |> Result.map_error (fun candidates -> RegistryConfigNotFoundIn(libpath, candidates))
      in
      let* (dependencies, abspath_lock_config) =
        match solve_input with
        | PackageSolveInput{
            root = absdir_package;
            lock = abspath_lock_config;
          } ->
            let* config = PackageConfig.load absdir_package in
            begin
              match config.package_contents with
              | PackageConfig.Library{ dependencies; _ } ->
                  return (dependencies, abspath_lock_config)

              | PackageConfig.Font(_) ->
                  let dependencies = [] in
                  return (dependencies, abspath_lock_config)
            end

        | DocumentSolveInput{
            path = abspath_in;
            lock = abspath_lock_config;
          } ->
            let* docattr = extract_attributes_from_document_file abspath_in in
            return (docattr.DocumentAttribute.dependencies, abspath_lock_config)
      in

      Logging.show_package_dependency_before_solving dependencies;

      let* package_context = PackageRegistry.load abspath_registry_config in
      let solutions_opt = PackageConstraintSolver.solve package_context dependencies in
      begin
        match solutions_opt with
        | None ->
            err CannotSolvePackageConstraints

        | Some(solutions) ->

            Logging.show_package_dependency_solutions solutions;

            let lock_config = convert_solutions_to_lock_config solutions in
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
