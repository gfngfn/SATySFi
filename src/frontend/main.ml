
open MyUtil
open Types
open StaticEnv


exception NoLibraryRootDesignation
exception NotADocumentFile             of abs_path * Typeenv.t * mono_type
exception NotAStringFile               of abs_path * Typeenv.t * mono_type
exception ShouldSpecifyOutputFile


(* Initialization that should be performed before every cross-reference-solving loop *)
let reset () =
  if OptionState.is_text_mode () then
    ()
  else begin
    FontInfo.initialize ();
    ImageInfo.initialize ();
    NamedDest.initialize ();
  end


(* Initialization that should be performed before typechecking *)
let initialize (abspath_dump : abs_path) : Typeenv.t * environment * bool =
  FreeID.initialize ();
  BoundID.initialize ();
  EvalVarID.initialize ();
  StoreID.initialize ();
  let dump_file_exists = CrossRef.initialize abspath_dump in
  let (tyenv, env) =
    if OptionState.is_text_mode () then
      Primitives.make_text_mode_environments ()
    else
      Primitives.make_pdf_mode_environments ()
  in
  begin
    if OptionState.is_bytecomp_mode () then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env, dump_file_exists)


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref * syntactic_value StoreIDMap.t


let freeze_environment (env : environment) : frozen_environment =
  let open MyUtil in
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


let typecheck_library_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utsig_opt : untyped_signature option) (utbinds : untyped_binding list) : StructSig.t abstracted * binding list =
  Logging.begin_to_typecheck_file abspath_in;
  let res = Typechecker.main_bindings tyenv utsig_opt utbinds in
  Logging.pass_type_check None;
  res


let typecheck_document_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree =
  Logging.begin_to_typecheck_file abspath_in;
  let (ty, ast) = Typechecker.main Stage1 tyenv utast in
  Logging.pass_type_check (Some(Display.show_mono_type ty));
  if OptionState.is_text_mode () then
    if Typechecker.are_unifiable ty (Range.dummy "text-mode", BaseType(StringType)) then
      ast
    else
      raise (NotAStringFile(abspath_in, tyenv, ty))
  else
    if Typechecker.are_unifiable ty (Range.dummy "pdf-mode", BaseType(DocumentType)) then
      ast
    else
      raise (NotADocumentFile(abspath_in, tyenv, ty))


let output_pdf (pdfret : HandlePdf.t) : unit =
  HandlePdf.write_to_file pdfret


let output_text (abspath_out : abs_path) (s : string) : unit =
  let outc = open_out_abs abspath_out in
  output_string outc s;
  close_out outc


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
  reset ();
  let env = unfreeze_environment env_freezed in
  let value =
    if OptionState.is_bytecomp_mode () then
      Bytecomp.compile_and_exec_0 env ast
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


let preprocess_and_evaluate (env : environment) (libs : (abs_path * binding list) list) (ast_doc : abstract_tree) (abspath_in : abs_path) (abspath_out : abs_path) (abspath_dump : abs_path) =

  (* Performs preprecessing:
       each evaluation called in `preprocess` is run by the naive interpreter
       regardless of whether `--bytecomp` was specified. *)
  let (env, codebindacc) =
    libs |> List.fold_left (fun (env, codebindacc) (abspath, binds) ->
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
  | NormalLine        of string
  | DisplayLine       of string
  | NormalLineOption  of string option
  | DisplayLineOption of string option

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
  lines |> List.fold_left (fun is_first line ->
    begin
      match line with
      | NormalLine(s)
      | NormalLineOption(Some(s)) ->
          if is_first then
            print_endline s
          else
            print_endline ("    " ^ s)

      | DisplayLine(s)
      | DisplayLineOption(Some(s)) ->
          if is_first then
            print_endline ("\n      " ^ s)
          else
            print_endline ("      " ^ s)

      | _ ->
          ()
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


let error_log_environment suspended =
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

  | FileDependencyResolver.CyclicFileDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      report_error Interface (
        (NormalLine("cyclic dependency detected:")) ::
        (pairs |> List.map (fun (abspath, _) -> DisplayLine(get_abs_path_string abspath)))
      )

  | FileDependencyResolver.CannotReadFileOwingToSystem(msg) ->
      report_error Interface [
        NormalLine("cannot read file:");
        DisplayLine(msg);
      ]

  | FileDependencyResolver.LibraryContainsWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "file '%s' is not a library; it has a return value." fname);
      ]

  | FileDependencyResolver.DocumentLacksWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "file '%s' is not a document; it lacks a return value." fname);
      ]

  | Config.PackageNotFound(package, pathcands) ->
      report_error Interface (List.append [
        NormalLine("package file not found:");
        DisplayLine(package);
        NormalLine("candidate paths:");
      ] (pathcands |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath))))

  | Config.LibraryFileNotFound(relpath, pathcands) ->
      report_error Interface (List.append [
        NormalLine("library file not found:");
        DisplayLine(get_lib_path_string relpath);
        NormalLine("candidate paths:");
      ] (pathcands |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath))))

  | Config.LibraryFilesNotFound(relpaths, pathcands) ->
      report_error Interface (List.concat [
        [ NormalLine("any of the following library file(s) not found:"); ];
        relpaths |> List.map (fun relpath -> DisplayLine(get_lib_path_string relpath));
        [ NormalLine("candidate paths:"); ];
        pathcands |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath));
      ])

  | Config.ImportedFileNotFound(s, pathcands) ->
      report_error Interface (List.append [
        NormalLine("imported file not found:");
        DisplayLine(s);
        NormalLine("candidate paths:");
      ] (pathcands |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath))))

  | NotADocumentFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a document file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAStringFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a file for generating text; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
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

  | FontFormat.FailToLoadFontOwingToSystem(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load font file '%s';" fname);
        DisplayLine(msg);
      ]

  | FontFormat.BrokenFont(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' is broken;" fname);
        DisplayLine(msg);
      ]

  | FontFormat.CannotFindUnicodeCmap(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have 'cmap' subtable for Unicode code points." fname);
      ]

  | FontInfo.InvalidFontAbbrev(abbrev) ->
      report_error Interface [
        NormalLine (Printf.sprintf "cannot find a font named '%s'." abbrev);
      ]

  | FontInfo.InvalidMathFontAbbrev(mfabbrev) ->
      report_error Interface [
        NormalLine(Printf.sprintf "cannot find a math font named '%s'." mfabbrev);
      ]

  | FontInfo.NotASingleFont(abbrev, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "the font file '%s'," fname);
        NormalLine(Printf.sprintf "which is associated with the font name '%s'," abbrev);
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

  | FontInfo.NotASingleMathFont(mfabbrev, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "the font file '%s'," fname);
        NormalLine(Printf.sprintf "which is associated with the math font name '%s'," mfabbrev);
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

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

  | IllegalArgumentLength(rng, len, lenexp) ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "this declaration has %d argument pattern(s)," len);
        NormalLine(Printf.sprintf "but is expected to have %d." lenexp);
      ]

  | ParserInterface.Error(rng) ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
      ]

  | ParseErrorDetail(rng, s) ->
      report_error Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(s)
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

  | Typechecker.TypeError(tyerr) ->
      begin
        match tyerr with
        | UndefinedVariable(rng, varnm, candidates) ->
            report_error Typechecker [
              NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
              NormalLine(Printf.sprintf "undefined variable '%s'." varnm);
              NormalLineOption(make_candidates_message candidates);
            ]

        | UndefinedConstructor(rng, constrnm, candidates) ->
            report_error Typechecker [
              NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
              NormalLine(Printf.sprintf "undefined constructor '%s'." constrnm);
              NormalLineOption(make_candidates_message candidates);
            ]

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
        | UndefinedHorzMacro(rng, csnm) ->
            report_error Typechecker [
              NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
              NormalLine(Printf.sprintf "undefined inline macro '%s'." csnm);
            ]

        | UndefinedVertMacro(rng, csnm) ->
            report_error Typechecker [
              NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
              NormalLine(Printf.sprintf "undefined block macro '%s'." csnm);
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

        | HorzCommandInMath(rng) ->
            report_error Typechecker [
              NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
              NormalLine("an inline command is used as a math command.");
            ]

        | MathCommandInHorz(rng) ->
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

        | ContradictionError(((rng1, _) as ty1), ((rng2, _) as ty2)) ->
            let strty1 = Display.show_mono_type ty1 in
            let strty2 = Display.show_mono_type ty2 in
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
              report_error Typechecker (List.append [
                NormalLine(posmsg);
                NormalLine("this expression has type");
                DisplayLine(Printf.sprintf "%s," strtyA);
                NormalLine("but is expected of type");
                DisplayLine(Printf.sprintf "%s." strtyB);
              ] additional)

        | InclusionError(((rng1, _) as ty1), ((rng2, _) as ty2)) ->
            let strty1 = Display.show_mono_type ty1 in
            let strty2 = Display.show_mono_type ty2 in
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
              report_error Typechecker (List.append [
                NormalLine(posmsg);
                NormalLine("this expression has types");
                DisplayLine(strtyA);
                NormalLine("and");
                DisplayLine(strtyB);
                NormalLine("at the same time, but these are incompatible.");
              ] additional)

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

        | NotASubtypeAboutType(rng, tynm, _tentry1, _tentry2) ->
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

      end

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


let setup_root_dirs (curdir : string) =
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
    if OptionState.use_no_default_config () then
      []
    else
      List.concat [ home_dirs; runtime_dirs ]
  in
  let extra_dirs =
    match OptionState.get_extra_config_paths () with
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
      input_file;
      output_file;
      extra_config_paths;
      output_mode;
      input_kind;
      page_number_limit;
      show_full_path;
      debug_show_bbox;
      debug_show_space;
      debug_show_block_bbox;
      debug_show_block_space;
      debug_show_overfull;
      type_check_only;
      bytecomp;
      show_fonts;
      no_default_config;
    };

    setup_root_dirs curdir;
    let abspath_in = input_file in
    let basename_without_extension =
      let abspathstr_in = get_abs_path_string abspath_in in
      try Filename.chop_extension abspathstr_in with
      | Invalid_argument(_) -> abspathstr_in
    in
    let abspath_dump = make_abs_path (Printf.sprintf "%s.satysfi-aux" basename_without_extension) in
    let abspath_out =
      match (output_mode, output_file) with
      | (_, Some(abspath_out)) -> abspath_out
      | (TextMode(_), None)    -> raise ShouldSpecifyOutputFile
      | (PdfMode, None)        -> make_abs_path (Printf.sprintf "%s.pdf" basename_without_extension)
    in
    Logging.target_file abspath_out;
    let (tyenv, env, dump_file_exists) = initialize abspath_dump in
    Logging.dump_file dump_file_exists abspath_dump;

    (* Resolve dependency: *)
    let inputs = FileDependencyResolver.main abspath_in in

    (* Typechecking and elaboration: *)
    let (_, libacc, ast_opt) =
      inputs |> List.fold_left (fun (tyenv, libacc, docopt) (abspath, file_info) ->
        match file_info with
        | DocumentFile(utast) ->
            let ast = typecheck_document_file tyenv abspath utast in
            (tyenv, libacc, Some(ast))

        | LibraryFile((modident, utsig_opt, utbinds)) ->
            let (_, modnm) = modident in
            let ((_quant, ssig), binds) = typecheck_library_file tyenv abspath utsig_opt utbinds in
            let mentry = { mod_signature = ConcStructure(ssig); } in
            let tyenv = tyenv |> Typeenv.add_module modnm mentry in
            (tyenv, Alist.extend libacc (abspath, binds), docopt)
      ) (tyenv, Alist.empty, None)
    in
    let libs = Alist.to_list libacc in

    if type_check_only then
      ()
    else
      match ast_opt with
      | None      -> assert false
      | Some(ast) -> preprocess_and_evaluate env libs ast abspath_in abspath_out abspath_dump
  )
