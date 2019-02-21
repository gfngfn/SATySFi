
open MyUtil
open Types
open Display


exception NoLibraryRootDesignation
exception NoInputFileDesignation
exception CyclicFileDependency        of abs_path list
exception CannotReadFileOwingToSystem of string
exception NotALibraryFile             of abs_path * Typeenv.t * mono_type
exception NotADocumentFile            of abs_path * Typeenv.t * mono_type
exception NotAStringFile              of abs_path * Typeenv.t * mono_type
exception ShouldSpecifyOutputFile
exception DocumentShouldBeAtStage1
exception InvalidDependencyAsToStaging of abs_path * stage * abs_path * stage


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
  let aux lst =
    lst |> List.fold_left (fun is_first line ->
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
    ) true
  in
  print_string ("! [" ^ (show_error_category cat) ^ "] ");
  aux lines |> ignore;
  exit 1


let make_candidates_message (candidates : string list) =
  let add_quote s = "'" ^ s ^ "'" in
  let rec aux lst =
    match List.rev lst with
    | []        -> ""
    | s :: []   -> add_quote s
    | s :: rest -> (String.concat ", " (List.map add_quote (List.rev rest))) ^ " or " ^ (add_quote s)
  in
  match candidates with
  | [] -> None
  | _  -> Some("Did you mean " ^ (aux candidates) ^ "?")


module FileDependencyGraph = DirectedGraph.Make
  (struct
    type t = abs_path
    let compare ap1 ap2 = String.compare (get_abs_path_string ap1) (get_abs_path_string ap2)
    let show ap = Filename.basename (get_abs_path_string ap)
  end)


type file_info =
  | DocumentFile of untyped_abstract_tree
  | LibraryFile  of stage * untyped_abstract_tree


let make_abs_path_of_required package =
  let extcands =
    match OptionState.get_mode () with
    | None      -> [".satyh"; ".satyg"]
    | Some(lst) -> List.append (lst |> List.map (fun s -> ".satyh-" ^ s)) [".satyg"]
  in
  Config.resolve_package_exn package extcands


let make_abs_path_of_package curdir headerelem =
  match headerelem with
  | HeaderRequire(s) -> make_abs_path_of_required s
  | HeaderImport(s)  -> make_abs_path (Filename.concat curdir (s ^ ".satyh"))


let rec register_library_file (dg : file_info FileDependencyGraph.t) (abspath : abs_path) : unit =
  begin
    Logging.begin_to_parse_file abspath;
    let curdir = Filename.dirname (get_abs_path_string abspath) in
    let inc = open_in_abs abspath in
    let (stage, header, utast) = ParserInterface.process (basename_abs abspath) (Lexing.from_channel inc) in
    FileDependencyGraph.add_vertex dg abspath (LibraryFile(stage, utast));
    header |> List.iter (fun headerelem ->
      let abspath_sub = make_abs_path_of_package curdir headerelem in
      begin
        if FileDependencyGraph.mem_vertex abspath_sub dg then () else
          register_library_file dg abspath_sub
      end;
      begin
        match FileDependencyGraph.get_vertex dg abspath_sub with
        | LibraryFile(stage_sub, _) ->
            begin
              match (stage_sub, stage) with
              | (Stage1, Stage0)
              | (Stage1, Persistent0)
              | (Stage0, Persistent0) ->
                  raise (InvalidDependencyAsToStaging(abspath, stage, abspath_sub, stage_sub))
              | _ ->
                  ()
            end

        | DocumentFile(_) ->
            assert false
      end;
      FileDependencyGraph.add_edge dg abspath abspath_sub
    )
  end


(* -- initialization that should be performed before every cross-reference-solving loop -- *)
let reset () =
  if OptionState.is_text_mode () then
    ()
  else begin
    FontInfo.initialize ();
    ImageInfo.initialize ();
    NamedDest.initialize ();
  end


(* -- initialization that should be performed before typechecking -- *)
let initialize (abspath_dump : abs_path) =
  FreeID.initialize ();
  BoundID.initialize ();
  Typeenv.initialize_id ();
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
    if OptionState.bytecomp_mode () then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env, dump_file_exists)


let output_pdf pdfret =
  HandlePdf.write_to_file pdfret


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref * syntactic_value StoreIDMap.t


let freeze_environment (env : environment) : frozen_environment =
  let open MyUtil in
  let (valenv, stenvref) = env in
  let stmap =
    StoreIDMap.empty @|> (!stenvref) @|> StoreIDHashTable.fold (fun stid value stmap ->
      stmap |> StoreIDMap.add stid value
    )
  in
  (valenv, stenvref, stmap)


let unfreeze_environment ((valenv, stenvref, stmap) : frozen_environment) : environment =
  let stenv = StoreIDHashTable.create 128 in
  stmap |> StoreIDMap.iter (fun stid value -> StoreIDHashTable.add stenv stid value);
  stenvref := stenv;
  (valenv, ref stenv)


let register_document_file (dg : file_info FileDependencyGraph.t) (abspath_in : abs_path) : unit =
  Logging.begin_to_parse_file abspath_in;
  let file_in = open_in_abs abspath_in in
  let curdir = Filename.dirname (get_abs_path_string abspath_in) in
  let (stage, header, utast) = ParserInterface.process (Filename.basename (get_abs_path_string abspath_in)) (Lexing.from_channel file_in) in
  begin
    match stage with
    | Stage1               -> ()
    | Stage0 | Persistent0 -> raise DocumentShouldBeAtStage1
  end;
  FileDependencyGraph.add_vertex dg abspath_in (DocumentFile(utast));
  header |> List.iter (fun headerelem ->
    let file_path_sub = make_abs_path_of_package curdir headerelem in
    begin
      if FileDependencyGraph.mem_vertex file_path_sub dg then () else
        register_library_file dg file_path_sub
    end;
    FileDependencyGraph.add_edge dg abspath_in file_path_sub
  )


let register_markdown_file (dg : file_info FileDependencyGraph.t) (setting : string) (abspath_in : abs_path) : unit =
  Logging.begin_to_parse_file abspath_in;
  let abspath = Config.resolve_lib_file_exn (make_lib_path (Filename.concat "dist/md" (setting ^ ".satysfi-md"))) in
  let (cmdrcd, depends) = LoadMDSetting.main abspath in
  match MyUtil.string_of_file abspath_in with
  | Ok(data) ->
      let utast = DecodeMD.decode cmdrcd data in
    (*
        let () = Format.printf "%a\n" pp_untyped_abstract_tree utast in  (* for debug *)
    *)
      FileDependencyGraph.add_vertex dg abspath_in (DocumentFile(utast));
      depends |> List.iter (fun package ->
        let file_path_sub = make_abs_path_of_required package in
        begin
        if FileDependencyGraph.mem_vertex file_path_sub dg then () else
          register_library_file dg file_path_sub
        end;
        FileDependencyGraph.add_edge dg abspath_in file_path_sub
      )

  | Error(msg) ->
      raise (CannotReadFileOwingToSystem(msg))


let output_text abspath_out s =
  let outc = open_out_abs abspath_out in
  output_string outc s;
  close_out outc


let typecheck_library_file (stage : stage) (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : Typeenv.t * abstract_tree =
  Logging.begin_to_typecheck_file abspath_in;
  let (ty, tyenvnew, ast) = Typechecker.main stage tyenv utast in
  Logging.pass_type_check None;
  match ty with
  | (_, BaseType(EnvType)) -> (tyenvnew, ast)
  | _                      -> raise (NotALibraryFile(abspath_in, tyenvnew, ty))


let typecheck_document_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree =
  Logging.begin_to_typecheck_file abspath_in;
  let (ty, _, ast) = Typechecker.main Stage1 tyenv utast in
  Logging.pass_type_check (Some(Display.string_of_mono_type tyenv ty));
  if OptionState.is_text_mode () then
    match ty with
    | (_, BaseType(StringType)) -> ast
    | _                         -> raise (NotAStringFile(abspath_in, tyenv, ty))
  else
    match ty with
    | (_, BaseType(DocumentType)) -> ast
    | _                           -> raise (NotADocumentFile(abspath_in, tyenv, ty))


let eval_library_file ~(is_preprocess : bool) (env : environment) (abspath : abs_path) (ast : abstract_tree) : environment =
  Logging.begin_to_eval_file abspath;
  let value =
    if (not is_preprocess) && OptionState.bytecomp_mode () then
      Bytecomp.compile_and_exec_0 env ast
    else
      Evaluator.interpret_0 env ast
  in
  match value with
  | EvaluatedEnvironment(envnew) -> envnew
  | _                            -> EvalUtil.report_bug_value "not an EvaluatedEnvironment(...)" value


let preprocess_file (env : environment) (abspath : abs_path) (ast : abstract_tree) : code_value =
  Logging.begin_to_preprocess_file abspath;
  Evaluator.interpret_1 env ast


let eval_main i env_freezed ast =
  Logging.start_evaluation i;
  reset ();
  let env = unfreeze_environment env_freezed in
  let valuedoc =
    if OptionState.bytecomp_mode () then
      Bytecomp.compile_and_exec_0 env ast
    else
      Evaluator.interpret_0 env ast
  in
  Logging.end_evaluation ();
  valuedoc


let eval_document_file (env : environment) (code : code_value) (abspath_out : abs_path) (abspath_dump : abs_path) =
  let ast = unlift_code code in
  let env_freezed = freeze_environment env in
  if OptionState.is_text_mode () then
    let rec aux i =
      let valuestr = eval_main i env_freezed ast in
      let s = EvalUtil.get_string valuestr in
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
    let rec aux i =
      let valuedoc = eval_main i env_freezed ast in
      match valuedoc with
      | DocumentValue(pagesize, pagecontf, pagepartsf, imvblst) ->
          Logging.start_page_break ();
          State.start_page_break ();
          let pdf = PageBreak.main abspath_out pagesize pagecontf pagepartsf imvblst in
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
          EvalUtil.report_bug_value "main; not a DocumentValue(...)" valuedoc
    in
    aux 1


let eval_abstract_tree_list (env : environment) (libs : (stage * abs_path * abstract_tree) list) (astdoc : abstract_tree) (abspath_in : abs_path) (abspath_out : abs_path) (abspath_dump : abs_path) =

  let rec preprocess (codeacc : (abs_path * code_value) Alist.t) (env : environment) libs =
    match libs with
    | [] ->
        let codedoc = preprocess_file env abspath_in astdoc in
        (env, Alist.to_list codeacc, codedoc)

    | ((Stage0 | Persistent0), abspath, astlib0) :: tail ->
        let envnew = eval_library_file ~is_preprocess:true env abspath astlib0 in
        preprocess codeacc envnew tail

    | (Stage1, abspath, astlib1) :: tail ->
        let code = preprocess_file env abspath astlib1 in
        preprocess (Alist.extend codeacc (abspath, code)) env tail
  in
    (* --
       each evaluation called in `preprocess` is run by the naive interpreter
       regardless of whether `--bytecomp` was specified.
       -- *)

  let rec eval (env : environment) (codes : (abs_path * code_value) list) : environment =
    match codes with
    | [] ->
        env

    | (abspath, code) :: tail ->
        let ast = unlift_code code in
        let envnew = eval_library_file ~is_preprocess:false env abspath ast in
        eval envnew tail
  in
  let (env, codes, codedoc) = preprocess Alist.empty env libs in
  let env = eval env codes in
  eval_document_file env codedoc abspath_out abspath_dump


let convert_abs_path_to_show abspath =
  let abspathstr = get_abs_path_string abspath in
  if OptionState.show_full_path () then
    abspathstr
  else
    Filename.basename abspathstr


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
        NormalLine("set appropriate environment variables.");
      ]

  | NoInputFileDesignation ->
      report_error Interface [
        NormalLine("no input file designation.");
      ]

  | CyclicFileDependency(cycle) ->
      report_error Interface (
        (NormalLine("cyclic dependency detected:")) ::
        (cycle |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath)))
      )

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

  | NotALibraryFile(abspath, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a header file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | NotADocumentFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a document file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | NotAStringFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a file for generating text; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | ShouldSpecifyOutputFile ->
      report_error Interface [
        NormalLine("should specify output file for text mode.");
      ]

  | DocumentShouldBeAtStage1 ->
      report_error Interface [
        NormalLine("invalid stage designation for a document file; should be at stage 1.");
      ]

  | LoadHyph.InvalidPatternElement(rng) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("invalid string for hyphenation pattern.");
      ]

  | FontFormat.FailToLoadFontOwingToSystem(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("cannot load font file '" ^ fname ^ "';");
        DisplayLine(msg);
      ]

  | FontFormat.BrokenFont(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("font file '" ^ fname ^ "' is broken;");
        DisplayLine(msg);
      ]

  | FontFormat.CannotFindUnicodeCmap(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("font file '" ^ fname ^ "' does not have 'cmap' subtable for Unicode code points.");
      ]

  | FontInfo.InvalidFontAbbrev(abbrev) ->
      report_error Interface [
        NormalLine ("cannot find a font named '" ^ abbrev ^ "'.");
      ]

  | FontInfo.InvalidMathFontAbbrev(mfabbrev) ->
      report_error Interface [
        NormalLine("cannot find a math font named '" ^ mfabbrev ^ "'.");
      ]

  | FontInfo.NotASingleFont(abbrev, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("the font file '" ^ fname ^ "',");
        NormalLine("which is associated with the font name '" ^ abbrev ^ "',");
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

  | FontInfo.NotASingleMathFont(mfabbrev, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("the font file '" ^ fname ^ "',");
        NormalLine("which is associated with the math font name '" ^ mfabbrev ^ "',");
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

  | ImageHashTable.CannotLoadPdf(msg, abspath, pageno) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("cannot load PDF file '" ^ fname ^ "' page #" ^ (string_of_int pageno) ^ ";");
        DisplayLine(msg);
      ]

  | ImageHashTable.CannotLoadImage(msg, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("cannot load image file '" ^ fname ^ "';");
        DisplayLine(msg);
      ]

  | ImageHashTable.ImageOfWrongFileType(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("cannot load image file '" ^ fname ^ "';");
        DisplayLine("This file format is not supported.");
      ]

  | ImageHashTable.UnsupportedColorModel(_, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Interface [
        NormalLine("cannot load image file '" ^ fname ^ "';");
        DisplayLine("This color model is not supported.");
      ]

  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(s);
      ]

  | Parsing.Parse_error -> report_error Parser [ NormalLine("something is wrong."); ]
  | ParseErrorDetail(s) -> report_error Parser [ NormalLine(s); ]

  | IllegalArgumentLength(rng, len, lenexp) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this declaration has" ^ (string_of_int len) ^ " argument pattern(s),");
        NormalLine("but is expected to have " ^ (string_of_int lenexp) ^ ".");
      ]

  | ParserInterface.Error(rng) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
      ]

  | LoadMDSetting.MultipleCodeNameDesignation(rng, s) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("multiple designation for key '" ^ s ^ "'.");
      ]

  | LoadMDSetting.NotAnInlineCommand(rng, s) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ s ^ "' is not an inline command name.");
      ]

  | LoadMDSetting.NotABlockCommand(rng, s) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ s ^ "' is not a block command name.");
      ]

  | MyYojsonUtil.SyntaxError(fname, msg) ->
      report_error System [
        NormalLine("in '" ^ fname ^ "':");
        NormalLine(msg);
      ]

  | MyYojsonUtil.MultipleDesignation(rng, key) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("multiple designation for key \"" ^ key ^ "\".");
      ]

  | Yojson.SafePos.Util.Type_error(msg, (pos, _)) ->
      let rng = MyYojsonUtil.make_range pos in
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(msg);
      ]

  | MyYojsonUtil.MissingRequiredKey(rng, key) ->
      report_error System [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("missing required key '" ^ key ^ "'.");
      ]

  | Typechecker.UndefinedVariable(rng, mdlnmlst, varnm, candidates) ->
      let s = String.concat "." (List.append mdlnmlst [varnm]) in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined variable '" ^ s ^ "'.");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typechecker.UndefinedConstructor(rng, constrnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined constructor '" ^ constrnm ^ "'.");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typechecker.TooManyArgument(rngcmdapp, tyenv, tycmd) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine("too many argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ".")
      ]

  | Typechecker.NeedsMoreArgument(rngcmdapp, tyenv, tycmd, tyreq) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine("needs more mandatory argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ",");
        NormalLine("and another argument of type");
        DisplayLine(Display.string_of_mono_type tyenv tyreq);
        NormalLine("is needed.");
      ]

  | Typechecker.InvalidOptionalCommandArgument(tyenv, tycmd, rngarg) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngarg) ^ ":");
        NormalLine("invalid application of an optional argument;");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ".");
      ]

  | Typechecker.UnknownUnitOfLength(rng, unitnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined unit of length '" ^ unitnm ^ "'.");
      ]

  | Typechecker.HorzCommandInMath(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("an inline command is used as a math command.");
      ]

  | Typechecker.MathCommandInHorz(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("a math command is used as an inline command.");
      ]

  | Typechecker.BreaksValueRestriction(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this expression breaks the value restriction;");
        NormalLine("it should be a syntactic function.");
      ]

  | Typechecker.MultiplePatternVariable(rng1, rng2, varnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng1));
        NormalLine("and at " ^ (Range.to_string rng2) ^ ":");
        NormalLine("pattern variable '" ^ varnm ^ "' is bound more than once.");
      ]

  | Typechecker.MultipleFieldInRecord(rng, fldnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this record expression has more than one field for '" ^ fldnm ^ "'.");
      ]

  | Typechecker.InvalidExpressionAsToStaging(rng, stage) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("invalid expression as to stage;");
        NormalLine("should be used at " ^ (string_of_stage stage) ^ ".");
      ]

  | Typechecker.InvalidOccurrenceAsToStaging(rng, varnm, stage) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("invalid occurrence of variable '" ^ varnm ^ "' as to stage;");
        NormalLine("should be used at " ^ (string_of_stage stage) ^ ".");
      ]

  | Typechecker.ApplicationOfNonFunction(rng, tyenv, ty) ->
      let strty = string_of_mono_type tyenv ty in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this expression has type");
        DisplayLine(strty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]

  | Typeenv.IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ tynm ^ "' is expected to have " ^ (string_of_int lenexp) ^ " type argument(s),");
        NormalLine("but it has " ^ (string_of_int lenerr) ^ " type argument(s) here.");
      ]

  | Typeenv.UndefinedTypeName(rng, mdlnmlst, tynm, candidates) ->
      let s = String.concat "." (List.append mdlnmlst [tynm]) in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type name '" ^ s ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.UndefinedModuleName(rng, mdlnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined module name '" ^ mdlnm ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.UndefinedTypeArgument(rng, tyargnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type argument '" ^ tyargnm ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.CyclicTypeDefinition(reslist) ->
      report_error Typechecker (
        (NormalLine("cyclic synonym type definition:"))
        :: (List.map (fun (rng, strty) -> DisplayLine(strty ^ " (at " ^ (Range.to_string rng) ^ ")")) reslist)
      )

  | Typeenv.MultipleTypeDefinition(rng1, rng2, tynm) ->
      report_error Typechecker [
        NormalLine("parallel type definition by the same name:");
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng1) ^ ")");
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng2) ^ ")");
      ]

  | Typeenv.NotProvidingTypeImplementation(rng, tynm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("The implementation does not provide type '" ^ tynm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotProvidingValueImplementation(rng, varnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("The implementation does not provide value '" ^ varnm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotMatchingInterface(rng, varnm, tyenv1, pty1, tyenv2, pty2) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("The implementation of value '" ^ varnm ^ "' has type");
        DisplayLine(Display.string_of_poly_type tyenv1 pty1);
        NormalLine("which is inconsistent with the type required by the signature");
        DisplayLine(Display.string_of_poly_type tyenv2 pty2);
      ]

  | Typechecker.ContradictionError(tyenv, ((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = string_of_mono_type tyenv ty1 in
      let strty2 = string_of_mono_type tyenv ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true)   -> ("(cannot report position; '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')", strty1, strty2, [])
        | (true, false)  -> ("at " ^ strrng2 ^ ":", strty2, strty1, [])
        | (false, true)  -> ("at " ^ strrng1 ^ ":", strty1, strty2, [])
        | (false, false) -> ("at " ^ strrng1 ^ ":", strty1, strty2, [ NormalLine("This constraint is required by the expression");
                                                                      NormalLine("at " ^ strrng2 ^ "."); ])
      in
        report_error Typechecker (List.append [
          NormalLine(posmsg);
          NormalLine("this expression has type");
          DisplayLine(strtyA ^ ",");
          NormalLine("but is expected of type");
          DisplayLine(strtyB ^ ".");
        ] additional)

  | Typechecker.InclusionError(tyenv, ((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = string_of_mono_type tyenv ty1 in
      let strty2 = string_of_mono_type tyenv ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true)   -> ("(cannot report position; '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')", strty1, strty2, [])
        | (true, false)  -> ("at " ^ strrng2 ^ ":", strty2, strty1, [])
        | (false, true)  -> ("at " ^ strrng1 ^ ":", strty1, strty2, [])
        | (false, false) -> ("at " ^ strrng1 ^ ":", strty1, strty2, [ NormalLine("This constraint is required by the expression");
                                                                      NormalLine("at " ^ strrng2 ^ "."); ])
      in
        report_error Typechecker (List.append [
          NormalLine(posmsg);
          NormalLine("this expression has types");
          DisplayLine(strtyA);
          NormalLine("and");
          DisplayLine(strtyB);
          NormalLine("at the same time, but these are incompatible.");
        ] additional)

  | Evaluator.EvalError(s)
  | Vm.ExecError(s)
      -> report_error Evaluator [ NormalLine(s); ]

  | State.NotDuringPageBreak ->
      report_error Evaluator [
        NormalLine("a primitive as to PDF annotation was called before page breaking starts.");
      ]

  | Sys_error(s) ->
      report_error System [ NormalLine(s); ]


let arg_version () =
  print_string (
    "  SATySFi version 0.0.3\n"
(*
      ^ "  (in the middle of the transition from Macrodown)\n"
      ^ "    ____   ____       ________     _____   ______\n"
      ^ "    \\   \\  \\   \\     /   _____|   /   __| /      \\\n"
      ^ "     \\   \\  \\   \\   /   /        /   /   /   /\\   \\\n"
      ^ "     /    \\  \\   \\  \\   \\       /   /   /   /  \\   \\\n"
      ^ "    /      \\  \\   \\  \\   \\     /   /   /   /    \\   \\\n"
      ^ "   /   /\\   \\  \\   \\  \\   \\   /   /   /   /      \\   \\\n"
      ^ "  /   /  \\   \\  \\   \\  \\___\\ /___/   /   /        \\   \\\n"
      ^ " /   /    \\   \\  \\   \\              /   /_________/   /\n"
      ^ "/___/      \\___\\  \\___\\            /_________________/\n"
*)
  );
  exit 0


let arg_output curdir s =
  let abspathstr =
    if Filename.is_relative s then Filename.concat curdir s else s
  in
  OptionState.set_output_file (make_abs_path abspathstr)


let handle_anonimous_arg (curdir : string) (s : string) =
  let abspathstr =
    if Filename.is_relative s then Filename.concat curdir s else s
  in
  OptionState.set_input_file (make_abs_path abspathstr)


let text_mode s =
  let slst = String.split_on_char ',' s in
  OptionState.set_text_mode slst


let input_markdown setting =
  OptionState.set_input_kind (OptionState.Markdown(setting))


let arg_spec_list curdir =
  [
    ("-o"                , Arg.String(arg_output curdir)             , " Specify output file"              );
    ("--output"          , Arg.String(arg_output curdir)             , " Specify output file"              );
    ("-v"                , Arg.Unit(arg_version)                     , " Prints version"                   );
    ("--version"         , Arg.Unit(arg_version)                     , " Prints version"                   );
    ("--full-path"       , Arg.Unit(OptionState.set_show_full_path)  , " Displays paths in full-path style");
    ("--debug-show-bbox" , Arg.Unit(OptionState.set_debug_show_bbox) , " Outputs bounding boxes for glyphs");
    ("--debug-show-space", Arg.Unit(OptionState.set_debug_show_space), " Outputs boxes for spaces"         );
    ("-t"                , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"        );
    ("--type-check-only" , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"        );
    ("-b"                , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"            );
    ("--bytecomp"        , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"            );
    ("--text-mode"       , Arg.String(text_mode)                     , " Set text mode"                    );
    ("--markdown"        , Arg.String(input_markdown)                , " Pass Markdown source as input"    );
    ("--show-fonts"      , Arg.Unit(OptionState.set_show_fonts)      , " Displays all the available fonts" );
  ]


let setup_root_dirs () =
  let runtime_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "SATYSFI_RUNTIME" with
      | None    -> []
      | Some(s) -> [s]
    else
      ["/usr/local/share/satysfi"; "/usr/share/satysfi"]
  in
  let user_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "userprofile" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
    else
      match Sys.getenv_opt "HOME" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
  in
  let ds = List.append user_dirs runtime_dirs in
  match ds with
  | []     -> raise NoLibraryRootDesignation
  | _ :: _ -> Config.initialize ds


let () =
  error_log_environment (fun () ->
    setup_root_dirs ();
    let curdir = Sys.getcwd () in
    Arg.parse (arg_spec_list curdir) (handle_anonimous_arg curdir) "";
    let abspath_in =
      match OptionState.input_file () with
      | None    -> raise NoInputFileDesignation
      | Some(v) -> v
    in
    let abspathstr_in = get_abs_path_string abspath_in in
    let basename_without_extension =
      try Filename.chop_extension abspathstr_in with
      | Invalid_argument(_) -> abspathstr_in
    in
    let abspath_dump = make_abs_path (basename_without_extension ^ ".satysfi-aux") in
    let abspath_out =
      match OptionState.output_file () with
      | Some(v) ->
          v

      | None ->
          if OptionState.is_text_mode () then
            raise ShouldSpecifyOutputFile
          else
            make_abs_path (basename_without_extension ^ ".pdf")
    in
    Logging.target_file abspath_out;
    let (tyenv, env, dump_file_exists) = initialize abspath_dump in
    Logging.dump_file dump_file_exists abspath_dump;

    let dg = FileDependencyGraph.create 32 in
    begin
      match OptionState.get_input_kind () with
      | OptionState.SATySFi ->
          register_document_file dg abspath_in

      | OptionState.Markdown(setting) ->
          register_markdown_file dg setting abspath_in
    end;
    match FileDependencyGraph.find_cycle dg with
    | Some(cycle) ->
        raise (CyclicFileDependency(cycle))

    | None ->
        let input_list =
          FileDependencyGraph.backward_bfs_fold (fun inputacc abspath file_info ->
            Alist.extend inputacc (abspath, file_info)
          ) Alist.empty dg |> Alist.to_list
        in

      (* -- type checking -- *)
        let (_, astacc, docopt) =
          input_list |> List.fold_left (fun (tyenv, libacc, docopt) (abspath, file_info) ->
            match file_info with
            | DocumentFile(utast) ->
                let ast = typecheck_document_file tyenv abspath utast in
                (tyenv, libacc, Some(ast))

            | LibraryFile(stage, utast) ->
                let (tyenvnew, ast) = typecheck_library_file stage tyenv abspath utast in
                (tyenvnew, Alist.extend libacc (stage, abspath, ast), docopt)
          ) (tyenv, Alist.empty, None)
        in
        if OptionState.type_check_only () then
          ()
        else
          match docopt with
          | None         -> assert false
          | Some(astdoc) -> eval_abstract_tree_list env (Alist.to_list astacc) astdoc abspath_in abspath_out abspath_dump
  )
