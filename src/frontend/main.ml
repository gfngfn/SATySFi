
open MyUtil
open Types
open Display
open StaticEnv


exception NoLibraryRootDesignation
exception NoInputFileDesignation
exception CyclicFileDependency        of abs_path list
exception CannotReadFileOwingToSystem of string
(*
exception NotALibraryFile             of abs_path * Typeenv.t * mono_type
*)
exception NotADocumentFile            of abs_path * Typeenv.t * mono_type
exception NotAStringFile              of abs_path * Typeenv.t * mono_type
exception LibraryContainsWholeReturnValue of abs_path
exception DocumentLacksWholeReturnValue   of abs_path
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
  | LibraryFile  of stage * (module_name ranged * untyped_binding list)


let has_library_extension abspath =
  let ext = get_abs_path_extension abspath in
  match ext with
  | ".satyh" | ".satyg" ->
      true

  | _ ->
      begin
        try
          let extpre = String.sub ext 0 7 in
          String.equal extpre ".satyh-"
        with
        | _ -> false
      end


let get_candidate_file_extensions () =
  match OptionState.get_mode () with
  | None      -> [".satyh"; ".satyg"]
  | Some(lst) -> List.append (lst |> List.map (fun s -> ".satyh-" ^ s)) [".satyg"]


let get_package_abs_path package =
  let extcands = get_candidate_file_extensions () in
  Config.resolve_package_exn package extcands


let get_abs_path_of_header curdir headerelem =
  match headerelem with
  | HeaderRequire(package) ->
      get_package_abs_path package

  | HeaderImport(s) ->
      let extcands = get_candidate_file_extensions () in
      Config.resolve_local_exn curdir s extcands


let rec register_library_file (dg : file_info FileDependencyGraph.t) (abspath : abs_path) : unit =
  begin
    Logging.begin_to_parse_file abspath;
    let curdir = Filename.dirname (get_abs_path_string abspath) in
    let inc = open_in_abs abspath in
    let (stage, header, utsrc) = ParserInterface.process (basename_abs abspath) (Lexing.from_channel inc) in
    close_in inc;
    let lib =
      match utsrc with
      | UTLibraryFile(lib) -> lib
      | UTDocumentFile(_)  -> raise (LibraryContainsWholeReturnValue(abspath))
    in
    FileDependencyGraph.add_vertex dg abspath (LibraryFile(stage, lib));
    header |> List.iter (fun headerelem ->
      let abspath_sub = get_abs_path_of_header curdir headerelem in
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
  let (stage, header, utsrc) =
    ParserInterface.process (Filename.basename (get_abs_path_string abspath_in)) (Lexing.from_channel file_in)
  in
  let utast =
    match (stage, utsrc) with
    | (_, UTLibraryFile(_))           -> raise (DocumentLacksWholeReturnValue(abspath_in))
    | (Stage1, UTDocumentFile(utast)) -> utast
    | (_, UTDocumentFile(_))          -> raise DocumentShouldBeAtStage1
  in
  FileDependencyGraph.add_vertex dg abspath_in (DocumentFile(utast));
  header |> List.iter (fun headerelem ->
    let file_path_sub = get_abs_path_of_header curdir headerelem in
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
        let file_path_sub = get_package_abs_path package in
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


let typecheck_library_file (stage : stage) (tyenv : Typeenv.t) (abspath_in : abs_path) (utbinds : untyped_binding list) : Typeenv.t * binding list =
  Logging.begin_to_typecheck_file abspath_in;
  let (binds, tyenv) = Typechecker.main_bindings stage tyenv utbinds in
  Logging.pass_type_check None;
  (tyenv, binds)


let typecheck_document_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree =
  Logging.begin_to_typecheck_file abspath_in;
  let (ty, ast) = Typechecker.main Stage1 tyenv utast in
  Logging.pass_type_check (Some(Display.string_of_mono_type ty));
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


let eval_library_file (env : environment) (abspath : abs_path) (binds : binding list) : environment =
  Logging.begin_to_eval_file abspath;
  if OptionState.bytecomp_mode () then
    Bytecomp.compile_and_exec_bindings_0 env binds
  else
    Evaluator.interpret_bindings_0 env binds


let preprocess_library_file (env : environment) (abspath : abs_path) (binds : binding list) : code_binding list * environment =
  Logging.begin_to_preprocess_file abspath;
  if OptionState.bytecomp_mode () then
    Bytecomp.compile_and_exec_bindings_1 env binds
  else
    Evaluator.interpret_bindings_1 env binds


let preprocess_file ?(is_document : bool = false) (env : environment) (abspath : abs_path) (ast : abstract_tree) : code_value * environment =
  Logging.begin_to_preprocess_file abspath;
  let (cd, envopt) =
    if OptionState.bytecomp_mode () then
      Bytecomp.compile_and_exec_1 env ast
    else
      Evaluator.interpret_1 env ast
  in
    if is_document then
      match envopt with
      | None    -> (cd, env)
      | Some(_) -> EvalUtil.report_bug_vm "environment returned for document"
    else
      match envopt with
      | Some(envnew) -> (cd, envnew)
      | None         -> EvalUtil.report_bug_vm "environment not returned"


let eval_main i env_freezed ast =
  Logging.start_evaluation i;
  reset ();
  let env = unfreeze_environment env_freezed in
  let (valuedoc, _) =
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
      | BaseConstant(BCDocument(pagesize, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
          Logging.start_page_break ();
          State.start_page_break ();
          let pdf =
            match pbstyle with
            | SingleColumn ->
                PageBreak.main abspath_out pagesize
                  columnhookf pagecontf pagepartsf imvblst

            | MultiColumn(origin_shifts) ->
                PageBreak.main_multicolumn abspath_out pagesize
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
          EvalUtil.report_bug_value "main; not a DocumentValue(...)" valuedoc
    in
    aux 1


let eval_abstract_tree_list (env : environment) (libs : (stage * abs_path * binding list) list) (ast_doc : abstract_tree) (abspath_in : abs_path) (abspath_out : abs_path) (abspath_dump : abs_path) =

  let rec preprocess (codeacc : (abs_path * code_binding list) Alist.t) (env : environment) libs =
    match libs with
    | [] ->
        let (codedoc, env) = preprocess_file ~is_document:true env abspath_in ast_doc in
        (env, Alist.to_list codeacc, codedoc)

    | ((Stage0 | Persistent0), abspath, binds0) :: tail ->
        let envnew = eval_library_file env abspath binds0 in
        preprocess codeacc envnew tail

    | (Stage1, abspath, binds1) :: tail ->
        let (codebinds, envnew) = preprocess_library_file env abspath binds1 in
        preprocess (Alist.extend codeacc (abspath, codebinds)) envnew tail
  in
    (* --
       each evaluation called in `preprocess` is run by the naive interpreter
       regardless of whether `--bytecomp` was specified.
       -- *)

  let rec eval (env : environment) (codebinds : (abs_path * code_binding list) list) : environment =
    match codebinds with
    | [] ->
        env

    | (abspath, codebinds) :: tail ->
        let ast = unlift_code_bindings codebinds in
        let env = eval_library_file env abspath ast in
        eval env tail
  in
  let (env, codebinds, codedoc) = preprocess Alist.empty env libs in
  let env = eval env codebinds in
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
        NormalLine("set appropriate environment variables");
        NormalLine("or specify configuration search paths with -C option.");
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

  | Config.ImportedFileNotFound(s, pathcands) ->
      report_error Interface (List.append [
        NormalLine("imported file not found:");
        DisplayLine(s);
        NormalLine("candidate paths:");
      ] (pathcands |> List.map (fun abspath -> DisplayLine(get_abs_path_string abspath))))
(*
  | NotALibraryFile(abspath, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a header file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]
*)
  | NotADocumentFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a document file; it is of type");
        DisplayLine(string_of_mono_type ty);
      ]

  | NotAStringFile(abspath_in, tyenv, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      report_error Typechecker [
        NormalLine("file '" ^ fname ^ "' is not a file for generating text; it is of type");
        DisplayLine(string_of_mono_type ty);
      ]

  | ShouldSpecifyOutputFile ->
      report_error Interface [
        NormalLine("should specify output file for text mode.");
      ]

  | DocumentShouldBeAtStage1 ->
      report_error Interface [
        NormalLine("invalid stage designation for a document file; should be at stage 1.");
      ]

  | InvalidDependencyAsToStaging(abspath1, stage1, abspath2, stage2) ->
      let fname1 = convert_abs_path_to_show abspath1 in
      let fname2 = convert_abs_path_to_show abspath2 in
      report_error Interface [
        NormalLine("invalid dependency as to stage:");
        NormalLine("'" ^ fname1 ^ "' (at " ^ (string_of_stage stage1) ^ ") depends on '" ^ fname2 ^ "' (at " ^ (string_of_stage stage2) ^ ")");
      ]

  | CannotReadFileOwingToSystem(msg) ->
      report_error Interface [
        NormalLine("cannot read file:");
        DisplayLine(msg);
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

  | IllegalArgumentLength(rng, len, lenexp) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this declaration has " ^ (string_of_int len) ^ " argument pattern(s),");
        NormalLine("but is expected to have " ^ (string_of_int lenexp) ^ ".");
      ]

  | ParserInterface.Error(rng) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
      ]

  | ParseErrorDetail(rng, s) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(s)
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

  | Typechecker.UndefinedHorzMacro(rng, csnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined inline macro '" ^ csnm ^ "'.");
      ]

  | Typechecker.UndefinedVertMacro(rng, csnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined block macro '" ^ csnm ^ "'.");
      ]

  | Typechecker.InvalidNumberOfMacroArguments(rng, macparamtys) ->
      report_error Typechecker (List.append [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("invalid number of macro arguments; types expected on arguments are:");
      ] (macparamtys |> List.map (function
        | LateMacroParameter(ty)  -> DisplayLine("* " ^ (Display.string_of_mono_type ty))
        | EarlyMacroParameter(ty) -> DisplayLine("* ~" ^ (Display.string_of_mono_type ty))
      )))

  | Typechecker.LateMacroArgumentExpected(rng, ty) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("an early macro argument is given, but a late argument of type");
        DisplayLine(Display.string_of_mono_type ty);
        NormalLine("is expected.");
      ]

  | Typechecker.EarlyMacroArgumentExpected(rng, ty) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("a late macro argument is given, but an early argument of type");
        DisplayLine(Display.string_of_mono_type ty);
        NormalLine("is expected.");
      ]

  | Typechecker.TooManyArgument(rngcmdapp, tycmd) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine("too many argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tycmd) ^ ".")
      ]

  | Typechecker.NeedsMoreArgument(rngcmdapp, tycmd, tyreq) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine("needs more mandatory argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tycmd) ^ ",");
        NormalLine("and another argument of type");
        DisplayLine(Display.string_of_mono_type tyreq);
        NormalLine("is needed.");
      ]

  | Typechecker.InvalidOptionalCommandArgument(tycmd, rngarg) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngarg) ^ ":");
        NormalLine("invalid application of an optional argument;");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tycmd) ^ ".");
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

  | Typechecker.ApplicationOfNonFunction(rng, ty) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this expression has type");
        DisplayLine(Display.string_of_mono_type ty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]


  | Typechecker.MultiCharacterMathScriptWithoutBrace(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("more than one character is used as a math sub/superscript without braces;");
        NormalLine("use braces for making association explicit.");
      ]

  | Typechecker.IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ tynm ^ "' is expected to have " ^ (string_of_int lenexp) ^ " type argument(s),");
        NormalLine("but it has " ^ (string_of_int lenerr) ^ " type argument(s) here.");
      ]
(*
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
*)
  | Typechecker.ContradictionError(((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = Display.string_of_mono_type ty1 in
      let strty2 = Display.string_of_mono_type ty2 in
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

  | Typechecker.InclusionError(((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = Display.string_of_mono_type ty1 in
      let strty2 = Display.string_of_mono_type ty2 in
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

  | PageBreak.PageNumberLimitExceeded(m) ->
      report_error Evaluator [
        NormalLine(Printf.sprintf "page number limit (= %d) exceeded." m);
        NormalLine(Printf.sprintf "If you really want to output more than %d pages, use '--page-number-limit'." m);
      ]

  | Sys_error(s) ->
      report_error System [ NormalLine(s); ]


let arg_version () =
  print_string (
    "  SATySFi version 0.0.6\n"
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


let handle_anonymous_arg (curdir : string) (s : string) =
  let abspathstr =
    if Filename.is_relative s then Filename.concat curdir s else s
  in
  OptionState.set_input_file (make_abs_path abspathstr)


let text_mode s =
  let slst = String.split_on_char ',' s in
  OptionState.set_text_mode slst


let input_markdown setting =
  OptionState.set_input_kind (OptionState.Markdown(setting))


let arg_config s =
  let paths = String.split_on_char ':' s in
  OptionState.set_extra_config_paths paths


let arg_spec_list curdir =
  [
    ("-o"                , Arg.String(arg_output curdir)             , " Specify output file"                                   );
    ("--output"          , Arg.String(arg_output curdir)             , " Specify output file"                                   );
    ("-v"                , Arg.Unit(arg_version)                     , " Prints version"                                        );
    ("--version"         , Arg.Unit(arg_version)                     , " Prints version"                                        );
    ("--full-path"       , Arg.Unit(OptionState.set_show_full_path)  , " Displays paths in full-path style"                     );
    ("--debug-show-bbox" , Arg.Unit(OptionState.set_debug_show_bbox) , " Outputs bounding boxes for glyphs"                     );
    ("--debug-show-space", Arg.Unit(OptionState.set_debug_show_space), " Outputs boxes for spaces"                              );
    ("--debug-show-block-bbox", Arg.Unit(OptionState.set_debug_show_block_bbox), " Outputs bounding boxes for blocks"           );
    ("--debug-show-block-space", Arg.Unit(OptionState.set_debug_show_block_space), " Outputs visualized block spaces"           );
    ("--debug-show-overfull", Arg.Unit(OptionState.set_debug_show_overfull), " Outputs visualized overfull or underfull lines"  );
    ("-t"                , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"                             );
    ("--type-check-only" , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"                             );
    ("-b"                , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"                                 );
    ("--bytecomp"        , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"                                 );
    ("--text-mode"       , Arg.String(text_mode)                     , " Set text mode"                                         );
    ("--markdown"        , Arg.String(input_markdown)                , " Pass Markdown source as input"                         );
    ("--show-fonts"      , Arg.Unit(OptionState.set_show_fonts)      , " Displays all the available fonts"                      );
    ("-C"                , Arg.String(arg_config)                    , " Add colon-separated paths to configuration search path");
    ("--config"          , Arg.String(arg_config)                    , " Add colon-separated paths to configuration search path");
    ("--no-default-config", Arg.Unit(OptionState.set_no_default_config_paths), " Does not use default configuration search path");
    ("--page-number-limit", Arg.Int(OptionState.set_page_number_limit), " Set the page number limit (default: 10000)"           );
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
  let home_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "userprofile" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
    else
      match Sys.getenv_opt "HOME" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
  in
  let default_dirs =
    if OptionState.get_no_default_config_paths () then
      []
    else
      List.concat [home_dirs; runtime_dirs]
  in
  let extra_dirs =
    match OptionState.get_extra_config_paths () with
    | None -> [Filename.concat (Sys.getcwd ()) ".satysfi"]
    | Some(lst) -> lst
  in
  let ds = List.append extra_dirs default_dirs in
  match ds with
  | []     -> raise NoLibraryRootDesignation
  | _ :: _ -> Config.initialize ds


let main () =
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in
    Arg.parse (arg_spec_list curdir) (handle_anonymous_arg curdir) "";
    setup_root_dirs ();
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
          if has_library_extension abspath_in && OptionState.type_check_only () then
            register_library_file dg abspath_in
          else
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
        let (_, libacc, ast_opt) =
          input_list |> List.fold_left (fun (tyenv, libacc, docopt) (abspath, file_info) ->
            match file_info with
            | DocumentFile(utast) ->
                let ast = typecheck_document_file tyenv abspath utast in
                (tyenv, libacc, Some(ast))

            | LibraryFile((stage, (_modident, utbinds))) ->
                (* TODO: use `modident` to divide namespaces *)
                let (tyenv, binds) = typecheck_library_file stage tyenv abspath utbinds in
                (tyenv, Alist.extend libacc (stage, abspath, binds), docopt)
          ) (tyenv, Alist.empty, None)
        in
        if OptionState.type_check_only () then
          ()
        else
          match ast_opt with
          | None      -> assert false
          | Some(ast) -> eval_abstract_tree_list env (Alist.to_list libacc) ast abspath_in abspath_out abspath_dump
  )
