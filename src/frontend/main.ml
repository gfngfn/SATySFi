open Types
open Display


type file_path = string


exception IllegalExtension of file_path
exception NotAHeaderFile   of file_path * Typeenv.t * mono_type
exception NotADocumentFile of file_path * Typeenv.t * mono_type


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
  let rec aux lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline ("    " ^ s) ; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("      " ^ s); aux tail end
  in
  let first lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline s; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("\n      " ^ s); aux tail end
  in
  begin
    print_string ("! [" ^ (show_error_category cat) ^ "] ");
    first lines;
    exit 1;
  end


let is_suffix pfx str =
  let pfxlen = String.length pfx in
  let strlen = String.length str in
    if strlen < pfxlen then false else
      (compare pfx (String.sub str (strlen - pfxlen) pfxlen)) = 0


let is_document_file   = is_suffix ".saty"
let is_header_file     = is_suffix ".satyh"
let is_standalone_file = is_suffix ".satys"


let make_environment_from_header_file (tyenv : Typeenv.t) (env : environment) (file_name_in : file_path) : Typeenv.t * environment =
  begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  reading '" ^ file_name_in ^ "' ...");
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_progexpr ();
        let utast = ParserInterface.process (Lexing.from_channel file_in) in
        let (ty, tyenvnew, ast) = Typechecker.main tyenv utast in
          begin
            print_endline ("  type check: " ^ (string_of_mono_type tyenv ty));
            let value = Evaluator.interpret env ast in
              match value with
              | EvaluatedEnvironment(envnew) -> (tyenvnew, envnew)
              | _                            -> raise (NotAHeaderFile(file_name_in, tyenvnew, ty))
          end
      end
  end


let libdir_ref : file_path ref = ref "/usr/local/lib-satysfi"


(* -- initialization that should be performed before every cross-reference-solving loop -- *)
let reset () =
  begin
(*
    StoreID.reset ();
*)
    FontInfo.initialize (!libdir_ref);
    ImageInfo.initialize ();
  end


(* -- initialization that should be performed before typechecking -- *)
let initialize (dumpfile : file_path) : bool =
  begin
    FreeID.initialize ();
    BoundID.initialize ();
    TypeID.initialize ();
    Typeenv.initialize_id ();
    EvalVarID.initialize ();
    StoreID.initialize ();
    let dump_file_exists = CrossRef.initialize dumpfile in
    dump_file_exists
  end


let output_pdf file_name_out pagesch pagelst =
  let pdf = HandlePdf.create_empty_pdf file_name_out in
  let pdfret =
    List.fold_left (fun pdf evvblstpage ->
      pdf |> HandlePdf.write_page pagesch evvblstpage
    ) pdf pagelst
  in
  HandlePdf.write_to_file pdfret


let ordinal i =
  let suffix =
    match i mod 10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"
  in
  (string_of_int i) ^ suffix


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


let read_document_file (tyenv : Typeenv.t) (env : environment) (file_name_in : file_path) (file_name_out : file_path) (dumpfile : file_path) =
  begin
    print_endline (" ---- ---- ---- ----");
    print_endline ("  reading '" ^ file_name_in ^ "' ...");
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_progexpr ();
        let () = PrintForDebug.mainE "END INITIALIZATION" in  (* for debug *)
        let utast = ParserInterface.process (Lexing.from_channel file_in) in
(*
        Format.printf "Main> %a\n" pp_untyped_abstract_tree utast;  (* for debug *)
*)
        let () = PrintForDebug.mainE "END PARSING" in  (* for debug *)
        let (ty, _, ast) = Typechecker.main tyenv utast in
(*
        Format.printf "Main> %a\n" pp_abstract_tree ast;  (* for debug *)
*)
        let () = PrintForDebug.mainE "END TYPE CHECKING" in  (* for debug *)
        let () = print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) in
        let env_freezed = freeze_environment env in
          match ty with
          | (_, BaseType(DocumentType)) ->
(*
              StoreID.set ();
*)
              let rec aux i =
                print_endline ("  begin to evaluate the document (" ^ (ordinal i) ^ " trial) ...");
                reset ();
                let env = unfreeze_environment env_freezed in
                let valuedoc = Evaluator.interpret env ast in
                print_endline ("  evaluation done.");
                begin
                  match valuedoc with
                  | DocumentValue(ctxdoc, imvblst) ->
                      print_endline (" ---- ---- ---- ----");
                      print_endline ("  breaking contents into pages ...");
                      let pagesch = ctxdoc.HorzBox.page_scheme in
                      let pagelst = PageBreak.main pagesch imvblst in
                      begin
                        match CrossRef.needs_another_trial dumpfile with
                        | CrossRef.NeedsAnotherTrial ->
                            print_endline ("  needs another trial for solving cross references...");
                            aux (i + 1)

                        | CrossRef.CountMax ->
                            begin
                              print_endline ("  some cross references were not solved.");
                              output_pdf file_name_out pagesch pagelst;
                              print_endline (" ---- ---- ---- ----");
                              print_endline ("  output written on '" ^ file_name_out ^ "'.");
                            end

                        | CrossRef.CanTerminate ->
                            begin
                              print_endline ("  all cross references were solved.");
                              output_pdf file_name_out pagesch pagelst;
                              print_endline (" ---- ---- ---- ----");
                              print_endline ("  output written on '" ^ file_name_out ^ "'.");
                            end
                      end

                  | _ -> failwith "main; not a DocumentValue(...)"
                end
              in
                aux 1

          | _  -> raise (NotADocumentFile(file_name_in, tyenv, ty))
      end
  end


let error_log_environment suspended =
  try
    suspended ()
  with
  | IllegalExtension(s) ->
      report_error Interface [
        NormalLine("File '" ^ s ^ "' has illegal filename extension;");
        NormalLine("maybe you need to use '--doc', or '--header' option.");
      ]

  | NotAHeaderFile(file_name_in, tyenv, ty) ->
      report_error Typechecker [
        NormalLine("file '" ^ file_name_in ^ "' is not a header file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | NotADocumentFile(file_name_in, tyenv, ty) ->
      report_error Typechecker [
        NormalLine("file '" ^ file_name_in ^ "' is not a document file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | CrossRef.InvalidYOJSON(dumpfile, msg) ->
      report_error Interface [
        NormalLine("dump file '" ^ dumpfile ^ "' is NOT a valid YOJSON file:");
        DisplayLine(msg);
      ]

  | CrossRef.DumpFileOtherThanAssoc(dumpfile) ->
      report_error Interface [
        NormalLine("the content of the dump file '" ^ dumpfile ^ "' is NOT a dictionary.");
      ]

  | CrossRef.DumpFileValueOtherThanString(dumpfile, key, jsonstr) ->
      report_error Interface [
        NormalLine("the value corresponding to the key '" ^ key ^ "' in the dump file '" ^ dumpfile ^ "' is NOT a string;");
        DisplayLine(jsonstr);
      ]

  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(s);
      ]

  | Parsing.Parse_error             -> report_error Parser [ NormalLine("something is wrong."); ]
  | ParseErrorDetail(s)             -> report_error Parser [ NormalLine(s); ]

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

  | Typechecker.UndefinedVariable(rng, varnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined variable '" ^ varnm ^ "'.");
      ]

  | Typechecker.UndefinedConstructor(rng, constrnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined constructor '" ^ constrnm ^ "'.");
      ]

  | Typechecker.InvalidArityOfCommand(rng, lenreq, lenreal) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("this command expects " ^ (string_of_int lenreq) ^ " argument(s),");
        NormalLine("but here is applied to " ^ (string_of_int lenreal) ^ " argument(s).");
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

  | Typeenv.IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ tynm ^ "' is expected to have " ^ (string_of_int lenexp) ^ " type argument(s),");
        NormalLine("but it has " ^ (string_of_int lenerr) ^ " type argument(s) here.");
      ]

  | Typeenv.UndefinedTypeName(rng, tynm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type name '" ^ tynm ^ "'");
      ]

  | Typeenv.UndefinedModuleName(rng, mdlnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined module name '" ^ mdlnm ^ "'");
      ]

  | Typeenv.UndefinedTypeArgument(rng, tyargnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type argument '" ^ tyargnm ^ "'");
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

  | Evaluator.EvalError(s) -> report_error Evaluator [ NormalLine(s); ]

  | Sys_error(s) -> report_error System    [ NormalLine(s); ]
(*
  | FontFormat.FontFormatBroken(e)  -> Otfm.pp_error Format.std_formatter e
*)

type input_file_kind =
  | DocumentFile
  | HeaderFile


let rec main (tyenv : Typeenv.t) (env : environment) (input_list : (input_file_kind * string) list) (file_name_out : file_path) (dump_file_name : file_path) =
    match input_list with
    | [] ->
        begin
          print_endline " ---- ---- ---- ----";
          print_endline "  no output.";
        end

    | (DocumentFile, file_name_in) :: tail ->
        read_document_file tyenv env file_name_in file_name_out dump_file_name

    | (HeaderFile, file_name_in) :: tail ->
        let (tyenvnew, envnew) = make_environment_from_header_file tyenv env file_name_in in
        main tyenvnew envnew tail file_name_out dump_file_name


let output_name_ref : file_path ref = ref "a.pdf"
let input_acc_ref : ((input_file_kind * string) Alist.t) ref = ref Alist.empty


let arg_output s =
  begin output_name_ref := s; end

let arg_header s =
  begin input_acc_ref := Alist.extend (!input_acc_ref) (HeaderFile, s); end

let arg_doc s =
  begin input_acc_ref := Alist.extend (!input_acc_ref) (DocumentFile, s); end

let arg_libdir s =
  begin libdir_ref := s; end


let arg_version () =
  begin
    print_string (
        "  SATySFi version 0.00\n"
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
    );
    exit 0;
  end

let arg_spec_list =
  [
    ("--libdir"    , Arg.String(arg_libdir)    , " Specify SATySFi library directory");
    ("-o"          , Arg.String(arg_output)    , " Specify output file");
    ("--output"    , Arg.String(arg_output)    , " Specify output file");
    ("-v"          , Arg.Unit(arg_version)     , " Print version");
    ("--version"   , Arg.Unit(arg_version)     , " Print version");
    ("--header"    , Arg.String(arg_header)    , " Specify input file as a header");
    ("--doc"       , Arg.String(arg_doc)       , " Specify input file as a document file");
  ]

let handle_anonimous_arg s =
  let i =
    match () with
    | ()  when is_document_file s -> (DocumentFile, s)
    | ()  when is_header_file s   -> (HeaderFile, s)
    | _                           -> raise (IllegalExtension(s))
  in
  input_acc_ref := Alist.extend (!input_acc_ref) i


let () =
  error_log_environment (fun () ->
    Arg.parse arg_spec_list handle_anonimous_arg "";
    let input_list = Alist.to_list (!input_acc_ref) in
    let output = !output_name_ref in
    input_list |> List.iter (fun (_, s) -> print_endline ("  [input] " ^ s));
    print_endline ("  [output] " ^ output);
    let dumpfile = (Filename.remove_extension output) ^ ".satysfi-aux" in
    let dump_file_exists = initialize dumpfile in
    begin
    if dump_file_exists then
      print_endline ("  [dump] " ^ dumpfile ^ " (already exists)")
    else
      print_endline ("  [dump] " ^ dumpfile ^ " (will be created)")
    end;
    let (tyenv, env) = Primitives.make_environments () in
(*
    (* begin: for debug *)
    Format.printf "Main> ==== ====\n";
    let () =
      let (valenv, _) = env in
      EvalVarIDMap.iter (fun evid _ -> Format.printf "Main> %s\n" (EvalVarID.show_direct evid)) valenv
    in
    Format.printf "Main> ==== ====\n";
    (* end: for debug *)
*)
    main tyenv env input_list output dumpfile
  )
