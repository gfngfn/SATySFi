open Types
open Display

exception MainError of string

type line = NormalLine of string | DisplayLine of string

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

let show_control_sequence_type : bool ref = ref false
let show_function_type         : bool ref = ref false


let report_error (cat : error_category) (lines : line list) =
  let rec aux lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline ("    " ^ s)  ; aux tail end
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


let make_environment_from_header_file (tyenv : Typeenv.t) env file_name_in =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_progexpr () ;
        let utast = ParserInterface.process (Lexing.from_channel file_in) in
        let (ty, newtyenv, ast) = Typechecker.main tyenv utast in
          begin
            print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) ;
            let evaled = Evaluator.interpret env ast in
              match evaled with
              | EvaluatedEnvironment(newenv) ->
                  begin
                    ( if !show_control_sequence_type then
                        if !show_function_type then
                          (* print_endline (Typeenv.string_of_type_environment newtyenv "Environment") *) ()
                        else
                          (* print_endline (Typeenv.string_of_control_sequence_type newtyenv) *) ()
                      else () ) ;
                    (newtyenv, newenv)
                  end
              | _ -> raise (MainError("'" ^ file_name_in ^ "' is not a header file"))
          end
      end
  end

(*n
let read_standalone_file (tyenv : Typeenv.t) env file_name_in file_name_out =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_progexpr () ;
        let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
        let (ty, _, ast) = Typechecker.main tyenv utast in
        let () = print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) in
          match ty with
          | (_, BaseType(TextColType)) ->
              let astfinal = VertLex(UninitializedContext, ast) in
              let valuefinal = Evaluator.interpret env astfinal in
              begin
                match valuefinal with
                | Vert(imvblst) ->
                    let pdf = HandlePdf.create_empty_pdf file_name_out in
                    begin
                      PageBreak.main pdf imvblst;
                      print_endline (" ---- ---- ---- ----");
                      print_endline ("  output written on '" ^ file_name_out ^ "'.");
                    end
                | _ -> failwith "main; not a Vert(_)"
              end
          | _  -> raise (MainError("the output of '" ^ file_name_in ^ "' is not text-col; it's " ^ (string_of_mono_type tyenv ty) ^ "."))
      end
  end
*)

let read_document_file (tyenv : Typeenv.t) env file_name_in file_name_out =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_progexpr () ;
        let () = PrintForDebug.mainE "END INITIALIZATION" in  (* for debug *)
        let utast = ParserInterface.process (Lexing.from_channel file_in) in
        let () = PrintForDebug.mainE "END PARSING" in  (* for debug *)
        let (ty, _, ast) = Typechecker.main tyenv utast in
        let () = PrintForDebug.mainE "END TYPE CHECKING" in  (* for debug *)
        let () = print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) in
          match ty with
          | (_, BaseType(DocumentType)) ->
(*
              let pagesch =
                HorzBox.({
                  page_size        = A4Paper;
                  left_page_margin = Length.of_pdf_point 100.;
                  top_page_margin  = Length.of_pdf_point 100.;
                  area_width       = Length.of_pdf_point 400.;
                  area_height      = Length.of_pdf_point 650.;
                })  (* temporary *)
              in
*)
              let valuedoc = Evaluator.interpret env ast in
              begin
                match valuedoc with
                | DocumentValue(ctxdoc, imvblst) ->
                    let pdf = HandlePdf.create_empty_pdf file_name_out in
                    begin
                      print_endline (" ---- ---- ---- ----");
                      print_endline ("  breaking contents into pages ...");
                      PageBreak.main pdf ctxdoc.HorzBox.page_scheme imvblst;
                      print_endline ("  output written on '" ^ file_name_out ^ "'.");
                    end
                | _ -> failwith "main; not a Vert(_)"
              end
          | _  -> raise (MainError("the output of '" ^ file_name_in ^ "' is not text-col; it's " ^ (string_of_mono_type tyenv ty) ^ "."))
(*
          begin
            print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) ;
            match ty with
            | (_, BaseType(StringType)) ->  (* temporary; will be modified to BaseType(BoxColType) *)
                let evaled = Evaluator.interpret env ast in
                let content_out = Out.main evaled in
                if (String.length content_out) = 0 then
                  begin
                    print_endline " ---- ---- ---- ----" ;
                    print_endline "  no output."
                  end
                else
                  begin
                    Files.file_out_of_string file_name_out content_out ;
                    print_endline " ---- ---- ---- ----" ;
                    print_endline ("  output written on '" ^ file_name_out ^ "'.")
                  end
            | _ -> raise (MainError("the output of '" ^ file_name_in ^ "' is not string"))
          end
*)
      end
  end


let error_log_environment suspended =
  try
    suspended ()
  with
  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(s);
      ]

  | Parsing.Parse_error             -> report_error Parser [ NormalLine("something is wrong."); ]
  | ParseErrorDetail(s)             -> report_error Parser [ NormalLine(s); ]

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

  | Evaluator.EvalError(s)          -> report_error Evaluator [ NormalLine(s); ]
  | MainError(s)                    -> report_error Interface [ NormalLine(s); ]
  | Sys_error(s)                    -> report_error System    [ NormalLine(s); ]
(*
  | FontFormat.FontFormatBroken(e)  -> Otfm.pp_error Format.std_formatter e
*)

type input_file_kind =
  | DocumentFile
  | HeaderFile
(*
  | StandaloneFile
*)

let rec main (tyenv : Typeenv.t) (env : environment) (input_list : (input_file_kind * string) list) (file_name_out : string) =
    match input_list with
    | [] ->
        begin
          print_endline " ---- ---- ---- ----" ;
          print_endline "  no output."
        end
    | (DocumentFile, file_name_in) :: tail ->
          read_document_file tyenv env file_name_in file_name_out

    | (HeaderFile, file_name_in) :: tail ->
          let (newtyenv, newenv) = make_environment_from_header_file tyenv env file_name_in in
            main newtyenv newenv tail file_name_out
(*
    | (StandaloneFile, file_name_in) :: tail ->
          read_standalone_file tyenv env file_name_in file_name_out
*)

let libdir_ref : string ref = ref "/usr/local/lib-satysfi"
let output_name_ref : string ref = ref "saty.out"
let input_acc_ref : ((input_file_kind * string) list) ref = ref []

let arg_output s =
  begin output_name_ref := s; end

let arg_header s =
  begin input_acc_ref := (HeaderFile, s) :: !input_acc_ref; end
  
let arg_doc s =
  begin input_acc_ref := (DocumentFile, s) :: !input_acc_ref; end
(*
let arg_standalone s =
  begin input_acc_ref := (StandaloneFile, s) :: !input_acc_ref; end
*)
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
(*
    ("--standalone", Arg.String(arg_standalone), " Specify input file as a standalone file");
*)
  ]

let handle_anonimous_arg s =
  let i =
    match () with
    | ()  when is_document_file s   -> (DocumentFile, s)
    | ()  when is_header_file s     -> (HeaderFile, s)
(*
    | ()  when is_standalone_file s -> (StandaloneFile, s)
*)
    | _                             -> raise (MainError("File '" ^ s ^ "' has illegal filename extension; maybe you need to use '--doc', '--standalone', or '--header' option."))
  in
  input_acc_ref := i :: (!input_acc_ref)


let () =
  error_log_environment (fun () ->
    Arg.parse arg_spec_list handle_anonimous_arg "";
    FreeID.initialize ();
    BoundID.initialize ();
    TypeID.initialize ();
    Typeenv.initialize_id ();
    EvalVarID.initialize ();
    let libdir = !libdir_ref in
    FontInfo.initialize libdir;  (* temporary *)
    ImageInfo.initialize ();
    let (tyenv, env) = Primitives.make_environments () in
    let input_list = List.rev (!input_acc_ref) in
    let output = !output_name_ref in
    input_list |> List.iter (fun (_, s) -> print_endline ("  [input] " ^ s));
    print_endline ("  [output] " ^ output);
    main tyenv env input_list output
  )
