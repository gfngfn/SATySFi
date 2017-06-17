open Types
open Display

exception MainError of string

type line = NormalLine of string | DisplayLine of string


let show_control_sequence_type : bool ref = ref false
let show_function_type         : bool ref = ref false


let report_error (category : string) (lines : line list) =
  let rec aux lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline ("    " ^ s)   ; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("      " ^ s) ; aux tail end
  in
  let first lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline s ; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("\n      " ^ s) ; aux tail end
  in
  begin
    print_string ("! [Error at " ^ category ^ "] ") ;
    first lines
  end


let is_suffix pfx str =
  let pfxlen = String.length pfx in
  let strlen = String.length str in
    if strlen < pfxlen then false else
      (compare pfx (String.sub str (strlen - pfxlen) pfxlen)) = 0


let is_document_file   = is_suffix ".mcrd"
let is_header_file     = is_suffix ".mcrdh"
let is_standalone_file = is_suffix ".mcrds"


let make_environment_from_header_file (tyenv : Typeenv.t) env file_name_in =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_numexpr () ;
        let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
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


let read_standalone_file (tyenv : Typeenv.t) env file_name_in file_name_out =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_numexpr () ;
        let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
        let (ty, _, ast) = Typechecker.main tyenv utast in
          begin
            print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) ;
            match ty with
            | (_, StringType) ->
                let evaled = Evaluator.interpret env ast in
                let content_out = Out.main evaled in
                  begin
                    Files.file_out_of_string file_name_out content_out ;
                    print_endline (" ---- ---- ---- ----") ;
                    print_endline ("  output written on '" ^ file_name_out ^ "'.")
                  end
            | _  -> raise (MainError("the output of '" ^ file_name_in ^ "' is not string"))
          end
      end
  end


let read_document_file (tyenv : Typeenv.t) env file_name_in file_name_out =
  begin
    print_endline (" ---- ---- ---- ----") ;
    print_endline ("  reading '" ^ file_name_in ^ "' ...") ;
    let file_in = open_in file_name_in in
      begin
        Lexer.reset_to_strexpr () ;
        let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
        let (ty, _, ast) = Typechecker.main tyenv utast in
          begin
            print_endline ("  type check: " ^ (string_of_mono_type tyenv ty)) ;
            match ty with
            | (_, StringType) ->
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
      end
  end


let error_log_environment suspended =
  try
    suspended ()
  with
  | Lexer.LexError(s)               -> report_error "Lexer" [ NormalLine(s); ]
  | Parsing.Parse_error             -> report_error "Parser" [ NormalLine("something is wrong."); ]
  | ParseErrorDetail(s)             -> report_error "Parser" [ NormalLine(s); ]

  | Typechecker.UndefinedVariable(rng, varnm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined variable '" ^ varnm ^ "'.");
      ]

  | Typechecker.UndefinedConstructor(rng, constrnm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined constructor '" ^ constrnm ^ "'.");
      ]

  | Typeenv.IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("'" ^ tynm ^ "' is expected to have " ^ (string_of_int lenexp) ^ " type argument(s),");
        NormalLine("but it has " ^ (string_of_int lenerr) ^ " type argument(s) here.");
      ]

  | Typeenv.UndefinedTypeName(rng, tynm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type name '" ^ tynm ^ "'");
      ]

  | Typeenv.UndefinedTypeArgument(rng, tyargnm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("undefined type argument '" ^ tyargnm ^ "'");
      ]

  | Typeenv.CyclicTypeDefinition(reslist) ->
      report_error "Typechecker" (
        (NormalLine("cyclic synonym type definition:"))
        :: (List.map (fun (rng, strty) -> DisplayLine(strty ^ " (at " ^ (Range.to_string rng) ^ ")")) reslist)
      )

  | Typeenv.MultipleTypeDefinition(rng1, rng2, tynm) ->
      report_error "Typechecker" [
        NormalLine("parallel type definition by the same name:");
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng1) ^ ")");
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng2) ^ ")");
      ]

  | Typeenv.NotProvidingTypeImplementation(rng, tynm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("The implementation does not provide type '" ^ tynm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotProvidingValueImplementation(rng, varnm) ->
      report_error "Typechecker" [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine("The implementation does not provide value '" ^ varnm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotMatchingInterface(rng, varnm, tyenv1, pty1, tyenv2, pty2) ->
      report_error "Typechecker" [
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
        report_error "Typechecker" (List.append [
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
        report_error "Typechecker" (List.append [
          NormalLine(posmsg);
          NormalLine("this expression has types");
          DisplayLine(strtyA);
          NormalLine("and");
          DisplayLine(strtyB);
          NormalLine("at the same time, but these are incompatible.");
        ] additional)

  | Evaluator.EvalError(s)          -> report_error "Evaluator" [ NormalLine(s); ]
  | Out.IllegalOut(s)               -> report_error "Output" [ NormalLine(s); ]
  | MainError(s)                    -> report_error "Toplevel" [ NormalLine(s); ]
  | Sys_error(s)                    -> report_error "System" [ NormalLine(s); ]


let rec main (tyenv : Typeenv.t) (env : environment) (file_name_in_list : string list) (file_name_out : string) =
  error_log_environment (fun () ->
    match file_name_in_list with
    | [] ->
        begin
          print_endline " ---- ---- ---- ----" ;
          print_endline "  no output."
        end
    | file_name_in :: tail  when is_document_file file_name_in ->
          read_document_file tyenv env file_name_in file_name_out

    | file_name_in :: tail  when is_header_file file_name_in ->
          let (newtyenv, newenv) = make_environment_from_header_file tyenv env file_name_in in
            main newtyenv newenv tail file_name_out

    | file_name_in :: tail  when is_standalone_file file_name_in ->
          read_standalone_file tyenv env file_name_in file_name_out

    | file_name_in :: _ -> raise (MainError("'" ^ file_name_in ^ "' has illegal filename extension"))
  )


let rec see_argv (num : int) (file_name_in_list : string list) (file_name_out : string) =
    if num = Array.length Sys.argv then
      begin
        print_endline ("  [output] " ^ file_name_out) ;
        print_endline "" ;
        FreeID.initialize () ;
        BoundID.initialize () ;
        TypeID.initialize () ;
        Typeenv.initialize_id () ;
        EvalVarID.initialize () ;
        let (tyenv, env) = Primitives.make_environments () in
          main tyenv env file_name_in_list file_name_out
      end
    else
      match Sys.argv.(num) with
      | "-v" ->
          print_string (
              "  Macrodown version 1.00z\n"
            ^ "    ____   ____       ________     _____   ______\n"
            ^ "    \\   \\  \\   \\     /   _____|   /   __| /      \\\n"
            ^ "     \\   \\  \\   \\   /   /        /   /   /   /\\   \\\n"
            ^ "     /    \\  \\   \\  \\   \\       /   /   /   /  \\   \\\n"
            ^ "    /      \\  \\   \\  \\   \\     /   /   /   /    \\   \\\n"
            ^ "   /   /\\   \\  \\   \\  \\   \\   /   /   /   /      \\   \\\n"
            ^ "  /   /  \\   \\  \\   \\  \\___\\ /___/   /   /        \\   \\\n"
            ^ " /   /    \\   \\  \\   \\              /   /_________/   /\n"
            ^ "/___/      \\___\\  \\___\\            /_________________/\n"
          )
      | "-o" ->
          begin
            try see_argv (num + 2) file_name_in_list (Sys.argv.(num + 1)) with
            | Invalid_argument(s) -> print_endline ("! missing file name after '-o' option; " ^ s)
          end
      | "-t" ->
          begin
            show_control_sequence_type := true ;
            see_argv (num + 1) file_name_in_list file_name_out
          end
      | "-f" ->
          begin
            show_control_sequence_type := true ;
            show_function_type         := true ;
            see_argv (num + 1) file_name_in_list file_name_out
          end
      | _    ->
          begin
            print_endline ("  [input] " ^ Sys.argv.(num)) ;
            see_argv (num + 1) (file_name_in_list @ [Sys.argv.(num)]) file_name_out
          end


let _ = see_argv 1 [] "mcrd.out"
