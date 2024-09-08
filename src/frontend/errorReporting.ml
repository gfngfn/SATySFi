
open ConfigError
open FontError
open Format
open MyUtil
open LoggingUtil
open StaticEnv
open Types
open TypeError


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


let report_and_exit msg =
    print_string msg;
    exit 1


let make_error_message (cat : error_category) (lines : line list) =
  let buf = Buffer.create 512 in
  let formatter = formatter_of_buffer buf in
  let f_print_endline s =
    pp_print_string formatter s;
    pp_print_newline formatter ()
  in
  pp_print_string formatter (Printf.sprintf "! [%s] " (show_error_category cat));
  lines |> List.fold_left (fun (is_first : bool) (line : line) ->
    begin
      match line with
      | NormalLine(s) ->
          if is_first then
            f_print_endline s
          else
            f_print_endline ("    " ^ s)

      | DisplayLine(s) ->
          if is_first then
            f_print_endline ("\n      " ^ s)
          else
            f_print_endline ("      " ^ s)
    end;
    false
  ) true |> ignore;
  pp_print_flush formatter ();
  Buffer.contents buf


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


let make_parse_error_message = function
  | CannotProgressParsing(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
      ]

  | IllegalItemDepth{ range = rng; before; current } ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "illegal item depth %d after %d" before current);
      ]

  | EmptyInputFile(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("empty input.");
      ]


let make_type_error_message = function
  | UndefinedVariable(rng, varnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined variable '%s'." varnm);
        ];
        candidates_message_lines;
      ]

  | UndefinedConstructor(rng, constrnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined constructor '%s'." constrnm);
        ];
        candidates_message_lines;
      ]

  | UndefinedTypeName(rng, tynm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type '%s'." tynm);
      ]

  | UndefinedTypeVariable(rng, tyvarnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type variable '%s'." tyvarnm);
      ]

  | UndefinedRowVariable(rng, rowvarnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined row variable '%s'." rowvarnm);
      ]

  | UndefinedKindName(rng, kdnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined kind '%s'." kdnm);
      ]

  | UndefinedModuleName(rng, modnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined module '%s'." modnm);
      ]

  | UndefinedSignatureName(rng, signm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined signature '%s'." signm);
      ]

  | UndefinedMacro(rng, csnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined macro '%s'." csnm);
      ]

  | InvalidNumberOfMacroArguments(rng, macparamtys) ->
      List.append [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid number of macro arguments; types expected on arguments are:");
      ] (macparamtys |> List.map (function
        | LateMacroParameter(ty)  -> DisplayLine(Printf.sprintf "* %s" (Display.show_mono_type ty))
        | EarlyMacroParameter(ty) -> DisplayLine(Printf.sprintf "* ~%s" (Display.show_mono_type ty))
      ))

  | LateMacroArgumentExpected(rng, ty) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an early macro argument is given, but a late argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | EarlyMacroArgumentExpected(rng, ty) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a late macro argument is given, but an early argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | UnknownUnitOfLength(rng, unitnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined unit of length '%s'." unitnm);
      ]

  | InlineCommandInMath(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an inline command is used as a math command.");
      ]

  | MathCommandInInline(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a math command is used as an inline command.");
      ]

  | BreaksValueRestriction(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression breaks the value restriction;");
        NormalLine("it should be a syntactic function.");
      ]

  | MultiplePatternVariable(rng1, rng2, varnm) ->
      [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and at %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "pattern variable '%s' is bound more than once." varnm);
      ]

  | LabelUsedMoreThanOnce(rng, label) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is used more than once." label);
      ]

  | InvalidExpressionAsToStaging(rng, stage) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid expression as to stage;");
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | InvalidOccurrenceAsToStaging(rng, varnm, stage) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "invalid occurrence of variable '%s' as to stage;" varnm);
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | ApplicationOfNonFunction(rng, ty) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression has type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]

  | MultiCharacterMathScriptWithoutBrace(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("more than one character is used as a math sub/superscript without braces;");
        NormalLine("use braces for making association explicit.");
      ]

  | IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      [
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
      List.concat [
        [
          NormalLine(posmsg);
          NormalLine("this expression has type");
          DisplayLine(Printf.sprintf "%s," strtyA);
          NormalLine("but is expected of type");
          DisplayLine(Printf.sprintf "%s." strtyB);
        ];
        detail;
        additional;
      ]

  | RowUnificationError(rng, row1, row2, ue) ->
      let dispmap =
        DisplayMap.empty
          |> Display.collect_ids_mono_row row1
          |> Display.collect_ids_mono_row row2
      in
      let str_row1 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      let str_row2 = Display.show_mono_row_by_map dispmap row2 |> Option.value ~default:"" in
      let detail = make_unification_error_message dispmap ue in
      List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine("the option row is");
          DisplayLine(str_row1);
          NormalLine("and");
          DisplayLine(Printf.sprintf "%s," str_row2);
          NormalLine("at the same time, but these are incompatible.");
        ];
        detail;
      ]

  | TypeParameterBoundMoreThanOnce(rng, tyvarnm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "type variable %s is bound more than once." tyvarnm);
      ]

  | ConflictInSignature(rng, member) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is declared more than once in a signature." member);
      ]

  | NotAStructureSignature(rng, _fsig) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a structure signature (TODO (enhance): detailed report)");
      ]

  | NotAFunctorSignature(rng, _ssig) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a functor signature (TODO (enhance): detailed report)");
      ]

  | MissingRequiredValueName(rng, x, pty) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required value '%s' of type" x);
        DisplayLine(Display.show_poly_type pty);
      ]

  | MissingRequiredMacroName(rng, csnm, pmacty) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required macro '%s' of type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty);
      ]

  | MissingRequiredConstructorName(rng, ctornm, _centry) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required constructor '%s'" ctornm);
        (* TODO (enhance): consider displaying `centry` *)
      ]

  | MissingRequiredTypeName(rng, tynm, arity) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required type '%s' of arity %d" tynm arity);
      ]

  | MissingRequiredModuleName(rng, modnm, _modsig) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required module '%s'" modnm);
        (* TODO (enhance): consider displaying `modsig` *)
      ]

  | MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required signature '%s'" signm);
        (* TODO (enhance): consider displaying absmodsig *)
      ]

  | NotASubtypeAboutValue(rng, x, pty1, pty2) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about value '%s'; type" x);
        DisplayLine(Display.show_poly_type pty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_type pty2);
      ]

  | NotASubtypeAboutValueStage(rng, x, stage1, stage2) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about the stage of value '%s';" x);
        DisplayLine(string_of_stage stage1);
        NormalLine("is not consistent with");
        DisplayLine(string_of_stage stage2);
      ]

  | NotASubtypeAboutMacro(rng, csnm, pmacty1, pmacty2) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about macro '%s'; type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_macro_type pmacty2);
      ]

  | NotASubtypeAboutConstructor(rng, ctornm, _tyscheme1, _tyscheme2) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about constructor '%s' (TODO (enhance): detailed report)" ctornm);
      ]

  | NotASubtypeAboutType(rng, tynm, tentry1, tentry2) ->
      Format.printf "1: %a,@ 2: %a@," pp_type_entry tentry1 pp_type_entry tentry2; (* TODO: remove this *)
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about type '%s' (TODO (enhance): detailed report)" tynm);
      ]

  | NotASubtypeSignature(rng, _modsig1, _modsig2) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a subtype signature (TODO (enhance): detailed report)");
      ]

  | UnexpectedOptionalLabel(rng, label, ty_cmd) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "unexpected application of label '%s';" label);
        NormalLine(Printf.sprintf "the command used here has type");
        DisplayLine(Display.show_mono_type ty_cmd);
      ]

  | InvalidArityOfCommandApplication(rng, arity_expected, arity_actual) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "this command expects %d argument(s)," arity_expected);
        NormalLine(Printf.sprintf "but is applied to %d argument(s) here." arity_actual);
      ]

  | CannotRestrictTransparentType(rng, tynm) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot restrict transparent type '%s'." tynm);
      ]

  | KindContradiction(rng, tynm, kd_expected, kd_actual) ->
      let Kind(bkds_expected) = kd_expected in
      let Kind(bkds_actual) = kd_actual in
      [
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
      (NormalLine("the following synonym types are cyclic:") :: lines)

  | MultipleSynonymTypeDefinition(tynm, rng1, rng2) ->
      [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "synonym type '%s' is defined more than once." tynm);
      ]

  | ValueAttributeError(ValueAttribute.Unexpected(rng)) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("unexpected value attributes.");
      ]

  | ModuleAttributeError(ModuleAttribute.Unexpected(rng)) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("unexpected module attributes.");
      ]

  | TestMustBeStage1NonRec(rng) ->
      [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("tests must be stage-1 non-recursive bindings.");
      ]


let module_name_chain_to_string (((_, modnm0), modidents) : module_name_chain) =
  let modidents = modidents |> List.map (fun (_, modnm) -> modnm) in
  let modidents = modnm0 :: modidents in
  modidents |> String.concat "."


let show_yaml_context (yctx : YamlDecoder.context) =
  Printf.sprintf "(context: %s)" (YamlDecoder.show_yaml_context yctx)


let make_yaml_error_message : yaml_error -> line list = function
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

  | NotAnUppercasedIdentifier{ context = yctx; got = s } ->
      [ NormalLine(Printf.sprintf "not an uppercased identifier: '%s' %s" s (show_yaml_context yctx)) ]

  | NotALowercasedIdentifier{ context = yctx; got = s } ->
      [ NormalLine(Printf.sprintf "not a lowercased identifier: '%s' %s" s (show_yaml_context yctx)) ]

  | NotACommand{ context = yctx; prefix = _; got = s } ->
      [ NormalLine(Printf.sprintf "not a command: '%s' %s" s (show_yaml_context yctx)) ]

  | NotAChainedIdentifier{ context = yctx; got = s } ->
      [ NormalLine(Printf.sprintf "not a long identifier: '%s' %s" s (show_yaml_context yctx)) ]

  | NotAnAbsolutePath{ context = yctx; got = s } ->
      [ NormalLine(Printf.sprintf "not an absolute path: '%s' %s" s (show_yaml_context yctx)) ]

  | NotARelativePath{ context = yctx; got = s } ->
      [ NormalLine(Printf.sprintf "not a relative path: '%s' %s" s (show_yaml_context yctx)) ]


let make_config_error_message (logging_spec : logging_spec) : config_error -> string = function
  | UnexpectedExtension(ext) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "unexpected file extension '%s'." ext);
      ]

  | NotALibraryFile(abspath) ->
      make_error_message Typechecker [
        NormalLine("the following file is expected to be a library file, but is not:");
        DisplayLine(Logging.show_path logging_spec abspath);
      ]

  | NotADocumentFile(abspath_in, ty) ->
      let fname = Logging.show_path logging_spec abspath_in in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a document file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAStringFile(abspath_in, ty) ->
      let fname = Logging.show_path logging_spec abspath_in in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a file for generating text; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | FileModuleNotFound(rng, modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot find a source file that defines module '%s'." modnm);
      ]

  | FileModuleNameConflict(modnm, abspath1, abspath2) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "more than one file defines module '%s':" modnm);
        DisplayLine(Printf.sprintf "- %s" (Logging.show_path logging_spec abspath1));
        DisplayLine(Printf.sprintf "- %s" (Logging.show_path logging_spec abspath2));
      ]

  | NoMainModule(modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "no main module '%s'." modnm);
      ]

  | UnknownPackageDependency(rng, modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "dependency on unknown package '%s'" modnm);
      ]

  | TypeError(tyerr) ->
      make_error_message Typechecker (make_type_error_message tyerr)

  | CyclicFileDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      make_error_message Interface (
        (NormalLine("cyclic dependency detected:")) ::
          (pairs |> List.map (fun (abspath, _) -> DisplayLine(Logging.show_path logging_spec abspath)))
      )

  | CannotReadFileOwingToSystem(msg) ->
      make_error_message Interface [
        NormalLine("cannot read file:");
        DisplayLine(msg);
      ]

  | LibraryContainsWholeReturnValue(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "file '%s' is not a library; it has a return value." fname);
      ]

  | DocumentLacksWholeReturnValue(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "file '%s' is not a document; it lacks a return value." fname);
      ]

  | CannotUseHeaderUse((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s' here; use 'use %s of ...' instead." modnm modnm);
      ]

  | CannotUseHeaderUseOf((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s of ...' here; use 'use %s' instead." modnm modnm);
      ]

  | FailedToParse(e) ->
      make_error_message Parser (make_parse_error_message e)

  | MainModuleNameMismatch{ expected; got } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "main module name mismatch; expected '%s' but got '%s'." expected got);
      ]

  | EnvelopeNameConflict(envelope_name) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "envelope name conflict: '%s'" envelope_name);
      ]

  | DependencyOnUnknownEnvelope{ depending; depended } ->
      make_error_message Interface [
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
      make_error_message Interface
        (NormalLine("the following envelopes are cyclic:") :: lines)

  | LocalFileNotFound{ relative; candidates } ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (Logging.show_path logging_spec abspath))
        )
      in
      make_error_message Interface
        (NormalLine(Printf.sprintf "cannot find local file '%s'. candidates:" relative) :: lines)

  | DepsConfigNotFound(abspath_deps_config) ->
      make_error_message Interface [
        NormalLine("cannot find a deps config at:");
        DisplayLine(Logging.show_path logging_spec abspath_deps_config);
      ]

  | DepsConfigError(abspath_deps_config, e) ->
      make_error_message Interface (List.append [
        NormalLine("failed to load a deps config:");
        DisplayLine(Logging.show_path logging_spec abspath_deps_config);
      ] (make_yaml_error_message e))

  | EnvelopeConfigNotFound(abspath_envelope_config) ->
      make_error_message Interface [
        NormalLine("cannot find an envelope config at:");
        DisplayLine(Logging.show_path logging_spec abspath_envelope_config);
      ]

  | EnvelopeConfigError(abspath_envelope_config, e) ->
      make_error_message Interface (List.append [
        NormalLine("failed to load an envelope config:");
        DisplayLine(Logging.show_path logging_spec abspath_envelope_config);
      ] (make_yaml_error_message e))

  | DumpFileError(abspath_dump, e) ->
      make_error_message Interface (List.append [
        NormalLine("failed to load a dump file (just removing it will remedy this):");
        DisplayLine(Logging.show_path logging_spec abspath_dump);
      ] (make_yaml_error_message e))

  | CannotWriteDumpFile(abspath_dump) ->
      make_error_message Interface [
        NormalLine("cannot write a dump file:");
        DisplayLine(Logging.show_path logging_spec abspath_dump);
      ]

  | DependedEnvelopeNotFound(envelope_name) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "unknown depended envelope '%s'" envelope_name);
      ]

  | MarkdownClassNotFound ->
      make_error_message Interface [
        NormalLine("Markdown class not found");
      ]

  | NoMarkdownConversion ->
      make_error_message Interface [
        NormalLine("no Markdown conversion");
      ]

  | MarkdownError(e) ->
      begin
        match e with
        | InvalidHeaderComment ->
            make_error_message Interface [
              NormalLine("invalid or missing header comment of a Markdown document.");
            ]

        | InvalidExtraExpression ->
            make_error_message Interface [
              NormalLine("cannot parse an extra expression in a Markdown document.");
            ]
      end

  | CannotReadDirectory{ path; message } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot read directory '%s':" (Logging.show_path logging_spec path));
        DisplayLine(message);
      ]

  | CannotOutputResult{ path; message } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot output the final result '%s':" (Logging.show_path logging_spec path));
        DisplayLine(message);
      ]


let make_font_error_message (logging_spec : logging_spec) = function
  | FailedToReadFont(abspath, msg) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "cannot load font file '%s';" fname);
        DisplayLine(msg);
      ]

  | FailedToDecodeFont(abspath, e) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "cannot decode font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Decode.Error.pp e);
      ]

  | FailedToMakeSubset(abspath, e) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "cannot make a subset of font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Subset.Error.pp e);
      ]

  | NotASingleFont(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "the font file '%s' is not a single font file." fname);
      ]

  | NotAFontCollectionElement(abspath, index) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "the font file '%s' (used with index %d) is not a collection." fname index);
      ]

  | NoMathTable(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'MATH' table." fname);
      ]

  | PostscriptNameNotFound(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "font file '%s' does not have a PostScript name." fname);
      ]

  | CannotFindUnicodeCmap(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'cmap' subtable for Unicode code points." fname);
      ]

  | CollectionIndexOutOfBounds{ path; index; num_elements } ->
      let fname = Logging.show_path logging_spec path in
      [
        NormalLine(Printf.sprintf "%d: index out of bounds;" index);
        NormalLine(Printf.sprintf "font file '%s' has %d elements." fname num_elements);
      ]

  | UnsupportedNoLangSys ->
      [ NormalLine("unsupported: no LangSys") ]

  | UnsupportedCidToGidMapOtherThanIdentity ->
      [ NormalLine("unsupported: /CIDToGIDMap other than /Identity") ]


let error_log_environment (logging_spec : logging_spec) (suspended : unit -> ('a, config_error) result) : ('a, config_error) result =
  let report_error kind lines =
    report_and_exit (make_error_message kind lines)
  in
  try
    suspended ()
  with
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
      report_error Interface (make_font_error_message logging_spec e)

  | ImageHashTable.CannotLoadPdf(msg, abspath, pageno) ->
      let fname = Logging.show_path logging_spec abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load PDF file '%s' page #%d;" fname pageno);
        DisplayLine(msg);
      ]

  | ImageHashTable.CannotLoadImage(msg, abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine(msg);
      ]

  | ImageHashTable.ImageOfWrongFileType(abspath) ->
      let fname = Logging.show_path logging_spec abspath in
      report_error Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This file format is not supported.");
      ]

  | ImageHashTable.UnsupportedColorModel(_, abspath) ->
      let fname = Logging.show_path logging_spec abspath in
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
