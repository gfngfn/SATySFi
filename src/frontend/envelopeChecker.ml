
open MyUtil
open EnvelopeSystemBase
open Types
open StaticEnv
open ConfigError
open TypeError

type 'a ok = ('a, config_error) result

type dependency_kind = EnvelopeDependency | LocalDependency

type local_type_environment = StructSig.t ModuleNameMap.t


let is_test_only_dependency (attrs : untyped_attribute list) : bool =
  attrs |> List.exists (fun (rng, attr) ->
    match attr with
    | UTAttribute("test-only", None) -> true
    | _                              -> false
  )


let add_dependency_to_type_environment ~(import_envelope_only : bool) ~(testing : bool) (header : header_element list) (genv : global_type_environment) (used_as_map : envelope_name ModuleNameMap.t) (lenv : local_type_environment) (tyenv : Typeenv.t) : Typeenv.t ok =
  let open ResultMonad in
  header |> foldM (fun tyenv headerelem ->
    let opt =
      match headerelem with
      | HeaderUse{ attributes; opening; mod_chain }
      | HeaderUseOf{ attributes; opening; mod_chain; _ } ->
          if import_envelope_only then
            None
          else if is_test_only_dependency attributes then
            if testing then
              Some((LocalDependency, opening, mod_chain))
            else
              None
          else
            Some((LocalDependency, opening, mod_chain))

      | HeaderUsePackage{ attributes; opening; mod_chain } ->
          if is_test_only_dependency attributes then
            if testing then
              Some((EnvelopeDependency, opening, mod_chain))
            else
              None
          else
            Some((EnvelopeDependency, opening, mod_chain))
    in
    match opt with
    | None ->
        return tyenv

    | Some((kind, opening, (rng, ((rng0, modnm0), modidents)))) ->
        let* ssig =
          match kind with
          | LocalDependency ->
              begin
                match lenv |> ModuleNameMap.find_opt modnm0 with
                | None ->
                    assert false (* Local dependency must be resolved beforehand. *)

                | Some(ssig) ->
                    return ssig
              end

          | EnvelopeDependency ->
              let* envelope_name0 =
                match used_as_map |> ModuleNameMap.find_opt modnm0 with
                | None                 -> err @@ UnknownPackageDependency(rng0, modnm0)
                | Some(envelope_name0) -> return envelope_name0
              in
              let ssig =
                match genv |> GlobalTypeenv.find_opt (EnvelopeName.EN(envelope_name0)) with
                | None       -> assert false (* Envelopes must be topologically sorted. *)
                | Some(ssig) -> ssig
              in
              return ssig
        in
        let mentry0 = { mod_signature = ConcStructure(ssig) } in
        let modnm =
          match List.rev modidents with
          | []              -> modnm0
          | (_, modnm) :: _ -> modnm
        in
        let* mentry =
          match TypecheckUtil.resolve_module_chain mentry0 modidents with
          | Ok(mentry) -> return mentry
          | Error(e)   -> err @@ TypeError(e)
        in
        let tyenv = tyenv |> Typeenv.add_module modnm mentry in
        let* tyenv =
          if opening then
            let* ssig =
              match mentry.mod_signature with
              | ConcStructure(ssig) -> return ssig
              | ConcFunctor(fsig)   -> err @@ TypeError(NotAStructureSignature(rng, fsig))
            in
            return (tyenv |> TypecheckUtil.add_to_type_environment_by_signature ssig)
          else
            return tyenv
        in
        return tyenv
  ) tyenv


let typecheck_library_file (display_config : Logging.config) (config : typecheck_config) ~for_struct:(tyenv_for_struct : Typeenv.t) ~for_sig:(tyenv_for_sig : Typeenv.t) (abspath_in : abs_path) (utsig_opt : untyped_signature option) (rng_struct : Range.t) (utbinds : untyped_binding list) : (StructSig.t abstracted * binding list) ok =
  let open ResultMonad in
  let res =
    Logging.begin_to_typecheck_file display_config abspath_in;
    let* absmodsig_opt = utsig_opt |> optionM (ModuleTypechecker.typecheck_signature config tyenv_for_sig) in
    let* ret = ModuleTypechecker.main config tyenv_for_struct absmodsig_opt rng_struct utbinds in
    Logging.pass_type_check display_config None;
    return ret
  in
  res |> Result.map_error (fun tyerr -> TypeError(tyerr))


let typecheck_document_file (display_config : Logging.config) (config : typecheck_config) (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree ok =
  let open ResultMonad in
  Logging.begin_to_typecheck_file display_config abspath_in;
  let* (ty, ast) = Typechecker.main config Stage1 tyenv utast |> Result.map_error (fun tyerr -> TypeError(tyerr)) in
  Logging.pass_type_check display_config (Some(Display.show_mono_type ty));
  if config.is_text_mode then
    if Typechecker.are_unifiable ty (Range.dummy "text-mode", BaseType(StringType)) then
      return ast
    else
      err (NotAStringFile(abspath_in, ty))
  else
    if Typechecker.are_unifiable ty (Range.dummy "pdf-mode", BaseType(DocumentType)) then
      return ast
    else
      err (NotADocumentFile(abspath_in, ty))


let check_library_envelope ~(testing : bool) (display_config : Logging.config) (config : typecheck_config) (tyenv_prim : Typeenv.t) (genv : global_type_environment) (used_as_map : envelope_name ModuleNameMap.t) (main_module_name : module_name) (utlibs : (abs_path * untyped_library_file) list) =
  let open ResultMonad in

  (* Resolves dependency among the source files in the envelope: *)
  let* sorted_utlibs = ClosedFileDependencyResolver.main utlibs in

  (* Typechecks each source file: *)
  let* (_lenv, libacc, ssig_opt) =
    sorted_utlibs |> foldM (fun (lenv, libacc, ssig_opt) (abspath, utlib) ->
      let (_attrs, header, (modident, utsig_opt, rng_struct, utbinds)) = utlib in
      let* tyenv_for_struct =
        tyenv_prim |> add_dependency_to_type_environment
          ~import_envelope_only:false
          ~testing
          header genv used_as_map lenv
      in
      let (_, modnm) = modident in
      if String.equal modnm main_module_name then
        (* Typechecks the main module: *)
        let* ((_quant, ssig), binds) =
          let* tyenv_for_sig =
            tyenv_prim |> add_dependency_to_type_environment
              ~import_envelope_only:true
              ~testing
              header genv used_as_map lenv
          in
          typecheck_library_file display_config config
            ~for_struct:tyenv_for_struct ~for_sig:tyenv_for_sig abspath utsig_opt rng_struct utbinds
        in
        let lenv = lenv |> ModuleNameMap.add modnm ssig in
        return (lenv, Alist.extend libacc (abspath, binds), Some(ssig))
      else
        let* ((_quant, ssig), binds) =
          typecheck_library_file display_config config
            ~for_struct:tyenv_for_struct ~for_sig:tyenv_for_struct abspath utsig_opt rng_struct utbinds
        in
        let lenv = lenv |> ModuleNameMap.add modnm ssig in
        return (lenv, Alist.extend libacc (abspath, binds), ssig_opt)
    ) (ModuleNameMap.empty, Alist.empty, None)
  in
  let libs = Alist.to_list libacc in
  match ssig_opt with
  | Some(ssig) -> return (ssig, libs)
  | None       -> err @@ NoMainModule(main_module_name)


let check_font_envelope (_main_module_name : module_name) (font_files : font_file_record list) =
  let open ResultMonad in
  let stage = Persistent0 in
  let (ssig, libacc) =
    font_files |> List.fold_left (fun (ssig, libacc) r ->
      let
        {
          r_font_file_path     = path;
          r_font_file_contents = font_file_contents;
        } = r
      in
      match font_file_contents with
      | OpentypeSingle(font_spec) ->
          let { font_item_name = varnm; used_as_math_font } = font_spec in
          let evid = EvalVarID.fresh (Range.dummy "font-envelope 1", varnm) in
          let bind = Bind(stage, NonRec(evid, LoadSingleFont{ path; used_as_math_font })) in
          let ventry =
            {
              val_name  = Some(evid);
              val_type  = Poly(Range.dummy "font-envelope 2", BaseType(FontType));
              val_stage = stage;
            }
          in
          (ssig |> StructSig.add_value varnm ventry, Alist.extend libacc (path, [ bind ]))

      | OpentypeCollection(font_specs) ->
          let (ssig, bindacc, _) =
            font_specs |> List.fold_left (fun (ssig, bindacc, index) font_spec ->
              let { font_item_name = varnm; used_as_math_font } = font_spec in
              let evid = EvalVarID.fresh (Range.dummy "font-envelope 3", varnm) in
              let bind = Bind(stage, NonRec(evid, LoadCollectionFont{ path; index; used_as_math_font })) in
              let ventry =
                {
                  val_name  = Some(evid);
                  val_type  = Poly(Range.dummy "font-envelope 4", BaseType(FontType));
                  val_stage = stage;
                }
              in
              (ssig |> StructSig.add_value varnm ventry, Alist.extend bindacc bind, index + 1)
            ) (ssig, Alist.empty, 0)
          in
          (ssig, Alist.extend libacc (path, Alist.to_list bindacc))

    ) (StructSig.empty, Alist.empty)
  in
  return (ssig, Alist.to_list libacc)


let main ~(testing : bool) (display_config : Logging.config) (config : typecheck_config) (tyenv_prim : Typeenv.t) (genv : global_type_environment) ~(used_as_map : envelope_name ModuleNameMap.t) (envelope : untyped_envelope) : (StructSig.t * (abs_path * binding list) list) ok =
  match envelope with
  | UTLibraryEnvelope{ main_module_name; modules = utlibs } ->
      check_library_envelope ~testing display_config config tyenv_prim genv used_as_map main_module_name utlibs

  | UTFontEnvelope{ main_module_name; font_files } ->
      check_font_envelope main_module_name font_files


let main_document ~(testing : bool) (display_config : Logging.config) (config : typecheck_config) (tyenv_prim : Typeenv.t) (genv : global_type_environment) ~(used_as_map : envelope_name ModuleNameMap.t) (sorted_locals : (abs_path * untyped_library_file) list) (abspath_and_utdoc : abs_path * untyped_document_file) : ((abs_path * binding list) list * abstract_tree) ok =
  let open ResultMonad in
  let* (lenv, libacc) =
    sorted_locals |> foldM (fun (lenv, libacc) (abspath, utlib) ->
      let (_attrs, header, (modident, utsig_opt, rng_struct, utbinds)) = utlib in
      let (_, modnm) = modident in
      let* ((_quant, ssig), binds) =
        let* tyenv =
          tyenv_prim |> add_dependency_to_type_environment
            ~import_envelope_only:false
            ~testing
            header genv used_as_map lenv
        in
        typecheck_library_file display_config config
          ~for_struct:tyenv ~for_sig:tyenv abspath utsig_opt rng_struct utbinds
      in
      let lenv = lenv |> ModuleNameMap.add modnm ssig in
      return (lenv, Alist.extend libacc (abspath, binds))
    ) (ModuleNameMap.empty, Alist.empty)
  in
  let libs = Alist.to_list libacc in

  (* Typechecks the document: *)
  let* ast_doc =
    let (abspath, (_attrs, header, utast)) = abspath_and_utdoc in
    let* tyenv =
      tyenv_prim |> add_dependency_to_type_environment
        ~import_envelope_only:false
        ~testing
        header genv used_as_map lenv
    in
    typecheck_document_file display_config config tyenv abspath utast
  in

  return (libs, ast_doc)
