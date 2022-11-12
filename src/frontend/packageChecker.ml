
open MyUtil
open Types
open StaticEnv
open ConfigError

type 'a ok = ('a, config_error) result

type dependency_kind = PackageDependency | LocalDependency


let add_dependency_to_type_environment ~(package_only : bool) (header : header_element list) (genv : global_type_environment) (tyenv : Typeenv.t) : Typeenv.t ok =
  let open ResultMonad in
  header |> foldM (fun tyenv headerelem ->
    let opt =
      match headerelem with
      | HeaderUse{ opening; module_name = modident }
      | HeaderUseOf{ opening; module_name = modident; _ } ->
          if package_only then
            None
          else
            Some((LocalDependency, opening, modident))

      | HeaderUsePackage{ opening; module_name = modident } ->
          Some((PackageDependency, opening, modident))
    in
    match opt with
    | None ->
        return tyenv

    | Some((kind, opening, (rng, modnm))) ->
        begin
          match (kind, genv |> GlobalTypeenv.find_opt modnm) with
          | (LocalDependency, None) ->
              assert false (* Local dependency must be resolved beforehand. *)

          | (PackageDependency, None) ->
              err @@ UnknownPackageDependency(rng, modnm)

          | (_, Some(ssig)) ->
              let mentry = { mod_signature = ConcStructure(ssig) } in
              let tyenv = tyenv |> Typeenv.add_module modnm mentry in
              let tyenv =
                if opening then
                  tyenv |> TypecheckUtil.add_to_type_environment_by_signature ssig
                else
                  tyenv
              in
              return tyenv
        end
  ) tyenv


let typecheck_library_file ~for_struct:(tyenv_for_struct : Typeenv.t) ~for_sig:(tyenv_for_sig : Typeenv.t) (abspath_in : abs_path) (utsig_opt : untyped_signature option) (utbinds : untyped_binding list) : (StructSig.t abstracted * binding list) ok =
  let open ResultMonad in
  let res =
    Logging.begin_to_typecheck_file abspath_in;
    let* absmodsig_opt = utsig_opt |> optionM (ModuleTypechecker.typecheck_signature tyenv_for_sig) in
    let* ret = ModuleTypechecker.main tyenv_for_struct absmodsig_opt utbinds in
    Logging.pass_type_check None;
    return ret
  in
  res |> Result.map_error (fun tyerr -> TypeError(tyerr))


let typecheck_document_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree ok =
  let open ResultMonad in
  Logging.begin_to_typecheck_file abspath_in;
  let* (ty, ast) = Typechecker.main Stage1 tyenv utast |> Result.map_error (fun tyerr -> TypeError(tyerr)) in
  Logging.pass_type_check (Some(Display.show_mono_type ty));
  if OptionState.is_text_mode () then
    if Typechecker.are_unifiable ty (Range.dummy "text-mode", BaseType(StringType)) then
      return ast
    else
      err (NotAStringFile(abspath_in, ty))
  else
    if Typechecker.are_unifiable ty (Range.dummy "pdf-mode", BaseType(DocumentType)) then
      return ast
    else
      err (NotADocumentFile(abspath_in, ty))


let check_library_package (tyenv_prim : Typeenv.t) (genv : global_type_environment) (main_module_name : module_name) (utlibs : (abs_path * untyped_library_file) list) =
  let open ResultMonad in

  (* Resolve dependency among the source files in the package: *)
  let* sorted_utlibs = ClosedFileDependencyResolver.main utlibs in

  (* Typecheck each source file: *)
  let* (_genv, libacc, ssig_opt) =
    sorted_utlibs |> foldM (fun (genv, libacc, ssig_opt) (abspath, utlib) ->
      let (_attrs, header, (modident, utsig_opt, utbinds)) = utlib in
      let* tyenv_for_struct = tyenv_prim |> add_dependency_to_type_environment ~package_only:false header genv in
      let (_, modnm) = modident in
      if String.equal modnm main_module_name then
        let* ((_quant, ssig), binds) =
          let* tyenv_for_sig = tyenv_prim |> add_dependency_to_type_environment ~package_only:true header genv in
          typecheck_library_file ~for_struct:tyenv_for_struct ~for_sig:tyenv_for_sig abspath utsig_opt utbinds
        in
        let genv = genv |> GlobalTypeenv.add modnm ssig in
        return (genv, Alist.extend libacc (abspath, binds), Some(ssig))
      else
        let* ((_quant, ssig), binds) =
          typecheck_library_file ~for_struct:tyenv_for_struct ~for_sig:tyenv_for_struct abspath utsig_opt utbinds
        in
        let genv = genv |> GlobalTypeenv.add modnm ssig in
        return (genv, Alist.extend libacc (abspath, binds), ssig_opt)
    ) (genv, Alist.empty, None)
  in
  let libs = Alist.to_list libacc in

  match ssig_opt with
  | Some(ssig) -> return (ssig, libs)
  | None       -> err @@ NoMainModule(main_module_name)


let check_font_package (_main_module_name : module_name) (font_files : font_file_record list) =
  let open ResultMonad in
  let stage = Persistent0 in
  let (ssig, libacc) =
    font_files |> List.fold_left (fun (ssig, libacc) r ->
      let
        {
          r_font_file_path     = path;
          r_font_file_contents = font_file_contents;
          r_used_as_math_font  = used_as_math_font;
        } = r
      in
      match font_file_contents with
      | OpentypeSingle(varnm) ->
          let evid = EvalVarID.fresh (Range.dummy "font-package 1", varnm) in
          let bind = Bind(stage, NonRec(evid, LoadSingleFont{ path; used_as_math_font })) in
          let ventry =
            {
              val_name  = Some(evid);
              val_type  = Poly(Range.dummy "font-package 2", BaseType(FontType));
              val_stage = stage;
            }
          in
          (ssig |> StructSig.add_value varnm ventry, Alist.extend libacc (path, [ bind ]))

      | OpentypeCollection(varnms) ->
          let (ssig, bindacc, _) =
            varnms |> List.fold_left (fun (ssig, bindacc, index) varnm ->
              let evid = EvalVarID.fresh (Range.dummy "font-package 3", varnm) in
              let bind = Bind(stage, NonRec(evid, LoadCollectionFont{ path; index; used_as_math_font })) in
              let ventry =
                {
                  val_name  = Some(evid);
                  val_type  = Poly(Range.dummy "font-package 4", BaseType(FontType));
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


let main (tyenv_prim : Typeenv.t) (genv : global_type_environment) (package : untyped_package) : (StructSig.t * (abs_path * binding list) list) ok =
  match package with
  | UTLibraryPackage{ main_module_name; modules = utlibs } ->
      check_library_package tyenv_prim genv main_module_name utlibs

  | UTFontPackage{ main_module_name; font_files } ->
      check_font_package main_module_name font_files


let main_document (tyenv_prim : Typeenv.t) (genv : global_type_environment) (sorted_locals : (abs_path * untyped_library_file) list) (abspath_and_utdoc : abs_path * untyped_document_file) : ((abs_path * binding list) list * abstract_tree) ok =
  let open ResultMonad in
  let* (genv, libacc) =
    sorted_locals |> foldM (fun (genv, libacc) (abspath, utlib) ->
      let (_attrs, header, (modident, utsig_opt, utbinds)) = utlib in
      let (_, modnm) = modident in
      let* ((_quant, ssig), binds) =
        let* tyenv = tyenv_prim |> add_dependency_to_type_environment ~package_only:false header genv in
        typecheck_library_file ~for_struct:tyenv ~for_sig:tyenv abspath utsig_opt utbinds
      in
      let genv = genv |> GlobalTypeenv.add modnm ssig in
      return (genv, Alist.extend libacc (abspath, binds))
    ) (genv, Alist.empty)
  in
  let libs = Alist.to_list libacc in

  (* Typecheck the document: *)
  let* ast_doc =
    let (abspath, (_attrs, header, utast)) = abspath_and_utdoc in
    let* tyenv = tyenv_prim |> add_dependency_to_type_environment ~package_only:false header genv in
    typecheck_document_file tyenv abspath utast
  in

  return (libs, ast_doc)
