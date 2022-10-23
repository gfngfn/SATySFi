
open MyUtil
open Types
open StaticEnv
open TypeError


type error =
  | TypeError        of type_error
  | NotADocumentFile of abs_path * Typeenv.t * mono_type
  | NotAStringFile   of abs_path * Typeenv.t * mono_type

type 'a ok = ('a, error) result


let add_dependency_to_type_environment (header : header_element list) (genv : global_type_environment) (tyenv : Typeenv.t) =
  header |> List.fold_left (fun tyenv headerelem ->
    match headerelem with
    | HeaderUse(_) ->
        assert false

    | HeaderUsePackage((_, modnm))
    | HeaderUseOf((_, modnm), _) ->
        begin
          match genv |> GlobalTypeenv.find_opt modnm with
          | None ->
              assert false

          | Some(ssig) ->
              let mentry = { mod_signature = ConcStructure(ssig) } in
              tyenv |> Typeenv.add_module modnm mentry
        end
  ) tyenv


let typecheck_library_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utsig_opt : untyped_signature option) (utbinds : untyped_binding list) : (StructSig.t abstracted * binding list) ok =
  let open ResultMonad in
  Logging.begin_to_typecheck_file abspath_in;
  let* ret = ModuleTypechecker.main tyenv utsig_opt utbinds |> Result.map_error (fun tyerr -> TypeError(tyerr)) in
  Logging.pass_type_check None;
  return ret


let typecheck_document_file (tyenv : Typeenv.t) (abspath_in : abs_path) (utast : untyped_abstract_tree) : abstract_tree ok =
  let open ResultMonad in
  Logging.begin_to_typecheck_file abspath_in;
  let* (ty, ast) = Typechecker.main Stage1 tyenv utast |> Result.map_error (fun tyerr -> TypeError(tyerr)) in
  Logging.pass_type_check (Some(Display.show_mono_type ty));
  if OptionState.is_text_mode () then
    if Typechecker.are_unifiable ty (Range.dummy "text-mode", BaseType(StringType)) then
      return ast
    else
      err (NotAStringFile(abspath_in, tyenv, ty))
  else
    if Typechecker.are_unifiable ty (Range.dummy "pdf-mode", BaseType(DocumentType)) then
      return ast
    else
      err (NotADocumentFile(abspath_in, tyenv, ty))


let main (tyenv_prim : Typeenv.t) (genv : global_type_environment) (_package : package_info) : (StructSig.t * (abs_path * binding list) list) ok =
  let open ResultMonad in
  let utlibs = failwith "TODO: extract `utlibs` from `package`" in

  (* Resolve dependency among the source files in the package: *)
  let* sorted_utlibs = ClosedFileDependencyResolver.main utlibs in

  (* Typecheck each source file: *)
  let* (_genv, libacc) =
    sorted_utlibs |> foldM (fun (genv, libacc) (abspath, utlib) ->
      let (header, (modident, utsig_opt, utbinds)) = utlib in
      let (_, modnm) = modident in
      let* ((_quant, ssig), binds) =
        let tyenv = tyenv_prim |> add_dependency_to_type_environment header genv in
        typecheck_library_file tyenv abspath utsig_opt utbinds
      in
      let genv = genv |> GlobalTypeenv.add modnm ssig in
      return (genv, Alist.extend libacc (abspath, binds))
    ) (genv, Alist.empty)
  in
  let libs = Alist.to_list libacc in

  (* TODO: check the main module *)
  let ssig = failwith "TODO: PackageChecker, check the main module" in
  return (ssig, libs)


let main_document (tyenv_prim : Typeenv.t) (genv : global_type_environment) sorted_locals =
  let open ResultMonad in
  let* (_, libacc, doc_opt) =
    sorted_locals |> foldM (fun (genv, libacc, doc_opt) (abspath, utsrc) ->
      match utsrc with
      | UTDocumentFile(header, utast) ->
          let* ast =
            let tyenv = tyenv_prim |> add_dependency_to_type_environment header genv in
            typecheck_document_file tyenv abspath utast
          in
          return (genv, libacc, Some(ast))

      | UTLibraryFile(header, (modident, utsig_opt, utbinds)) ->
          let (_, modnm) = modident in
          let* ((_quant, ssig), binds) =
            let tyenv = tyenv_prim |> add_dependency_to_type_environment header genv in
            typecheck_library_file tyenv abspath utsig_opt utbinds
          in
          let genv = genv |> GlobalTypeenv.add modnm ssig in
          return (genv, Alist.extend libacc (abspath, binds), doc_opt)

    ) (genv, Alist.empty, None)
  in
  let libs = Alist.to_list libacc in
  return (libs, doc_opt)
