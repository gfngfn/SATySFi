
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result


let has_library_extension (abspath : abs_path) : bool =
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


type local_or_package =
  | Local   of module_name ranged * abs_path
  | Package of module_name ranged


let get_header (extensions : string list) (curdir : string) (headerelem : header_element) : local_or_package ok =
  let open ResultMonad in
  match headerelem with
  | HeaderUsePackage(modident) ->
      return @@ Package(modident)

  | HeaderUse(modident) ->
      err @@ CannotUseHeaderUse(modident)

  | HeaderUseOf(modident, s_relpath) ->
      let* abspath = Config.resolve_local ~extensions ~origin:curdir ~relative:s_relpath in
      return @@ Local(modident, abspath)


let rec register_library_file (extensions : string list) (graph : FileDependencyGraph.t) (package_names : PackageNameSet.t) ~prev:(vertex_prev_opt : FileDependencyGraph.vertex option) (abspath : abs_path) : (PackageNameSet.t * FileDependencyGraph.t) ok =
  let open ResultMonad in
  match graph |> FileDependencyGraph.get_vertex abspath with
  | Some(vertex) ->
    (* If `abspath` has already been parsed: *)
      let graph =
        match vertex_prev_opt with
        | None              -> graph
        | Some(vertex_prev) -> graph |> FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
      in
      return (package_names, graph)

  | None ->
      let curdir = Filename.dirname (get_abs_path_string abspath) in
      let* utlib =
        Logging.begin_to_parse_file abspath;
        let* utsrc = ParserInterface.process_file abspath |> Result.map_error (fun rng -> FailedToParse(rng)) in
        match utsrc with
        | UTLibraryFile(utlib) -> return utlib
        | UTDocumentFile(_, _) -> err @@ LibraryContainsWholeReturnValue(abspath)
      in
      let (header, _) = utlib in
      let (graph, vertex) =
        match graph |> FileDependencyGraph.add_vertex abspath utlib with
        | Error(_vertex) -> assert false
        | Ok(pair)       -> pair
      in
      let graph =
        match vertex_prev_opt with
        | None              -> graph
        | Some(vertex_prev) -> graph |> FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
      in
      header |> foldM (fun (package_names, graph) headerelem ->
        let* local_or_package = get_header extensions curdir headerelem in
        match local_or_package with
        | Package((_, main_module_name)) ->
            return (package_names |> PackageNameSet.add main_module_name, graph)

        | Local(_modident_sub, abspath_sub) ->
            register_library_file extensions graph package_names ~prev:(Some(vertex)) abspath_sub
      ) (package_names, graph)


let register_document_file (extensions : string list) (abspath_in : abs_path) : (PackageNameSet.t * FileDependencyGraph.t * untyped_document_file) ok =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
  let curdir = Filename.dirname (get_abs_path_string abspath_in) in
  let* utsrc =
    ParserInterface.process_file abspath_in
      |> Result.map_error (fun rng -> FailedToParse(rng))
  in
  let* utdoc =
    match utsrc with
    | UTLibraryFile(_)      -> err @@ DocumentLacksWholeReturnValue(abspath_in)
    | UTDocumentFile(utdoc) -> return utdoc
  in
  let (header, _) = utdoc in
  let* (package_names, graph) =
    header |> foldM (fun (package_names, graph) headerelem ->
      let* local_or_package = get_header extensions curdir headerelem in
      match local_or_package with
      | Package((_, main_module_name)) ->
          return (package_names |> PackageNameSet.add main_module_name, graph)

      | Local(_, abspath_sub) ->
          register_library_file extensions graph package_names ~prev:None abspath_sub
    ) (PackageNameSet.empty, FileDependencyGraph.empty)
  in
  return (package_names, graph, utdoc)


let register_markdown_file (setting : string) (abspath_in : abs_path) : (PackageNameSet.t * untyped_document_file) ok =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
  let* abspath = Config.resolve_lib_file (make_lib_path (Filename.concat "dist/md" (setting ^ ".satysfi-md"))) in
  let (cmdrcd, depends) = LoadMDSetting.main abspath in (* TODO: make this monadic *)
  let* utast =
    match MyUtil.string_of_file abspath_in with
    | Ok(data)   -> return (DecodeMD.decode cmdrcd data)
    | Error(msg) -> err (CannotReadFileOwingToSystem(msg))
  in
  let package_names =
    depends |> List.fold_left (fun package_names main_module_name ->
      package_names |> PackageNameSet.add main_module_name
    ) PackageNameSet.empty
  in
  let header =
    depends |> List.map (fun main_module_name ->
      HeaderUsePackage((Range.dummy "md-header", main_module_name))
    )
  in
  return (package_names, (header, utast))


let main ~(extensions : string list) (abspath_in : abs_path) : (PackageNameSet.t * (abs_path * untyped_library_file) list * untyped_document_file option) ok =
  let open ResultMonad in
  let* (package_names, graph, utdoc_opt) =
    match OptionState.get_input_kind () with
    | OptionState.SATySFi ->
        if has_library_extension abspath_in && OptionState.is_type_check_only () then
          let graph = FileDependencyGraph.empty in
          let package_names = PackageNameSet.empty in
          let* (package_names, graph) =
            register_library_file extensions graph package_names ~prev:None abspath_in
          in
          return (package_names, graph, None)
        else
          let* (package_names, graph, utdoc) = register_document_file extensions abspath_in in
          return (package_names, graph, Some(utdoc))

    | OptionState.Markdown(setting) ->
        let* (package_names, utdoc) = register_markdown_file setting abspath_in in
        return (package_names, FileDependencyGraph.empty, Some(utdoc))
  in
  let* sorted_locals =
    FileDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
  in
  return (package_names, sorted_locals, utdoc_opt)
