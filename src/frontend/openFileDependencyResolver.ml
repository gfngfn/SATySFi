
open MyUtil
open EnvelopeSystemBase
open Types
open ConfigError


type 'a ok = ('a, config_error) result


module FileDependencyGraph = DependencyGraph.Make(AbsPath)

type graph = untyped_library_file FileDependencyGraph.t

type vertex = FileDependencyGraph.Vertex.t

type local_or_envelope =
  | Local    of module_name_chain ranged * abs_path
  | Envelope of module_name_chain ranged


let resolve_local ~(origin_dir : abs_path) ~(relpath_without_ext : string) ~(extensions : string list) =
  let open ResultMonad in
  let path_without_ext = Filename.concat (get_abs_path_string origin_dir) relpath_without_ext in
  let pathcands = extensions |> List.map (fun ext -> path_without_ext ^ ext) in
  match
    pathcands |> List.find_map (fun pathcand ->
      if Sys.file_exists pathcand then Some(pathcand) else None
    )
  with
  | None          -> err (pathcands |> List.map make_abs_path)
  | Some(pathstr) -> return @@ make_abs_path pathstr


let get_header (extensions : string list) (absdir_current : abs_path) (headerelem : header_element) : local_or_envelope ok =
  let open ResultMonad in
  match headerelem with
  | HeaderUsePackage{ mod_chain; _ } ->
      return @@ Envelope(mod_chain)

  | HeaderUse{ mod_chain; _ } ->
      err @@ CannotUseHeaderUse(mod_chain)

  | HeaderUseOf{ mod_chain; relpath_without_ext; _ } ->
      let* abspath =
        resolve_local ~origin_dir:absdir_current ~relpath_without_ext ~extensions
          |> Result.map_error (fun candidates -> LocalFileNotFound{ relative = relpath_without_ext; candidates })
      in
      return @@ Local(mod_chain, abspath)


let rec register_library_file (display_config : Logging.config) (extensions : string list) (graph : graph) ~prev:(vertex_prev_opt : vertex option) (abspath : abs_path) : graph ok =
  let open ResultMonad in
  match graph |> FileDependencyGraph.get_vertex abspath with
  | Some(vertex) ->
    (* If `abspath` has already been parsed: *)
      let graph =
        match vertex_prev_opt with
        | None              -> graph
        | Some(vertex_prev) -> graph |> FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
      in
      return graph

  | None ->
      let absdir_current = make_abs_path (Filename.dirname (get_abs_path_string abspath)) in
      let* utlib =
        Logging.begin_to_parse_file display_config abspath;
        let* utsrc = ParserInterface.process_file abspath |> Result.map_error (fun rng -> FailedToParse(rng)) in
        match utsrc with
        | UTLibraryFile(utlib)    -> return utlib
        | UTDocumentFile(_, _, _) -> err @@ LibraryContainsWholeReturnValue(abspath)
      in
      let (_attrs, header, _) = utlib in
      let (graph, vertex) =
        match graph |> FileDependencyGraph.add_vertex abspath utlib with
        | Error(_vertex) -> assert false (* Freshness has been asserted by `get_vertex`. *)
        | Ok(pair)       -> pair
      in
      let graph =
        match vertex_prev_opt with
        | None              -> graph
        | Some(vertex_prev) -> graph |> FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
      in
      header |> foldM (fun graph headerelem ->
        let* local_or_envelope = get_header extensions absdir_current headerelem in
        match local_or_envelope with
        | Envelope((_, _main_module_name)) ->
            return graph

        | Local(_modident_sub, abspath_sub) ->
            register_library_file display_config extensions graph ~prev:(Some(vertex)) abspath_sub
      ) graph


let register_document_file (display_config : Logging.config) (extensions : string list) (abspath_in : abs_path) : (graph * untyped_document_file) ok =
  let open ResultMonad in
  Logging.begin_to_parse_file display_config abspath_in;
  let absdir_doc = make_abs_path (Filename.dirname (get_abs_path_string abspath_in)) in
  let* utsrc =
    ParserInterface.process_file abspath_in
      |> Result.map_error (fun rng -> FailedToParse(rng))
  in
  let* utdoc =
    match utsrc with
    | UTLibraryFile(_)      -> err @@ DocumentLacksWholeReturnValue(abspath_in)
    | UTDocumentFile(utdoc) -> return utdoc
  in
  let (_attrs, header, _) = utdoc in
  let* graph =
    header |> foldM (fun (graph) headerelem ->
      let* local_or_envelope = get_header extensions absdir_doc headerelem in
      match local_or_envelope with
      | Envelope((_, _main_module_name)) ->
          return graph

      | Local(_, abspath_sub) ->
          register_library_file display_config extensions graph ~prev:None abspath_sub
    ) FileDependencyGraph.empty
  in
  return (graph, utdoc)


let extract_markdown_conversion (envelope_config : EnvelopeConfig.t) : markdown_conversion ok =
  let open ResultMonad in
  let { envelope_contents } = envelope_config in
  match envelope_contents with
  | Library{ markdown_conversion = Some(conv); _ } -> return conv
  | _                                              -> err NoMarkdownConversion


let register_markdown_file (display_config : Logging.config) (configenv : EnvelopeConfig.t GlobalTypeenv.t) (used_as_map : envelope_name ModuleNameMap.t) (abspath_in : abs_path) : untyped_document_file ok =
  let open ResultMonad in
  Logging.begin_to_parse_file display_config abspath_in;
  let* md =
    match read_file abspath_in with
    | Ok(data)   -> MarkdownParser.decode data |> Result.map_error (fun e -> MarkdownError(e))
    | Error(msg) -> err (CannotReadFileOwingToSystem(msg))
  in
  let class_module_name = MarkdownParser.get_class_module_name md in
  let* class_envelope_name =
    match used_as_map |> ModuleNameMap.find_opt class_module_name with
    | Some(class_envelope_name) -> return class_envelope_name
    | None                      -> err @@ UnknownPackageDependency(Range.dummy "register_markdown_file", class_module_name)
  in
  let* conv =
    match configenv |> GlobalTypeenv.find_opt (EnvelopeName.EN(class_envelope_name)) with
    | None                  -> err @@ MarkdownClassNotFound
    | Some(envelope_config) -> extract_markdown_conversion envelope_config
  in
  let utast = MarkdownParser.convert conv md in
  let header =
    [
      HeaderUsePackage{
        opening   = false;
        mod_chain = (Range.dummy "md-header", ((Range.dummy "md-header", class_module_name), []));
      };
    ]
  in
  let utdoc = ([], header, utast) in
  return utdoc


let main (display_config : Logging.config) ~(extensions : string list) (input_kind : input_kind) (configenv : EnvelopeConfig.t GlobalTypeenv.t) ~(used_as_map : envelope_name ModuleNameMap.t) (abspath_in : abs_path) : ((abs_path * untyped_library_file) list * untyped_document_file) ok =
  let open ResultMonad in
  let* (graph, utdoc) =
    match input_kind with
    | InputSatysfi ->
        register_document_file display_config extensions abspath_in

    | InputMarkdown ->
        let* utdoc = register_markdown_file display_config configenv used_as_map abspath_in in
        return (FileDependencyGraph.empty, utdoc)
  in
  let* sorted_locals =
    FileDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
  in
  return (sorted_locals, utdoc)
