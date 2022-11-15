
open MyUtil
open Types
open PackageSystemBase
open ConfigError


type 'a ok = ('a, config_error) result


module FileDependencyGraph = DependencyGraph.Make(AbsPath)

type graph = untyped_library_file FileDependencyGraph.t

type vertex = FileDependencyGraph.Vertex.t

type local_or_package =
  | Local   of module_name ranged * abs_path
  | Package of module_name ranged


let get_header (extensions : string list) (curdir : string) (headerelem : header_element) : local_or_package ok =
  let open ResultMonad in
  match headerelem with
  | HeaderUsePackage{ module_name = modident; _ } ->
      return @@ Package(modident)

  | HeaderUse{ module_name = modident; _ } ->
      err @@ CannotUseHeaderUse(modident)

  | HeaderUseOf{ module_name = modident; path = s_relpath; _ } ->
      let* abspath =
        Config.resolve_local ~extensions ~origin:curdir ~relative:s_relpath
          |> Result.map_error (fun candidates -> LocalFileNotFound{ relative = s_relpath; candidates })
      in
      return @@ Local(modident, abspath)


let rec register_library_file (extensions : string list) (graph : graph) ~prev:(vertex_prev_opt : vertex option) (abspath : abs_path) : graph ok =
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
      let curdir = Filename.dirname (get_abs_path_string abspath) in
      let* utlib =
        Logging.begin_to_parse_file abspath;
        let* utsrc = ParserInterface.process_file abspath |> Result.map_error (fun rng -> FailedToParse(rng)) in
        match utsrc with
        | UTLibraryFile(utlib)    -> return utlib
        | UTDocumentFile(_, _, _) -> err @@ LibraryContainsWholeReturnValue(abspath)
      in
      let (_attrs, header, _) = utlib in
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
      header |> foldM (fun graph headerelem ->
        let* local_or_package = get_header extensions curdir headerelem in
        match local_or_package with
        | Package((_, _main_module_name)) ->
            return graph

        | Local(_modident_sub, abspath_sub) ->
            register_library_file extensions graph ~prev:(Some(vertex)) abspath_sub
      ) graph


let register_document_file (extensions : string list) (abspath_in : abs_path) : (graph * untyped_document_file) ok =
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
  let (_attrs, header, _) = utdoc in
  let* graph =
    header |> foldM (fun (graph) headerelem ->
      let* local_or_package = get_header extensions curdir headerelem in
      match local_or_package with
      | Package((_, _main_module_name)) ->
          return graph

      | Local(_, abspath_sub) ->
          register_library_file extensions graph ~prev:None abspath_sub
    ) FileDependencyGraph.empty
  in
  return (graph, utdoc)


let extract_markdown_command_record ~(module_name : module_name) (config : PackageConfig.t) : MarkdownParser.command_record ok =
  let open ResultMonad in
  match config.PackageConfig.package_contents with
  | PackageConfig.Library{ conversion_specs; _ } ->
      begin
        match
          conversion_specs |> List.filter_map (function
          | PackageConfig.MarkdownConversion(cmdrcd) ->
              Some(cmdrcd)
          )
        with
        | [] ->
            err @@ NoMarkdownConversion(module_name)

        | [ cmdrcd ] ->
            return cmdrcd

        | _ :: _ ->
            err @@ MoreThanOneMarkdownConversion(module_name)
      end

  | _ ->
      err @@ NoMarkdownConversion(module_name)


let register_markdown_file (configenv : PackageConfig.t GlobalTypeenv.t) (abspath_in : abs_path) : untyped_document_file ok =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
  let* (_docattr, main_module_name_class, md) =
    match read_file abspath_in with
    | Ok(data)   -> MarkdownParser.decode data |> Result.map_error (fun e -> MarkdownError(e))
    | Error(msg) -> err (CannotReadFileOwingToSystem(msg))
  in
  let* cmdrcd =
    match configenv |> GlobalTypeenv.find_opt main_module_name_class with
    | None ->
        err @@ MarkdownClassNotFound(main_module_name_class)

    | Some(config) ->
        extract_markdown_command_record ~module_name:main_module_name_class config
  in
  let utast = MarkdownParser.convert cmdrcd md in
  let header =
    [ HeaderUsePackage{ opening = false; module_name = (Range.dummy "md-header", main_module_name_class) } ]
  in
  let utdoc = ([], header, utast) in
  return utdoc


let main ~(extensions : string list) (input_kind : input_kind) (configenv : PackageConfig.t GlobalTypeenv.t) (abspath_in : abs_path) : ((abs_path * untyped_library_file) list * untyped_document_file) ok =
  let open ResultMonad in
  let* (graph, utdoc) =
    match input_kind with
    | InputSatysfi ->
        register_document_file extensions abspath_in

    | InputMarkdown ->
        let* utdoc = register_markdown_file configenv abspath_in in
        return (FileDependencyGraph.empty, utdoc)
  in
  let* sorted_locals =
    FileDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
  in
  return (sorted_locals, utdoc)
