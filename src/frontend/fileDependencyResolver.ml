
open MyUtil
open Types


exception CyclicFileDependency            of (abs_path * file_info) cycle
exception CannotReadFileOwingToSystem     of string
exception LibraryContainsWholeReturnValue of abs_path
exception DocumentLacksWholeReturnValue   of abs_path


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


let get_candidate_file_extensions () =
  match OptionState.get_output_mode () with
  | PdfMode           -> [ ".satyh"; ".satyg" ]
  | TextMode(formats) -> List.append (formats |> List.map (fun s -> ".satyh-" ^ s)) [ ".satyg" ]


let get_package_abs_path (package : string) : abs_path =
  let extcands = get_candidate_file_extensions () in
  Config.resolve_package_exn package extcands


let get_abs_path_of_header (curdir : string) (headerelem : header_element) : abs_path =
  match headerelem with
  | HeaderRequire(package) ->
      get_package_abs_path package

  | HeaderImport(s) ->
      let extcands = get_candidate_file_extensions () in
      Config.resolve_local_exn curdir s extcands


let rec register_library_file (graph : FileDependencyGraph.t) ~prev:(vertex_prev : FileDependencyGraph.vertex) (abspath : abs_path) : FileDependencyGraph.t =
  begin
    Logging.begin_to_parse_file abspath;
    let curdir = Filename.dirname (get_abs_path_string abspath) in
    let inc = open_in_abs abspath in
    let (header, utsrc) = ParserInterface.process (basename_abs abspath) (Sedlexing.Utf8.from_channel inc) in
    close_in inc;
    let lib =
      match utsrc with
      | UTLibraryFile(lib) -> lib
      | UTDocumentFile(_)  -> raise (LibraryContainsWholeReturnValue(abspath))
    in
    let (graph, vertex) =
      match graph |> FileDependencyGraph.add_vertex abspath (LibraryFile(lib)) with
      | Error(_) -> assert false
      | Ok(pair) -> pair
    in
    let graph = FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex graph in
    header |> List.fold_left (fun graph headerelem ->
      let abspath_sub = get_abs_path_of_header curdir headerelem in
      match graph |> FileDependencyGraph.get_vertex abspath_sub with
      | Some(vertex_sub) ->
        (* If `abs_path` has already been parsed *)
          graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub

      | None ->
          register_library_file graph ~prev:vertex abspath_sub

    ) graph
  end


let register_document_file (graph : FileDependencyGraph.t) (abspath_in : abs_path) : FileDependencyGraph.t =
  Logging.begin_to_parse_file abspath_in;
  let file_in = open_in_abs abspath_in in
  let curdir = Filename.dirname (get_abs_path_string abspath_in) in
  let (header, utsrc) =
    ParserInterface.process (Filename.basename (get_abs_path_string abspath_in)) (Sedlexing.Utf8.from_channel file_in)
  in
  let utast =
    match utsrc with
    | UTLibraryFile(_)      -> raise (DocumentLacksWholeReturnValue(abspath_in))
    | UTDocumentFile(utast) -> utast
  in
  let (graph, vertex) =
    match graph |> FileDependencyGraph.add_vertex abspath_in (DocumentFile(utast)) with
    | Error(_) -> assert false
    | Ok(pair) -> pair
  in
  header |> List.fold_left (fun graph headerelem ->
    let abspath_sub = get_abs_path_of_header curdir headerelem in
    match graph |> FileDependencyGraph.get_vertex abspath_sub with
    | Some(vertex_sub) ->
        graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub

    | None ->
        register_library_file graph ~prev:vertex abspath_sub

  ) graph


let register_markdown_file (graph : FileDependencyGraph.t) (setting : string) (abspath_in : abs_path) : FileDependencyGraph.t =
  Logging.begin_to_parse_file abspath_in;
  let (cmdrcd, depends) =
    let abspath =
      Config.resolve_lib_file_exn (make_lib_path (Filename.concat "dist/md" (setting ^ ".satysfi-md")))
    in
    LoadMDSetting.main abspath
  in
  let utast =
    match MyUtil.string_of_file abspath_in with
    | Ok(data)   -> DecodeMD.decode cmdrcd data
    | Error(msg) -> raise (CannotReadFileOwingToSystem(msg))
  in
  let (graph, vertex) =
    match graph |> FileDependencyGraph.add_vertex abspath_in (DocumentFile(utast)) with
    | Error(_) -> assert false
    | Ok(pair) -> pair
  in
  depends |> List.fold_left (fun graph package ->
    let abspath_sub = get_package_abs_path package in
    match graph |> FileDependencyGraph.get_vertex abspath_sub with
    | Some(vertex_sub) ->
        graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub

    | None ->
        register_library_file graph ~prev:vertex abspath_sub

  ) graph



let main (abspath_in : abs_path) =
  let graph = FileDependencyGraph.empty in
  let graph =
    match OptionState.get_input_kind () with
    | OptionState.SATySFi ->
        if has_library_extension abspath_in && OptionState.is_type_check_only () then
          let vertex = failwith "TODO" in
          register_library_file graph ~prev:vertex abspath_in
        else
          register_document_file graph abspath_in

    | OptionState.Markdown(setting) ->
        register_markdown_file graph setting abspath_in
  in
  match FileDependencyGraph.topological_sort graph with
  | Error(cycle) ->
      raise (CyclicFileDependency(cycle))

  | Ok(inputs) ->
      inputs
