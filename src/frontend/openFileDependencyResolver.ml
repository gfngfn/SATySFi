
open MyUtil
open Types


type error =
  | CyclicFileDependency            of (abs_path * file_info) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | FailedToParse                   of Range.t

type 'a ok = ('a, error) result


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


(*
let get_package_abs_path (package : string) : abs_path =
  let extcands = get_candidate_file_extensions () in
  Config.resolve_package_exn package extcands
*)


type local_or_package =
  | Local   of module_name ranged * abs_path
  | Package of module_name ranged


let get_header (curdir : string) (headerelem : header_element) : local_or_package =
  match headerelem with
  | HeaderUsePackage(modident) ->
      Package(modident)

  | HeaderUse(_) ->
      failwith "TODO (error): cannot use 'use X' here; use 'use X of path' instead"

  | HeaderUseOf(modident, s_relpath) ->
      let extcands = get_candidate_file_extensions () in
      let abspath = Config.resolve_local_exn curdir s_relpath extcands in
      Local(modident, abspath)


let rec register_library_file (graph : FileDependencyGraph.t) (packages : PackageNameSet.t) ~prev:(vertex_prev : FileDependencyGraph.vertex) (abspath : abs_path) : (FileDependencyGraph.t * PackageNameSet.t) ok =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath;
  let curdir = Filename.dirname (get_abs_path_string abspath) in
  let* (header, utsrc) =
    ParserInterface.process_file abspath
      |> Result.map_error (fun rng -> FailedToParse(rng))
  in
  let* lib =
    match utsrc with
    | UTLibraryFile(lib) -> return lib
    | UTDocumentFile(_)  -> err @@ LibraryContainsWholeReturnValue(abspath)
  in
  let (graph, vertex) =
    match graph |> FileDependencyGraph.add_vertex abspath (LibraryFile(lib)) with
    | Error(_) -> assert false
    | Ok(pair) -> pair
  in
  let graph = FileDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex graph in
    header |> foldM (fun (graph, packages) headerelem ->
      match get_header curdir headerelem with
      | Package((_, main_module_name)) ->
          return (graph, packages |> PackageNameSet.add main_module_name)

      | Local(_modident_sub, abspath_sub) ->
          begin
            match graph |> FileDependencyGraph.get_vertex abspath_sub with
            | Some(vertex_sub) ->
              (* If `abs_path` has already been parsed *)
                let graph = graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub in
                return (graph, packages)

            | None ->
                register_library_file graph packages ~prev:vertex abspath_sub
          end

    ) (graph, packages)


let register_document_file (graph : FileDependencyGraph.t) (packages : PackageNameSet.t) (abspath_in : abs_path) : (FileDependencyGraph.t * PackageNameSet.t) ok =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
  let curdir = Filename.dirname (get_abs_path_string abspath_in) in
  let* (header, utsrc) =
    ParserInterface.process_file abspath_in
      |> Result.map_error (fun rng -> FailedToParse(rng))
  in
  let* utast =
    match utsrc with
    | UTLibraryFile(_)      -> err @@ DocumentLacksWholeReturnValue(abspath_in)
    | UTDocumentFile(utast) -> return utast
  in
  let (graph, vertex) =
    match graph |> FileDependencyGraph.add_vertex abspath_in (DocumentFile(utast)) with
    | Error(_) -> assert false
    | Ok(pair) -> pair
  in
  header |> foldM (fun (graph, packages) headerelem ->
    match get_header curdir headerelem with
    | Package((_, main_module_name)) ->
        return (graph, packages |> PackageNameSet.add main_module_name)

    | Local(_, abspath_sub) ->
        begin
          match graph |> FileDependencyGraph.get_vertex abspath_sub with
          | Some(vertex_sub) ->
              let graph = graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub in
              return (graph, packages)

          | None ->
              register_library_file graph packages ~prev:vertex abspath_sub
        end

  ) (graph, packages)


(*
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
*)


let main (abspath_in : abs_path) : ((abs_path * file_info) list * PackageNameSet.t) ok =
  let open ResultMonad in
  let graph = FileDependencyGraph.empty in
  let packages = PackageNameSet.empty in
  let* (graph, packages) =
    match OptionState.get_input_kind () with
    | OptionState.SATySFi ->
        if has_library_extension abspath_in && OptionState.is_type_check_only () then
          let vertex = failwith "TODO: type-check-only" in
          register_library_file graph packages ~prev:vertex abspath_in
        else
          register_document_file graph packages abspath_in

    | OptionState.Markdown(_setting) ->
        failwith "TODO: Markdown"
(*
        register_markdown_file graph setting abspath_in
*)
  in
  begin
    FileDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
  end >>= fun inputs ->
  return (inputs, packages)
