
open MyUtil
open Types


type error =
  | CyclicFileDependency            of (abs_path * untyped_library_file) cycle
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


let rec register_library_file (graph : FileDependencyGraph.t) (package_names : PackageNameSet.t) ~prev:(vertex_prev_opt : FileDependencyGraph.vertex option) (abspath : abs_path) : (PackageNameSet.t * FileDependencyGraph.t) ok =
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
        match get_header curdir headerelem with
        | Package((_, main_module_name)) ->
            return (package_names |> PackageNameSet.add main_module_name, graph)

        | Local(_modident_sub, abspath_sub) ->
            register_library_file graph package_names ~prev:(Some(vertex)) abspath_sub
      ) (package_names, graph)


let register_document_file (graph : FileDependencyGraph.t) (package_names : PackageNameSet.t) (abspath_in : abs_path) : (PackageNameSet.t * FileDependencyGraph.t * untyped_document_file) ok =
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
      match get_header curdir headerelem with
      | Package((_, main_module_name)) ->
          return (package_names |> PackageNameSet.add main_module_name, graph)

      | Local(_, abspath_sub) ->
          register_library_file graph package_names ~prev:None abspath_sub
    ) (package_names, graph)
  in
  return (package_names, graph, utdoc)

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


let main (abspath_in : abs_path) : (PackageNameSet.t * (abs_path * untyped_library_file) list * untyped_document_file) ok =
  let open ResultMonad in
  let graph = FileDependencyGraph.empty in
  let package_names = PackageNameSet.empty in
  let* (package_names, graph, utdoc) =
    match OptionState.get_input_kind () with
    | OptionState.SATySFi ->
        if has_library_extension abspath_in && OptionState.is_type_check_only () then
          failwith "TODO: --type-check-only"
(*
          register_library_file graph package_names ~prev:None abspath_in
*)
        else
          register_document_file graph package_names abspath_in

    | OptionState.Markdown(_setting) ->
        failwith "TODO: Markdown"
(*
        register_markdown_file graph setting abspath_in
*)
  in
  let* sorted_locals =
    FileDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
  in
  return (package_names, sorted_locals, utdoc)
