
open MyUtil
open Types


exception CyclicFileDependency            of abs_path list
exception CannotReadFileOwingToSystem     of string
exception LibraryContainsWholeReturnValue of abs_path
exception DocumentLacksWholeReturnValue   of abs_path


module FileDependencyGraph = DirectedGraph.Make
  (struct
    type t = abs_path
    let compare ap1 ap2 = String.compare (get_abs_path_string ap1) (get_abs_path_string ap2)
    let show ap = Filename.basename (get_abs_path_string ap)
  end)


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
  match OptionState.get_mode () with
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


let rec register_library_file (dg : file_info FileDependencyGraph.t) (abspath : abs_path) : unit =
  begin
    Logging.begin_to_parse_file abspath;
    let curdir = Filename.dirname (get_abs_path_string abspath) in
    let inc = open_in_abs abspath in
    let (header, utsrc) = ParserInterface.process (basename_abs abspath) (Lexing.from_channel inc) in
    close_in inc;
    let lib =
      match utsrc with
      | UTLibraryFile(lib) -> lib
      | UTDocumentFile(_)  -> raise (LibraryContainsWholeReturnValue(abspath))
    in
    FileDependencyGraph.add_vertex dg abspath (LibraryFile(lib));
    header |> List.iter (fun headerelem ->
      let abspath_sub = get_abs_path_of_header curdir headerelem in
      begin
        if FileDependencyGraph.mem_vertex abspath_sub dg then () else
          register_library_file dg abspath_sub
      end;
      FileDependencyGraph.add_edge dg abspath abspath_sub
    )
  end


let register_document_file (dg : file_info FileDependencyGraph.t) (abspath_in : abs_path) : unit =
  Logging.begin_to_parse_file abspath_in;
  let file_in = open_in_abs abspath_in in
  let curdir = Filename.dirname (get_abs_path_string abspath_in) in
  let (header, utsrc) =
    ParserInterface.process (Filename.basename (get_abs_path_string abspath_in)) (Lexing.from_channel file_in)
  in
  let utast =
    match utsrc with
    | UTLibraryFile(_)      -> raise (DocumentLacksWholeReturnValue(abspath_in))
    | UTDocumentFile(utast) -> utast
  in
  FileDependencyGraph.add_vertex dg abspath_in (DocumentFile(utast));
  header |> List.iter (fun headerelem ->
    let file_path_sub = get_abs_path_of_header curdir headerelem in
    begin
      if FileDependencyGraph.mem_vertex file_path_sub dg then () else
        register_library_file dg file_path_sub
    end;
    FileDependencyGraph.add_edge dg abspath_in file_path_sub
  )


let register_markdown_file (dg : file_info FileDependencyGraph.t) (setting : string) (abspath_in : abs_path) : unit =
  Logging.begin_to_parse_file abspath_in;
  let abspath = Config.resolve_lib_file_exn (make_lib_path (Filename.concat "dist/md" (setting ^ ".satysfi-md"))) in
  let (cmdrcd, depends) = LoadMDSetting.main abspath in
  match MyUtil.string_of_file abspath_in with
  | Ok(data) ->
      let utast = DecodeMD.decode cmdrcd data in
      FileDependencyGraph.add_vertex dg abspath_in (DocumentFile(utast));
      depends |> List.iter (fun package ->
        let file_path_sub = get_package_abs_path package in
        begin
        if FileDependencyGraph.mem_vertex file_path_sub dg then () else
          register_library_file dg file_path_sub
        end;
        FileDependencyGraph.add_edge dg abspath_in file_path_sub
      )

  | Error(msg) ->
      raise (CannotReadFileOwingToSystem(msg))


let main (abspath_in : abs_path) =
  let dg = FileDependencyGraph.create 32 in
  begin
    match OptionState.get_input_kind () with
    | OptionState.SATySFi ->
        if has_library_extension abspath_in && OptionState.is_type_check_only () then
          register_library_file dg abspath_in
        else
          register_document_file dg abspath_in

    | OptionState.Markdown(setting) ->
        register_markdown_file dg setting abspath_in
  end;
  match FileDependencyGraph.find_cycle dg with
  | Some(cycle) ->
      raise (CyclicFileDependency(cycle))

  | None ->
      let inputs =
        FileDependencyGraph.backward_bfs_fold (fun inputacc abspath file_info ->
          Alist.extend inputacc (abspath, file_info)
        ) Alist.empty dg |> Alist.to_list
      in
      inputs
