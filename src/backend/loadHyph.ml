
type dir_path = string
type file_path = string

exception InvalidYOJSON               of file_path * string
exception OtherThanDictionary         of file_path
exception NotProvidingExceptionList   of file_path
exception ExceptionListOtherThanArray of file_path
exception InvalidExceptionElement     of file_path
exception NotProvidingPatternList     of file_path
exception PatternListOtherThanArray   of file_path
exception InvalidPatternElement       of file_path


module ExceptionMap = Map.Make(String)


let read_exception_list srcpath jsonarr =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.fold_left (fun mapacc json ->
        match json with
        | `Tuple[`String(wordfrom); `List(jsonlstto)] ->
            let fraclstto =
              jsonlstto |> List.map (function
                | `String(fracto) -> fracto
                | _               -> raise (InvalidExceptionElement(srcpath))
              )
            in
            mapacc |> ExceptionMap.add wordfrom fraclstto

        | _ ->
            raise (InvalidExceptionElement(srcpath))
      ) ExceptionMap.empty

  | _ ->
      raise (ExceptionListOtherThanArray(srcpath))


let read_pattern_list srcpath jsonarr =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.map (function
        | `String(strpat) ->
            failwith "remains to be implemented"  (* TEMPORARY *)

        | _ ->
            raise (InvalidPatternElement(srcpath))
      )

  | _ ->
      raise (PatternListOtherThanArray(srcpath))


let read_assoc (srcpath : file_path) assoc =
  let excpmap =
    match assoc |> List.assoc_opt "exceptions" with
    | None          -> raise (NotProvidingExceptionList(srcpath))
    | Some(jsonarr) -> read_exception_list srcpath jsonarr
  in
  let hyphpatlst =
    match assoc |> List.assoc_opt "patterns" with
    | None          -> raise (NotProvidingPatternList(srcpath))
    | Some(jsonarr) -> read_pattern_list srcpath jsonarr
  in
    (excpmap, hyphpatlst)


let main (satysfi_root_dir : dir_path) (filename : file_path) =
  let srcpath = Filename.concat satysfi_root_dir (Filename.concat "dist/hyph" filename) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
        match json with
        | `Assoc(assoc) -> read_assoc srcpath assoc
        | json_other    -> raise (OtherThanDictionary(srcpath))
    with
    | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))
