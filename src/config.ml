
exception DistFileNotFound of string * string list


let satysfi_root_dirs : (string list) ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve fn =
  if Sys.file_exists fn then Some(fn) else None


let resolve_dist_file filename =
  let dirs = !satysfi_root_dirs in
  let pathcands =
    dirs |> List.map (fun dir -> Filename.concat dir filename)
  in
    match MyUtil.first_some resolve pathcands with
    | None     -> raise (DistFileNotFound(filename, pathcands))
    | Some(fn) -> fn


let resolve_dist_package package extcands =
  let withexts =
    extcands |> List.map (fun extcand -> package ^ extcand)
  in
  let dirs = !satysfi_root_dirs in
  let pathcands =
    dirs |> List.map (fun dir ->
      withexts |> List.map (fun withext ->
        Filename.concat dir withext
      )
    ) |> List.concat
  in
    match MyUtil.first_some resolve pathcands with
    | None     -> raise (DistFileNotFound(package, pathcands))
    | Some(fn) -> fn
