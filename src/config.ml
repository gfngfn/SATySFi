open MyUtil


let resolve fn =
  if Sys.file_exists fn then Some(fn) else None


let resolve_local ~(extensions : string list) ~origin:(dir : string) ~relative:(s : string) =
  let open ResultMonad in
  let path_without_ext = Filename.concat dir s in
  let pathcands = extensions |> List.map (fun ext -> path_without_ext ^ ext) in
  match pathcands |> List.find_map resolve with
  | None          -> err (pathcands |> List.map make_abs_path)
  | Some(pathstr) -> return @@ make_abs_path pathstr
