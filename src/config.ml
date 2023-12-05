
open MyUtil


let satysfi_root_dirs : (string list) ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve fn =
  if Sys.file_exists fn then Some(fn) else None


(* Receives a file path relative to `LIBROOT`
   and returns its corresponding absolute path. *)
let resolve_lib_file (relpath : lib_path) : (abs_path, abs_path list) result =
  let open ResultMonad in
  let dirs = !satysfi_root_dirs in
  let relpathstr = get_lib_path_string relpath in
  let pathcands = dirs |> List.map (fun dir -> Filename.concat dir relpathstr) in
  match pathcands |> List.find_map resolve with
  | None          -> err (pathcands |> List.map make_abs_path)
  | Some(pathstr) -> return @@ make_abs_path pathstr


let resolve_local ~(extensions : string list) ~origin:(dir : string) ~relative:(s : string) =
  let open ResultMonad in
  let path_without_ext = Filename.concat dir s in
  let pathcands = extensions |> List.map (fun ext -> path_without_ext ^ ext) in
  match pathcands |> List.find_map resolve with
  | None          -> err (pathcands |> List.map make_abs_path)
  | Some(pathstr) -> return @@ make_abs_path pathstr
