
open MyUtil
open ConfigError


let satysfi_root_dirs : (string list) ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve fn =
  if Sys.file_exists fn then Some(fn) else None


let resolve_directory fn =
  try
    if Sys.is_directory fn then Some(make_abs_path fn) else None
  with
  | Sys_error(_) -> None


(* Receives a file path relative to `LIBROOT`
   and returns its corresponding absolute path. *)
let resolve_lib_file (relpath : lib_path) : (abs_path, config_error) result =
  let dirs = !satysfi_root_dirs in
  let relpathstr = get_lib_path_string relpath in
  let pathcands =
    dirs |> List.map (fun dir -> Filename.concat dir relpathstr)
  in
  match first_some resolve pathcands with
  | Some(abspathstr) -> Ok(make_abs_path abspathstr)
  | None             -> Error(CannotFindLibraryFile(relpath, pathcands))


let resolve_package_directory (main_module_name : string) =
  let open ResultMonad in
  let dirs = !satysfi_root_dirs in
  let pathcands_local =
    dirs |> List.map (fun dir ->
      Filename.concat (Filename.concat dir "local/packages") main_module_name
    )
  in
  let pathcands_dist =
    dirs |> List.map (fun dir ->
      Filename.concat (Filename.concat dir "dist/packages") main_module_name
    )
  in
  match MyUtil.first_some resolve_directory (List.append pathcands_local pathcands_dist) with
  | None    -> err (List.append pathcands_local pathcands_dist)
  | Some(p) -> return p


let resolve_local ~(extensions : string list) ~origin:(dir : string) ~relative:(s : string) =
  let open ResultMonad in
  let path_without_ext = Filename.concat dir s in
  let pathcands = extensions |> List.map (fun ext -> path_without_ext ^ ext) in
  match first_some resolve pathcands with
  | None          -> err @@ LocalFileNotFound{ relative = s; candidates = pathcands }
  | Some(pathstr) -> return @@ make_abs_path pathstr
