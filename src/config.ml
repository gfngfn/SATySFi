
open MyUtil

exception PackageNotFound      of string * abs_path list
exception LibraryFileNotFound  of lib_path * abs_path list
exception LibraryFilesNotFound of lib_path list * abs_path list
exception ImportedFileNotFound of string * abs_path list


let satysfi_root_dirs : (string list) ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve fn =
  if Sys.file_exists fn then Some(fn) else None


(* --
   `resolve_lib_file` receives a file path relative to `LIBROOT`
   and returns its corresponding absolute path.
   -- *)
let resolve_lib_file_scheme (relpath : lib_path) : string option * string list =
  let dirs = !satysfi_root_dirs in
  let relpathstr = get_lib_path_string relpath in
  let pathcands =
    dirs |> List.map (fun dir -> Filename.concat dir relpathstr)
  in
  (first_some resolve pathcands, pathcands)


let resolve_lib_file_opt (relpath : lib_path) : abs_path option =
  let (opt, _) = resolve_lib_file_scheme relpath in
  opt |> BatOption.map make_abs_path


let resolve_lib_file_exn (relpath : lib_path) : abs_path =
  let (opt, pathcands) = resolve_lib_file_scheme relpath in
  match opt with
  | None             -> raise (LibraryFileNotFound(relpath, pathcands |> List.map make_abs_path))
  | Some(abspathstr) -> make_abs_path abspathstr


let resolve_lib_file_from_candidates_exn (relpaths : lib_path list) : abs_path =
  let rec aux candacc = function
    | [] ->
        raise (LibraryFilesNotFound(relpaths, candacc |> Alist.to_list |> List.map make_abs_path))

    | relpath :: tail ->
        let (opt, pathcands) = resolve_lib_file_scheme relpath in
        begin
          match opt with
          | Some(abspathstr) -> make_abs_path abspathstr
          | None             -> aux (Alist.append candacc pathcands) tail
        end
  in
  aux Alist.empty relpaths


let resolve_package_res (relbasename : string) (extcands : string list) : (abs_path, abs_path list) result =
  let withexts =
    extcands |> List.map (fun extcand -> relbasename ^ extcand)
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
  | None    -> Error(pathcands |> List.map make_abs_path)
  | Some(p) -> Ok(make_abs_path p)


(* --
   `resolve_package_exn` receives

   - `package`: a package name (i.e. a relative path from `LIBROOT/local/packages` or `LIBROOT/dist/packages` without file extension)
   - `extcands`: a list of candidates of the file extension (first match, i.e., earlier entry has the higher priority)

   and returns the absolute path of the package.
   -- *)
let resolve_package_exn package extcands =
  match resolve_package_res (Filename.concat "local/packages" package) extcands with
  | Error(pathcands_local) ->
      begin
        match resolve_package_res (Filename.concat "dist/packages" package) extcands with
        | Error(pathcands_dist) ->
            let pathcands = List.append pathcands_local pathcands_dist in
            raise (PackageNotFound(package, pathcands))

        | Ok(fn_dist) ->
            fn_dist
      end

  | Ok(fn_local) ->
      fn_local


let resolve_local_exn dir s extcands =
  let pathwithoutext = Filename.concat dir s in
  let pathcands = extcands |> List.map (fun ext -> pathwithoutext ^ "." ^ ext) in
  match first_some resolve pathcands with
  | None          -> raise (ImportedFileNotFound(s, pathcands |> List.map make_abs_path))
  | Some(pathstr) -> make_abs_path pathstr
