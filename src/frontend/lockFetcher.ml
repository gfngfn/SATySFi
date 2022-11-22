
open MyUtil
open PackageSystemBase


type error =
  | FailedToFetchTarball of {
      lock_name     : lock_name;
      exit_status   : int;
      fetch_command : string;
    }
  | FailedToExtractTarball of {
      lock_name          : lock_name;
      exit_status        : int;
      extraction_command : string;
    }


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let fetch_tarball ~(wget_command : string) ~(lock_name : lock_name) ~(url : string) ~(output : abs_path) : (unit, error) result =
  let open ResultMonad in
  let fetch_command =
    Printf.sprintf "\"%s\" -O \"%s\" \"%s\""
      (escape_string wget_command)
      (escape_string (get_abs_path_string output))
      (escape_string url)
  in
  let exit_status = Sys.command fetch_command in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToFetchTarball{ lock_name; exit_status; fetch_command }


let extract_tarball_with_components_stripped ~(tar_command : string) ~(lock_name : lock_name) ~tarball:(abspath_tarball : abs_path) ~lock_root:(absdir_lock_root : abs_path) =
  let open ResultMonad in
  let extraction_command =
    Printf.sprintf "\"%s\" -xzf \"%s\" -C \"%s\" --strip-components 1"
      (escape_string tar_command)
      (escape_string (get_abs_path_string abspath_tarball))
      (escape_string (get_abs_path_string absdir_lock_root))
  in
  let exit_status = Sys.command extraction_command in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToExtractTarball{ lock_name; exit_status; extraction_command }


let main ~(wget_command : string) ~(tar_command : string) ~cache_directory:(absdir_lock_cache : abs_path) (impl_spec : implementation_spec) : (unit, error) result =
  let open ResultMonad in
  let ImplSpec{ lock_name; container_directory; source } = impl_spec in
  let absdirstr_container = get_abs_path_string container_directory in
  let absdirstr_lock_root = Filename.concat absdirstr_container lock_name in

  (* Creates the lock cache directory if non-existent, or does nothing otherwise: *)
  Core_unix.mkdir_p (get_abs_path_string absdir_lock_cache);

  (* Creates the directory if non-existent, or does nothing otherwise: *)
  Core_unix.mkdir_p absdirstr_lock_root;

  if Sys.file_exists (Filename.concat absdirstr_lock_root "satysfi.yaml") then begin
  (* If the lock has already been fetched: *)
    Logging.lock_already_installed lock_name (make_abs_path absdirstr_lock_root);
    return ()
  end else begin
    match source with
    | NoSource ->
        return ()

    | TarGzip{ url } ->
        (* Synchronously fetches a tarball: *)
        let abspath_tarball =
          make_abs_path
            (Filename.concat (get_abs_path_string absdir_lock_cache) (Printf.sprintf "%s.tar.gz" lock_name))
        in
        let* () =
          if Sys.file_exists (get_abs_path_string abspath_tarball) then begin
            Logging.lock_cache_exists lock_name abspath_tarball;
            return ()
          end else begin
            Logging.downloading_lock lock_name abspath_tarball;
            fetch_tarball ~wget_command ~lock_name ~url ~output:abspath_tarball
          end
        in

        (* Extract sources from the tarball: *)
        let absdir_lock_root = make_abs_path absdirstr_lock_root in
        let* () =
          extract_tarball_with_components_stripped
            ~tar_command ~lock_name ~tarball:abspath_tarball ~lock_root:absdir_lock_root
        in

        return ()
  end
