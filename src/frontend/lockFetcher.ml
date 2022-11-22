
open MyUtil
open PackageSystemBase


type error =
  | FetchFailed of {
      lock_name     : lock_name;
      exit_status   : int;
      fetch_command : string;
    }


let fetch_tarball ~(wget_command : string) ~(lock_name : lock_name) ~(url : string) ~(output : abs_path) : (unit, error) result =
  let open ResultMonad in
  let fetch_command = Printf.sprintf "\"%s\" -O \"%s\" \"%s\"" wget_command (get_abs_path_string output) url in
  let exit_status = Sys.command fetch_command in
  if exit_status = 0 then
    return ()
  else
    err @@ FetchFailed{ lock_name; exit_status; fetch_command }


let main ~(wget_command : string) (impl_spec : implementation_spec) : (unit, error) result =
  let open ResultMonad in
  let ImplSpec{ lock_name; container_directory; source } = impl_spec in
  let absdirstr_container = get_abs_path_string container_directory in
  let absdirstr_lock_root = Filename.concat absdirstr_container lock_name in

  (* Creates the directory if non-existent, or does nothing otherwise: *)
  Core_unix.mkdir_p absdirstr_lock_root;

  if Sys.file_exists (Filename.concat absdirstr_lock_root "package.yaml") then begin
  (* If the lock has already been fetched: *)
    Logging.lock_already_installed lock_name (make_abs_path absdirstr_lock_root);
    return ()
  end else begin
    Logging.installing_lock lock_name (make_abs_path absdirstr_lock_root);
    match source with
    | NoSource ->
        return ()

    | TarGzip{ url } ->
        (* Synchronously fetches a tarball: *)
        let abspath_tarball =
          make_abs_path (Filename.concat absdirstr_container (Printf.sprintf "%s.tar.gz" lock_name))
        in
        let* () = fetch_tarball ~wget_command ~lock_name ~url ~output:abspath_tarball in

        (* TODO: extract sources from the tarball here *)
        return ()
  end
