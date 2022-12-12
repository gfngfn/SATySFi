
open MyUtil
open PackageSystemBase
open ConfigError


type 'a ok = ('a, config_error) result


let fetch_tarball ~(wget_command : string) ~(lock_name : lock_name) ~(url : string) ~(output : abs_path) : unit ok =
  let open ResultMonad in
  let ShellCommand.{ exit_status; command } = ShellCommand.run_wget ~wget_command ~url ~output in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToFetchTarball{ lock_name; exit_status; command }


let extract_tarball_with_components_stripped ~(tar_command : string) ~(lock_name : lock_name) ~(tarball : abs_path) ~(lock_root : abs_path) =
  let open ResultMonad in
  let ShellCommand.{ exit_status; command } =
    ShellCommand.run_tar_xzf_strip_components_1 ~tar_command ~tarball ~output_dir:lock_root
  in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToExtractTarball{ lock_name; exit_status; command }


let fetch_external_zip ~(wget_command : string) ~(url : string) ~(output : abs_path) : unit ok =
  let open ResultMonad in
  let ShellCommand.{ exit_status; command } =
    ShellCommand.run_wget ~wget_command ~url ~output
  in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToFetchExternalZip{ url; exit_status; command }


let extract_external_zip ~(unzip_command : string) ~(zip : abs_path) ~(output_container_dir : abs_path) =
  let open ResultMonad in
  let ShellCommand.{ exit_status; command } =
    ShellCommand.run_unzip ~unzip_command ~zip ~output_container_dir
  in
  if exit_status = 0 then
    return ()
  else
    err @@ FailedToExtractExternalZip{ exit_status; command }


let main ~(wget_command : string) ~(tar_command : string) ~(unzip_command : string) ~cache_directory:(absdir_lock_cache : abs_path) (impl_spec : implementation_spec) : unit ok =
  let open ResultMonad in
  let ImplSpec{ lock_name; container_directory; source } = impl_spec in
  let absdirstr_container = get_abs_path_string container_directory in
  let absdir_lock_root = make_abs_path (Filename.concat absdirstr_container lock_name) in

  (* Creates the lock cache directory if non-existent, or does nothing otherwise: *)
  ShellCommand.mkdir_p absdir_lock_cache;

  (* Creates the directory if non-existent, or does nothing otherwise: *)
  ShellCommand.mkdir_p absdir_lock_root;

  let abspathstr_config =
    Filename.concat (get_abs_path_string absdir_lock_root) Constant.package_config_file_name
  in
  if Sys.file_exists abspathstr_config then begin
  (* If the lock has already been fetched: *)
    Logging.lock_already_installed lock_name absdir_lock_root;
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

        (* Extracts sources from the tarball: *)
        let* () =
          extract_tarball_with_components_stripped
            ~tar_command ~lock_name ~tarball:abspath_tarball ~lock_root:absdir_lock_root
        in

        (* Fetches external sources according to the package config: *)
        let* PackageConfig.{ external_sources; _ } = PackageConfig.load absdir_lock_root in
        let* () =
          external_sources |> foldM (fun () (name, external_source) ->
            match external_source with
            | ExternalZip{ url; checksum; extractions } ->

                (* Creates a directory for putting zips: *)
                let absdir_external =
                  make_abs_path (Filename.concat (get_abs_path_string absdir_lock_cache) lock_name)
                in
                ShellCommand.mkdir_p absdir_external;

                (* Fetches the zip file: *)
                let abspath_zip =
                  make_abs_path
                    (Filename.concat (get_abs_path_string absdir_external) (Printf.sprintf "%s.zip" name))
                in
                let* () = fetch_external_zip ~wget_command ~url ~output:abspath_zip in

                (* Checks the fetched file by using checksum: *)
                let* () =
                  let checksum_got = Digest.file (get_abs_path_string abspath_zip) in
                  if checksum_got = checksum then
                    return ()
                  else
                    err @@ ExternalZipChecksumMismatch{
                      url;
                      path     = abspath_zip;
                      expected = checksum;
                      got      = checksum_got;
                    }
                in

                (* Extracts the zip file: *)
                let* () =
                  extract_external_zip
                    ~unzip_command ~zip:abspath_zip ~output_container_dir:absdir_external
                in

                (* Copies files: *)
                let* () =
                  extractions |> foldM (fun () extraction ->
                    let { extracted_from; extracted_to } = extraction in
                    let abspath_from =
                      make_abs_path
                        (Filename.concat (get_abs_path_string absdir_external) extracted_from)
                    in
                    let abspath_to =
                      make_abs_path
                        (Filename.concat (get_abs_path_string absdir_lock_root) extracted_to)
                    in
                    let ShellCommand.{ exit_status; command } =
                      ShellCommand.cp ~from:abspath_from ~to_:abspath_to
                    in
                    if exit_status = 0 then
                      return ()
                    else
                      err @@ FailedToCopyFile{ exit_status; command }
                  ) ()
                in

                return ()
          ) ()
        in

        return ()
  end
