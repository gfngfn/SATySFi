
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


let fetch_registered_lock ~(wget_command : string) ~(tar_command : string) ~store_root:(absdir_store_root : abs_path) (reglock : RegisteredLock.t) (source : implementation_source) : unit ok =
  let open ResultMonad in
  let RegisteredLock.{ registered_package_id; locked_version } = reglock in
  let RegisteredPackageId.{ registry_hash_value; package_name } = registered_package_id in
  let lock_tarball_name = Constant.lock_tarball_name package_name locked_version in
  let absdir_lock = Constant.registered_lock_directory ~store_root:absdir_store_root reglock in
  let absdir_lock_tarball_cache =
    Constant.lock_tarball_cache_directory ~store_root:absdir_store_root registry_hash_value
  in

  (* Creates the lock cache directory if non-existent, or does nothing otherwise: *)
  ShellCommand.mkdir_p absdir_lock_tarball_cache;

  (* Creates the directory if non-existent, or does nothing otherwise: *)
  ShellCommand.mkdir_p absdir_lock;

  let abspath_config = Constant.library_package_config_path ~dir:absdir_lock in
  if AbsPathIo.file_exists abspath_config then begin
  (* If the lock has already been fetched: *)
    Logging.lock_already_installed lock_tarball_name absdir_lock;
    return ()
  end else
    match source with
    | NoSource ->
        return ()

    | TarGzip{ url; checksum } ->
        (* Synchronously fetches a tarball (if non-existent): *)
        let abspath_tarball =
          append_to_abs_directory absdir_lock_tarball_cache
            (Printf.sprintf "%s.tar.gz" lock_tarball_name)
        in
        let* () =
          if AbsPathIo.file_exists abspath_tarball then begin
            Logging.lock_cache_exists lock_tarball_name abspath_tarball;
            return ()
          end else begin
            Logging.downloading_lock lock_tarball_name abspath_tarball;
            fetch_tarball ~wget_command ~lock_name:lock_tarball_name ~url ~output:abspath_tarball
          end
        in

        (* Checks the fetched tarball by using the checksum: *)
        let* () =
          let checksum_got = Digest.to_hex (Digest.file (get_abs_path_string abspath_tarball)) in
          if String.equal checksum_got checksum then
            return ()
          else
            err @@ TarGzipChecksumMismatch{
              lock_name = lock_tarball_name;
              url;
              path     = abspath_tarball;
              expected = checksum;
              got      = checksum_got;
            }
        in

        (* Extracts sources from the tarball: *)
        let* () =
          extract_tarball_with_components_stripped
            ~tar_command ~lock_name:lock_tarball_name ~tarball:abspath_tarball ~lock_root:absdir_lock
        in

        return ()


let fetch_external_zip_if_nonexistent ~(wget_command : string) (abspath_zip : abs_path) ~(url : string) ~checksum:(checksum_expected : string) =
  let open ResultMonad in
  if AbsPathIo.file_exists abspath_zip then
    (* Does nothing if the required zip file has already been fetched: *)
    return ()
  else begin
    (* Creates a directory for putting zips: *)
    ShellCommand.mkdir_p (dirname abspath_zip);

    (* Fetches the zip file: *)
    let* () = fetch_external_zip ~wget_command ~url ~output:abspath_zip in

    (* Checks the fetched file by using checksum: *)
    let checksum_got = Digest.to_hex (Digest.file (get_abs_path_string abspath_zip)) in
    if String.equal checksum_got checksum_expected then
      return ()
    else
      err @@ ExternalZipChecksumMismatch{
        url;
        path     = abspath_zip;
        expected = checksum_expected;
        got      = checksum_got;
      }
  end


type extraction_abs_path_pair = {
  abspath_from : abs_path;
  abspath_to   : abs_path;
}


let extract_external_zip_and_arrange_files_if_necessary ~(unzip_command : string) ~zip_path:(abspath_zip : abs_path) ~extraction_dir:(absdir_extraction : abs_path) ~destination_lock_dir:(absdir_lock : abs_path) (extractions : extraction list) =
  let open ResultMonad in

  let pairs =
    extractions |> List.map (fun extraction ->
      let { extracted_from; extracted_to } = extraction in
      let abspath_from = append_to_abs_directory absdir_extraction extracted_from in
      let abspath_to = append_to_abs_directory absdir_lock extracted_to in
      { abspath_from; abspath_to }
    )
  in

  if pairs |> List.for_all (fun { abspath_to; _ } -> AbsPathIo.file_exists abspath_to) then
    return ()
  else begin

    (* Creates a directory for putting extracted files: *)
    ShellCommand.mkdir_p absdir_extraction;

    (* Extracts the zip file: *)
    let* () =
      extract_external_zip
        ~unzip_command ~zip:abspath_zip ~output_container_dir:absdir_extraction
    in

    (* Copies files: *)
    pairs |> foldM (fun () { abspath_from; abspath_to } ->
      let ShellCommand.{ exit_status; command } =
        ShellCommand.cp ~from:abspath_from ~to_:abspath_to
      in
      if exit_status = 0 then
        return ()
      else
        err @@ FailedToCopyFile{ exit_status; command }
    ) ()
  end


let fetch_external_resources ~(wget_command : string) ~(unzip_command : string) ~store_root:(absdir_store_root : abs_path) (reglock : RegisteredLock.t) =
  let open ResultMonad in
  let RegisteredLock.{ registered_package_id; locked_version } = reglock in
  let RegisteredPackageId.{ registry_hash_value; package_name } = registered_package_id in
  let lock_tarball_name = Constant.lock_tarball_name package_name locked_version in
  let absdir_lock = Constant.registered_lock_directory ~store_root:absdir_store_root reglock in

  (* Loads the package config: *)
  let* PackageConfig.{ external_resources; _ } =
    PackageConfig.load (Constant.library_package_config_path ~dir:absdir_lock)
  in
  let* () =
    external_resources |> foldM (fun () (name, external_resource) ->
      match external_resource with
      | ExternalZip{ url; checksum; extractions } ->
          let absdir_external =
            append_to_abs_directory
              (Constant.external_resource_cache_directory ~store_root:absdir_store_root registry_hash_value)
              lock_tarball_name
          in

          (* Fetches the external zip file if nonexistent: *)
          let abspath_zip =
            append_to_abs_directory absdir_external (Printf.sprintf "archives/%s.zip" name)
          in
          let* () =
            fetch_external_zip_if_nonexistent
              ~wget_command
              abspath_zip
              ~url
              ~checksum
          in

          (* Extracts the external zip file if some of the resulting files are nonexistent: *)
          let absdir_extraction =
            append_to_abs_directory absdir_external (Printf.sprintf "extractions/%s" name)
          in
          let* () =
            extract_external_zip_and_arrange_files_if_necessary
              ~unzip_command
              ~zip_path:abspath_zip
              ~extraction_dir:absdir_extraction
              ~destination_lock_dir:absdir_lock
              extractions
          in

          return ()
    ) ()
  in

  return ()


let main ~(wget_command : string) ~(tar_command : string) ~(unzip_command : string) ~(store_root : abs_path) (impl_spec : implementation_spec) : unit ok =
  let open ResultMonad in
  let ImplSpec{ lock; source } = impl_spec in
  match lock with
  | Lock.Registered(reglock) ->
      let* () =
        fetch_registered_lock
          ~wget_command
          ~tar_command
          ~store_root
          reglock
          source
      in
      let* () =
        fetch_external_resources
          ~wget_command
          ~unzip_command
          ~store_root
          reglock
      in
      return ()

  | Lock.LocalFixed(_) ->
      (* TODO: Use `fetch_external_resources` here *)
      return ()
