
open MyUtil
open CommonUtil
open ConfigError
open PackageConfigImpl
open PackageSystemBase
open EnvelopeSystemBase


module ConfigDecoder = YamlDecoder.Make(YamlError)

module RegistryLocalNameMap = Map.Make(String)


(* Package names must consist only of lowercased Latin letters, digits, and the hyphen in ASCII. *)
let package_name_decoder : package_name ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun package_name ->
  if
    package_name |> Core.String.for_all ~f:(fun char ->
      Char.equal char '-' || Core.Char.is_digit char || Core.Char.is_lowercase char
    )
  then
    succeed package_name
  else
    failure (fun context -> InvalidPackageName{ context; got = package_name })


let uppercased_identifier_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if is_uppercased_identifier s then
    succeed s
  else
    failure (fun context -> NotAnUppercasedIdentifier{ context; got = s })


let lowercased_identifier_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if is_lowercased_identifier s then
    succeed s
  else
    failure (fun context -> NotALowercasedIdentifier{ context; got = s })


(* Note: the empty string is allowed *)
let relative_path_decoder : relative_path ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if Filename.is_relative s then
    succeed s
  else
    failure (fun context -> NotARelativePath{ context; got = s })


(* Registry hash values must consist only of lowercased hex digits (i.e., 0-9 and a-f). *)
let registry_hash_value_decoder : registry_hash_value ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun registry_hash_value ->
  if registry_hash_value |> Core.String.for_all ~f:Core.Char.is_hex_digit then
    succeed registry_hash_value
  else
    failure (fun context -> InvalidRegistryHashValue{ got = registry_hash_value; context })


let version_decoder : SemanticVersion.t ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version ->
  match SemanticVersion.parse s_version with
  | None ->
      failure (fun context -> NotASemanticVersion{ context; got = s_version })

  | Some(semver) ->
      succeed semver


let requirement_decoder : SemanticVersion.requirement ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version_requirement ->
  match SemanticVersion.parse_requirement s_version_requirement with
  | None         -> failure (fun context -> NotAVersionRequirement{ context; got = s_version_requirement })
  | Some(verreq) -> succeed verreq


let version_checker (version : SemanticVersion.t) : unit ConfigDecoder.t =
  let open ConfigDecoder in
  requirement_decoder >>= fun requirement ->
  if version |> SemanticVersion.fulfill requirement then
    succeed ()
  else
    failure (fun context -> BreaksVersionRequirement{ context; requirement })


let registry_local_name_decoder =
  ConfigDecoder.string
    (* Registry local names can be arbitrary strings. *)


let dependency_spec_decoder : parsed_package_dependency_spec ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "registered" ==> begin
      get "registry" registry_local_name_decoder >>= fun registry_local_name ->
      get "name" package_name_decoder >>= fun package_name ->
      get "requirement" requirement_decoder >>= fun version_requirement ->
      succeed @@ ParsedRegisteredDependency{
        registry_local_name;
        package_name;
        version_requirement;
      }
    end;
    "local" ==> begin
      get "path" relative_path_decoder >>= fun relative_path ->
      succeed @@ ParsedLocalFixedDependency{ relative_path }
    end;
  ]


let dependency_decoder : parsed_package_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "used_as" uppercased_identifier_decoder >>= fun used_as ->
  dependency_spec_decoder >>= fun spec ->
  succeed @@ ParsedPackageDependency{ used_as; spec }


let registry_remote_decoder : registry_remote ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "git" ==> begin
      get "url" string >>= fun url ->
      get "branch" string >>= fun branch ->
      succeed @@ GitRegistry{ url; branch }
    end;
  ]


let registry_spec_decoder : (registry_local_name * registry_remote) ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" registry_local_name_decoder >>= fun registry_local_name ->
  registry_remote_decoder >>= fun registry_remote ->
  succeed (registry_local_name, registry_remote)


(* Decodes a string that can be used as a substring of a filename in file systems. *)
let writable_name_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  let flag =
    match Core.String.to_list s with
    | [] ->
        false

    | ch0 :: chs ->
        let pairs = (ch0, false) :: List.map (fun ch -> (ch, true)) chs in
        pairs |> List.for_all (fun (ch, is_middle) ->
          List.fold_left ( || ) false [
            Core.Char.is_lowercase ch;
            Core.Char.is_uppercase ch;
            Core.Char.is_digit ch;
            Char.equal ch '-';
            Char.equal ch '_';
            is_middle && Char.equal ch '.'; (* The first letter must not be a dot. *)
          ]
        )
  in
  if flag then
    succeed s
  else
    failure (fun context -> CannotBeUsedAsAName{ context; got = s })


(* TODO: make a type-level distinction between ordinary registry URLs and canonicalized ones *)
let make_registry_hash_value (registry_remote : registry_remote) : (registry_hash_value, config_error) result =
  let open ResultMonad in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      (* TODO: perhaps the canonicalization performed here is unnecessary *)
      let* canonicalized_url =
        CanonicalRegistryUrl.make url
          |> Result.map_error (fun e -> CanonicalRegistryUrlError(e))
      in
      let hash_value =
        Digest.to_hex (Digest.string (Printf.sprintf "git#%s#%s" canonicalized_url branch))
      in
      return hash_value


let lookup_registry_hash_value (registry_local_name : registry_local_name) (localmap : registry_remote RegistryLocalNameMap.t) =
  let open ResultMonad in
  match localmap |> RegistryLocalNameMap.find_opt registry_local_name with
  | None ->
      err @@ UndefinedRegistryLocalName{ registry_local_name }

  | Some(registry_remote) ->
      make_registry_hash_value registry_remote


let construct_registry_local_map (registry_specs : (registry_local_name * registry_remote) list) =
  let open ResultMonad in
  let* (localmap, registry_remote_acc) =
    registry_specs |> foldM (fun (localmap, registry_remote_acc) (registry_local_name, registry_remote) ->
      if localmap |> RegistryLocalNameMap.mem registry_local_name then
        err @@ DuplicateRegistryLocalName{ registry_local_name }
      else
        let localmap = localmap |> RegistryLocalNameMap.add registry_local_name registry_remote in
        let registry_remote_acc = Alist.extend registry_remote_acc registry_remote in
        return (localmap, registry_remote_acc)
    ) (RegistryLocalNameMap.empty, Alist.empty)
  in
  let registry_remotes = Alist.to_list registry_remote_acc in
  return (localmap, registry_remotes)
