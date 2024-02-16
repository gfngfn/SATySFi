
open ConfigError
open PackageConfigImpl
open PackageSystemBase


module ConfigDecoder = YamlDecoder.Make(YamlError)


let package_name_decoder : package_name ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun package_name ->
  let chars = Core.String.to_list_rev package_name in
  if
    chars |> List.for_all (fun char ->
      Char.equal char '-' || Core.Char.is_digit char || Core.Char.is_lowercase char
    )
  then
    succeed package_name
  else
    failure (fun yctx -> InvalidPackageName(yctx, package_name))


let version_decoder : SemanticVersion.t ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version ->
  match SemanticVersion.parse s_version with
  | None ->
      failure (fun yctx -> NotASemanticVersion(yctx, s_version))

  | Some(semver) ->
      succeed semver


let requirement_decoder : SemanticVersion.requirement ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version_requirement ->
  match SemanticVersion.parse_requirement s_version_requirement with
  | None         -> failure (fun context -> NotAVersionRequirement(context, s_version_requirement))
  | Some(verreq) -> succeed verreq


let version_checker (version : SemanticVersion.t) : unit ConfigDecoder.t =
  let open ConfigDecoder in
  requirement_decoder >>= fun requirement ->
  if version |> SemanticVersion.fulfill requirement then
    succeed ()
  else
    failure (fun yctx -> BreaksVersionRequirement(yctx, requirement))


let dependency_spec_decoder : parsed_package_dependency_spec ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "registered" ==> begin
      get "registry" string >>= fun registry_local_name ->
      get "name" package_name_decoder >>= fun package_name ->
      get "requirement" requirement_decoder >>= fun version_requirement ->
      succeed @@ ParsedRegisteredDependency{
        registry_local_name;
        package_name;
        version_requirement;
      }
    end;
    "local" ==> begin
      get "path" string >>= fun relative_path ->
      succeed @@ ParsedLocalFixedDependency{ relative_path }
    end;
  ]


let dependency_decoder : parsed_package_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "used_as" string >>= fun used_as ->
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


let name_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if s |> Core.String.to_list_rev |> List.exists (Char.equal '/') then
    failure (fun context -> CannotBeUsedAsAName(context, s))
  else
    succeed s


let make_registry_hash_value (registry_remote : registry_remote) : (registry_hash_value, config_error) result =
  let open ResultMonad in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      let* canonicalized_url =
        CanonicalRegistryUrl.make url
          |> Result.map_error (fun e -> CanonicalRegistryUrlError(e))
      in
      let hash_value =
        Digest.to_hex (Digest.string (Printf.sprintf "git#%s#%s" canonicalized_url branch))
      in
(*
      Logging.report_canonicalized_url ~url ~canonicalized_url ~hash_value;
*)
      return hash_value
