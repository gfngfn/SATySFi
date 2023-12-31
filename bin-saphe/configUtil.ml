
open ConfigError
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


let dependency_spec_decoder : package_dependency_spec ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "registered" ==> begin
      get "registry" string >>= fun registry_local_name ->
      get "name" package_name_decoder >>= fun package_name ->
      get "requirement" requirement_decoder >>= fun version_requirement ->
      succeed @@ RegisteredDependency{
        registry_local_name;
        package_name;
        version_requirement;
      }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let dependency_decoder : package_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "used_as" string >>= fun used_as ->
  get "spec" dependency_spec_decoder >>= fun spec ->
  succeed @@ PackageDependency{ used_as; spec }


let registry_remote_decoder : registry_remote ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "git" ==> begin
      get "url" string >>= fun url ->
      get "branch" string >>= fun branch ->
      succeed @@ GitRegistry{ url; branch }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let name_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if s |> Core.String.to_list_rev |> List.exists (Char.equal '/') then
    failure (fun context -> CannotBeUsedAsAName(context, s))
  else
    succeed s
