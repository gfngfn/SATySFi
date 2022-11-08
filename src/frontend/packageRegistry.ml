
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result


let implementation_decoder : implementation_record ConfigDecoder.t =
  let open ConfigDecoder in
  get "version" string >>= fun s_version ->
  get "dependencies" (list dependency_decoder) >>= fun dependencies ->
  match SemanticVersion.parse s_version with
  | None ->
      failure @@ (fun yctx -> NotASemanticVersion(yctx, s_version))

  | Some(semver) ->
      succeed { version = semver; requires = dependencies }


let package_decoder : (package_name * implementation_record list) ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun package_name ->
  get "implementations" (list implementation_decoder) >>= fun impls ->
  succeed (package_name, impls)


let registry_config_decoder : package_context ConfigDecoder.t =
  let open ConfigDecoder in
  get "packages" (list package_decoder) >>= fun packages ->
  packages |> List.fold_left (fun res (package_name, impls) ->
    res >>= fun map ->
    if map |> PackageNameMap.mem package_name then
      failure (fun yctx -> MultiplePackageDefinition{ context = yctx; package_name })
    else
      succeed (map |> PackageNameMap.add package_name impls)
  ) (succeed PackageNameMap.empty) >>= fun registry_contents ->
  succeed { registry_contents }



let ( !@ ) s =
  match SemanticVersion.parse s with
  | None         -> assert false
  | Some(semver) -> semver


let dependency package_name semver =
  PackageDependency{
    package_name;
    restrictions = [ CompatibleWith(semver) ];
  }


let load (abspath_registry_config : abs_path) : package_context ok =
  let open ResultMonad in
  (* TODO: load this from a cache file *)
  let* inc =
    try
      return (open_in_abs abspath_registry_config)
    with
    | Sys_error(_) -> err (RegistryConfigNotFound(abspath_registry_config))
  in
  let s = Core.In_channel.input_all inc in
  close_in inc;
  ConfigDecoder.run registry_config_decoder s
    |> Result.map_error (fun e -> RegistryConfigError(abspath_registry_config, e))
(*
  let registry_contents =
    List.fold_left (fun map (package_name, impls) ->
      map |> PackageNameMap.add package_name impls
    ) PackageNameMap.empty [
      ("stdlib", [
        {
          version = !@ "0.0.1";
          requires = [];
        };
      ]);
      ("math", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("code", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("annot", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("itemize", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("proof", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("tabular", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("footnote-scheme", [
        {
          version = !@ "0.0.1";
          requires = [ dependency "stdlib" (!@ "0.0.1") ];
        };
      ]);
      ("std-ja", [
        {
          version = !@ "0.0.1";
          requires = [
            dependency "stdlib" (!@ "0.0.1");
            dependency "math" (!@ "0.0.1");
            dependency "annot" (!@ "0.0.1");
            dependency "code" (!@ "0.0.1");
          ];
        };
      ]);
      ("std-ja-book", [
        {
          version = !@ "0.0.1";
          requires = [
            dependency "stdlib" (!@ "0.0.1");
            dependency "math" (!@ "0.0.1");
            dependency "annot" (!@ "0.0.1");
            dependency "code" (!@ "0.0.1");
            dependency "footnote-scheme" (!@ "0.0.1");
          ];
        };
      ]);
      ("std-ja-report", [
        {
          version = !@ "0.0.1";
          requires = [
            dependency "stdlib" (!@ "0.0.1");
            dependency "math" (!@ "0.0.1");
            dependency "annot" (!@ "0.0.1");
            dependency "code" (!@ "0.0.1");
            dependency "footnote-scheme" (!@ "0.0.1");
          ];
        };
      ]);
    ]
  in
  return { registry_contents }
*)
