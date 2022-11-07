
open MyUtil
open ConfigError
open PackageSystemBase


type 'a ok = ('a, config_error) result


let ( !@ ) s =
  match SemanticVersion.parse s with
  | None         -> assert false
  | Some(semver) -> semver


let dependency package_name semver =
  PackageDependency{
    package_name;
    restrictions = [ CompatibleWith(semver) ];
  }


let load_cache () : package_context ok =
  let open ResultMonad in
  (* TODO: load this from a cache file *)
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
          requires = [
            dependency "stdlib" (!@ "0.0.1");
          ];
        };
      ]);
    ]
  in
  return { registry_contents }
