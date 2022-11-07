
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
    ]
  in
  return { registry_contents }
