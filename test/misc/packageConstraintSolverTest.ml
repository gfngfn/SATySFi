
open Main__PackageSystemBase
module SemanticVersion = Main__SemanticVersion
module Constant = Main__Constant
module PackageConstraintSolver = Main__PackageConstraintSolver


let make_version (s_version : string) : SemanticVersion.t =
  match SemanticVersion.parse s_version with
  | Some(semver) -> semver
  | None         -> assert false


let make_dependency (package_name : package_name) (s_version : string) : package_dependency =
  PackageDependency{
    package_name;
    registry_local_name = "default";
    version_requirement = SemanticVersion.CompatibleWith(make_version s_version);
  }


let make_dependency_in_registry (package_name : package_name) (s_version : string) : package_dependency_in_registry =
  PackageDependencyInRegistry{
    package_name;
    version_requirement = SemanticVersion.CompatibleWith(make_version s_version);
  }


let make_impl (s_version : string) (deps : package_dependency_in_registry list) : implementation_record =
  ImplRecord{
    version              = make_version s_version;
    source               = NoSource;
    language_requirement = SemanticVersion.CompatibleWith(Constant.current_language_version);
    dependencies         = deps;
  }


let make_lock (package_name : package_name) (s_version : string) : Lock.t =
  Lock.{
    package_name;
    locked_version = make_version s_version;
  }


let make_solution ?(test_only : bool = false) (package_name : package_name) (s_version : string) (deps : Lock.t list) : package_solution =
  {
    lock = make_lock package_name s_version;
    locked_source = NoSource;
    locked_dependencies = deps;
    used_in_test_only = test_only;
  }


let check package_context dependencies_with_flags expected =
  let got = PackageConstraintSolver.solve package_context dependencies_with_flags in
  Alcotest.(check (option (list (of_pp pp_package_solution)))) "solutions" expected got


let solve_test_1 () =
  let package_context =
    let packages =
      PackageNameMap.of_seq @@ List.to_seq [
        ("foo", [
          make_impl "1.0.0" [];
          make_impl "2.0.0" [];
        ]);
        ("bar", [
          make_impl "1.0.0" [ make_dependency_in_registry "foo" "2.0.0" ];
        ]);
        ("qux", [
          make_impl "1.0.0" [ make_dependency_in_registry "foo" "1.0.0" ];
        ]);
      ]
    in
    { registries = RegistryLocalNameMap.singleton "default" packages }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_dependency "bar" "1.0.0");
      (SourceDependency, make_dependency "qux" "1.0.0");
    ]
  in
  let expected =
    Some([
      make_solution "bar" "1.0.0" [
        make_lock "foo" "2.0.0";
      ];
      make_solution "foo" "1.0.0" [];
      make_solution "foo" "2.0.0" [];
      make_solution "qux" "1.0.0" [
        make_lock "foo" "1.0.0";
      ];
    ])
  in
  check package_context dependencies_with_flags expected


let solve_test_2 () =
  let package_context =
    let packages =
      PackageNameMap.of_seq @@ List.to_seq [
        ("foo", [
          make_impl "1.0.0" [];
          make_impl "1.1.0" [];
        ]);
        ("bar", [
          make_impl "1.0.0" [ make_dependency_in_registry "foo" "1.1.0" ];
        ]);
        ("qux", [
          make_impl "1.0.0" [ make_dependency_in_registry "foo" "1.0.0" ];
        ]);
      ]
    in
    { registries = RegistryLocalNameMap.singleton "default" packages }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_dependency "bar" "1.0.0");
      (SourceDependency, make_dependency "qux" "1.0.0");
    ]
  in
  let expected =
    Some([
      make_solution "bar" "1.0.0" [ make_lock "foo" "1.1.0" ];
      make_solution "foo" "1.1.0" [];
      make_solution "qux" "1.0.0" [ make_lock "foo" "1.1.0" ];
    ])
  in
  check package_context dependencies_with_flags expected


let test_cases =
  Alcotest.[
    test_case "solve 1" `Quick solve_test_1;
    test_case "solve 2" `Quick solve_test_2;
  ]
