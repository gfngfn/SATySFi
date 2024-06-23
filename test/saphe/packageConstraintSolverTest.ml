
open SapheTestUtil
open SapheMain__PackageSystemBase
module Constant = SapheMain__Constant
module PackageConstraintSolver = SapheMain__PackageConstraintSolver


let default_registry_hash_value =
  "c0bebeef4423"


let ecosystem_version =
  make_version "0.1.0"


let language_version =
  make_version "0.1.0"


let make_registered_dependency ~(used_as : string) ?(registry_hash_value : registry_hash_value = default_registry_hash_value) (package_name : package_name) (s_version : string) : package_dependency =
  PackageDependency{
    used_as;
    spec =
      RegisteredDependency{
        registered_package_id = RegisteredPackageId.{ package_name; registry_hash_value };
        version_requirement   = SemanticVersion.CompatibleWith(make_version s_version);
      };
  }


let make_local_fixed_dependency ~(used_as : string) (abspathstr : string) : package_dependency =
  PackageDependency{
    used_as;
    spec = LocalFixedDependency{ absolute_path = AbsPath.of_string_exn abspathstr };
  }


let make_dependency_in_registry ~(used_as : string) ?(external_registry_hash_value : registry_hash_value option) (package_name : package_name) (s_version : string) : package_dependency_in_registry =
  PackageDependencyInRegistry{
    used_as;
    external_registry_hash_value;
    package_name;
    version_requirement = SemanticVersion.CompatibleWith(make_version s_version);
  }


let make_impl (s_version : string) ?(registry_remotes : registry_remote list = []) (deps : package_dependency_in_registry list) : implementation_record =
  ImplRecord{
    language_requirement  = SemanticVersion.CompatibleWith(language_version);
    package_version       = make_version s_version;
    source                = NoSource;
    registry_remotes      = registry_remotes;
    dependencies          = deps;
  }


let make_registered_lock ?(registry_hash_value : registry_hash_value = default_registry_hash_value) (package_name : package_name) (s_version : string) : RegisteredLock.t =
  RegisteredLock.{
    registered_package_id = RegisteredPackageId.{ package_name; registry_hash_value };
    locked_version        = make_version s_version;
  }


let make_locked_dependency ~(used_as : string) ?(registry_hash_value : registry_hash_value = default_registry_hash_value) (package_name : package_name) (s_version : string) : locked_dependency =
  {
    depended_lock      = Lock.Registered(make_registered_lock ~registry_hash_value package_name s_version);
    dependency_used_as = used_as;
  }


let make_registered_solution ?(explicitly_depended : string option) ?(test_only : bool = false) ?(registry_hash_value : registry_hash_value = default_registry_hash_value) (package_name : package_name) (s_version : string) (deps : locked_dependency list) : package_solution =
  {
    lock                = Lock.Registered(make_registered_lock ~registry_hash_value package_name s_version);
    locked_source       = NoSource;
    locked_dependencies = deps;
    used_in_test_only   = test_only;
    explicitly_depended;
    explicitly_test_depended = None;
  }


let make_local_fixed_solution ?(explicitly_depended : string option) ?(test_only : bool = false) (abspathstr : string) (deps : locked_dependency list) : package_solution =
  {
    lock                = Lock.LocalFixed{ absolute_path = AbsPath.of_string_exn abspathstr };
    locked_source       = NoSource;
    locked_dependencies = deps;
    used_in_test_only   = test_only;
    explicitly_depended;
    explicitly_test_depended = None;
  }


let check package_context dependencies_with_flags expected =
  let got = PackageConstraintSolver.solve package_context dependencies_with_flags in
  Alcotest.(check (option (list (of_pp pp_package_solution)))) "solutions" expected got


type entry = registry_hash_value * package_name * implementation_record list


let make_registered_package_impls : entry list -> (implementation_record list) RegisteredPackageIdMap.t =
  List.fold_left (fun map (registry_hash_value, package_name, impls) ->
    let registered_package_id = RegisteredPackageId.{ package_name; registry_hash_value } in
    map |> RegisteredPackageIdMap.add registered_package_id impls
  ) RegisteredPackageIdMap.empty


let make_entry ?(registry_hash_value : registry_hash_value = default_registry_hash_value) (package_name : package_name) (impls : implementation_record list) : entry =
  (registry_hash_value, package_name, impls)


let make_local_fixed_dependencies =
  List.fold_left (fun lfpkgmap (abspathstr, deps) ->
    LocalFixedPackageIdMap.add (AbsPath.of_string_exn abspathstr) deps lfpkgmap
  ) LocalFixedPackageIdMap.empty


let solve_test_1 () =
  let package_context =
    let local_fixed_dependencies = make_local_fixed_dependencies [] in
    let registered_package_impls =
      make_registered_package_impls [
        make_entry "foo" [
          make_impl "1.0.0" [];
          make_impl "2.0.0" [];
        ];
        make_entry "bar" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"Foo" "foo" "2.0.0" ];
        ];
        make_entry "qux" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"Foo" "foo" "1.0.0" ];
        ];
      ]
    in
    { language_version; local_fixed_dependencies; registered_package_impls }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_registered_dependency ~used_as:"Bar" "bar" "1.0.0");
      (SourceDependency, make_registered_dependency ~used_as:"Qux" "qux" "1.0.0");
    ]
  in
  let expected =
    Some([
      make_registered_solution ~explicitly_depended:"Bar"
        "bar" "1.0.0" [ make_locked_dependency ~used_as:"Foo" "foo" "2.0.0" ];
      make_registered_solution
        "foo" "1.0.0" [];
      make_registered_solution
        "foo" "2.0.0" [];
      make_registered_solution ~explicitly_depended:"Qux"
        "qux" "1.0.0" [ make_locked_dependency ~used_as:"Foo" "foo" "1.0.0" ];
    ])
  in
  check package_context dependencies_with_flags expected


let solve_test_2 () =
  let package_context =
    let local_fixed_dependencies = make_local_fixed_dependencies [] in
    let registered_package_impls =
      make_registered_package_impls [
        make_entry "foo" [
          make_impl "1.0.0" [];
          make_impl "1.1.0" [];
        ];
        make_entry "bar" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"FooA" "foo" "1.1.0" ];
        ];
        make_entry "qux" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"FooB" "foo" "1.0.0" ];
        ];
      ]
    in
    { language_version; local_fixed_dependencies; registered_package_impls }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_registered_dependency ~used_as:"Bar" "bar" "1.0.0");
      (SourceDependency, make_registered_dependency ~used_as:"Qux" "qux" "1.0.0");
    ]
  in
  let expected =
    Some([
      make_registered_solution ~explicitly_depended:"Bar"
        "bar" "1.0.0" [ make_locked_dependency ~used_as:"FooA" "foo" "1.1.0" ];
      make_registered_solution
        "foo" "1.1.0" [];
      make_registered_solution ~explicitly_depended:"Qux"
        "qux" "1.0.0" [ make_locked_dependency ~used_as:"FooB" "foo" "1.1.0" ];
    ])
  in
  check package_context dependencies_with_flags expected


let solve_test_3 () =
  let package_context =
    let local_fixed_dependencies =
      make_local_fixed_dependencies [
        ("/home/john/path/to/sub/", [
          make_registered_dependency ~used_as:"FooB" "foo" "1.0.0";
        ]);
      ]
    in
    let registered_package_impls =
      make_registered_package_impls [
        make_entry "foo" [
          make_impl "1.0.0" [];
        ];
        make_entry "bar" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"FooA" "foo" "1.0.0" ];
        ];
      ]
    in
    { language_version; local_fixed_dependencies; registered_package_impls }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_registered_dependency ~used_as:"Bar" "bar" "1.0.0");
      (SourceDependency, make_local_fixed_dependency ~used_as:"Sub" "/home/john/path/to/sub/");
    ]
  in
  let expected =
    Some([
      make_registered_solution ~explicitly_depended:"Bar"
        "bar" "1.0.0" [ make_locked_dependency ~used_as:"FooA" "foo" "1.0.0" ];
      make_registered_solution
        "foo" "1.0.0" [];
      make_local_fixed_solution ~explicitly_depended:"Sub"
        "/home/john/path/to/sub/" [ make_locked_dependency ~used_as:"FooB" "foo" "1.0.0" ];
    ])
  in
  check package_context dependencies_with_flags expected


let solve_test_4 () =
  let external_registry_hash_value = "effec4f01023" in
  let package_context =
    let local_fixed_dependencies = make_local_fixed_dependencies [] in
    let registered_package_impls =
      make_registered_package_impls [
        make_entry ~registry_hash_value:external_registry_hash_value "foo" [
          make_impl "1.0.0" [];
          make_impl "2.0.0" [];
        ];
        make_entry "bar" [
          make_impl "1.0.0"
            ~registry_remotes:[ GitRegistry{ url = "example.com"; branch = "master" } ] (* Dummy *)
            [
               make_dependency_in_registry
                 ~used_as:"Foo" ~external_registry_hash_value "foo" "2.0.0"
            ];
        ];
        make_entry "qux" [
          make_impl "1.0.0" [ make_dependency_in_registry ~used_as:"Bar" "bar" "1.0.0" ];
        ];
      ]
    in
    { language_version; local_fixed_dependencies; registered_package_impls }
  in
  let dependencies_with_flags =
    [
      (SourceDependency, make_registered_dependency ~used_as:"Bar" "bar" "1.0.0");
      (SourceDependency, make_registered_dependency ~used_as:"Qux" "qux" "1.0.0");
    ]
  in
  let expected =
    Some([
      make_registered_solution ~explicitly_depended:"Bar"
        "bar" "1.0.0" [
          make_locked_dependency
            ~used_as:"Foo" ~registry_hash_value:external_registry_hash_value "foo" "2.0.0";
        ];
      make_registered_solution ~explicitly_depended:"Qux"
        "qux" "1.0.0" [
          make_locked_dependency
            ~used_as:"Bar" "bar" "1.0.0"
        ];
      make_registered_solution
        ~registry_hash_value:external_registry_hash_value
        "foo" "2.0.0" [];
    ])
  in
  check package_context dependencies_with_flags expected


let test_cases =
  Alcotest.[
    test_case "solve 1 (basic)" `Quick solve_test_1;
    test_case "solve 2 (different versions of a common package)" `Quick solve_test_2;
    test_case "solve 3 (local fixed dependencies)" `Quick solve_test_3;
    test_case "solve 4 (external registries)" `Quick solve_test_4;
  ]
