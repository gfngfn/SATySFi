
let () =
  let open Alcotest in
  run "Saphe Test" [
    ("PackageConfig", PackageConfigTest.test_cases);
    ("PackageReleaseConfig", PackageReleaseConfigTest.test_cases);
    ("PackageConstraintSolver", PackageConstraintSolverTest.test_cases);
  ]
