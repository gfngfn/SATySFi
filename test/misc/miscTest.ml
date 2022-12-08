
let () =
  let open Alcotest in
  run "SATySFi Misc" [
    ("DependencyGraph", DependencyGraphTest.test_cases);
    ("PackageConstraintSolver", PackageConstraintSolverTest.test_cases);
  ]
