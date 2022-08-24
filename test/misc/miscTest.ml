
let () =
  let open Alcotest in
  run "SATySFi Misc" [
    ("DependencyGraph", [
      test_case "IntDependencyGraph" `Quick DependencyGraphTest.tests;
    ]);
  ]
